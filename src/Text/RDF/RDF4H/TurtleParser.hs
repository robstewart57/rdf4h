-- |An 'RdfParser' implementation for the Turtle format 
-- <http://www.w3.org/TeamSubmission/turtle/>.

module Text.RDF.RDF4H.TurtleParser(
  TurtleParser(TurtleParser)
)

where

import Data.Char (toLower,toUpper)
import Data.RDF.Types
import Data.RDF.Namespace
import Text.RDF.RDF4H.ParserUtils
import Text.Parsec
import Text.Parsec.Text.Lazy
import qualified Data.Map as Map
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Sequence(Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Char (isDigit)
import Control.Monad
import Data.Maybe (fromMaybe)

-- |An 'RdfParser' implementation for parsing RDF in the 
-- Turtle format. It is an implementation of W3C Turtle grammar rules at
-- http://www.w3.org/TR/turtle/#sec-grammar-grammar .
-- It takes optional arguments representing the base URL to use
-- for resolving relative URLs in the document (may be overridden in the document
-- itself using the \@base directive), and the URL to use for the document itself
-- for resolving references to <> in the document.
-- To use this parser, pass a 'TurtleParser' value as the first argument to any of
-- the 'parseString', 'parseFile', or 'parseURL' methods of the 'RdfParser' type
-- class.
data TurtleParser = TurtleParser (Maybe BaseUrl) (Maybe T.Text)

-- |'TurtleParser' is an instance of 'RdfParser'.
instance RdfParser TurtleParser where
  parseString (TurtleParser bUrl dUrl)  = parseString' bUrl dUrl 
  parseFile   (TurtleParser bUrl dUrl)  = parseFile' bUrl dUrl
  parseURL    (TurtleParser bUrl dUrl)  = parseURL'  bUrl dUrl

type ParseState =
  (Maybe BaseUrl,    -- the current BaseUrl, may be Nothing initially, but not after it is once set
   Maybe T.Text,     -- the docUrl, which never changes and is used to resolve <> in the document.
   Int,              -- the id counter, containing the value of the next id to be used
   PrefixMappings,   -- the mappings from prefix to URI that are encountered while parsing
   [Subject],        -- stack of current subject nodes, if we have parsed a subject but not finished the triple
   [Predicate],      -- stack of current predicate nodes, if we've parsed a predicate but not finished the triple
   [Bool],           -- a stack of values to indicate that we're processing a (possibly nested) collection; top True indicates just started (on first element)
   Seq Triple)       -- the triples encountered while parsing; always added to on the right side

-- grammar rule: [1] turtleDoc
t_turtleDoc :: GenParser ParseState (Seq Triple, PrefixMappings)
t_turtleDoc =
  many t_statement >> (eof <?> "eof") >> getState >>= \(_, _, _, pms, _, _, _, ts) -> return (ts, pms)

-- grammar rule: [2] statement
t_statement :: GenParser ParseState ()
t_statement = d <|> t <|> void (many1 t_ws <?> "blankline-whitespace")
  where
    d = void
      (try t_directive >>
      (many t_ws <?> "directive-whitespace2"))
    t = void
      (t_triples >> (many t_ws <?> "triple-whitespace1") >>
      (char '.' <?> "end-of-triple-period") >>
      (many t_ws <?> "triple-whitespace2"))

-- grammar rule: [6] triples
t_triples :: GenParser ParseState ()
t_triples = t_subject >> (many1 t_ws <?> "subject-predicate-whitespace") >> t_predicateObjectList >> resetSubjectPredicate

-- grammar rule: [3] directive
t_directive :: GenParser ParseState ()
t_directive = t_prefixID <|> t_base <|> t_sparql_prefix <|> t_sparql_base

-- grammar rule: [135s] iri
t_iri :: GenParser ParseState T.Text
t_iri =  try t_iriref <|> t_prefixedName

-- grammar rule: [136s] PrefixedName
t_prefixedName :: GenParser ParseState T.Text
t_prefixedName = do
  t <- try t_pname_ln <|> try t_pname_ns
  return t

-- grammar rule: [4] prefixID
t_prefixID :: GenParser ParseState ()
t_prefixID =
  do try (string "@prefix" <?> "@prefix-directive")
     pre <- (many1 t_ws <?> "whitespace-after-@prefix") >> option T.empty t_pn_prefix
     char ':' >> (many1 t_ws <?> "whitespace-after-@prefix-colon")
     uriFrag <- t_iriref
     (many t_ws <?> "prefixID-whitespace")
     (char '.' <?> "end-of-prefixID-period")
     (bUrl, dUrl, _, PrefixMappings pms, _, _, _, _) <- getState
     updatePMs $ Just (PrefixMappings $ Map.insert pre (absolutizeUrl bUrl dUrl uriFrag) pms)
     return ()

-- grammar rule: [6s] sparqlPrefix
t_sparql_prefix :: GenParser ParseState ()
t_sparql_prefix =
  do try (caseInsensitiveString "PREFIX" <?> "@prefix-directive")
     pre <- (many1 t_ws <?> "whitespace-after-@prefix") >> option T.empty t_pn_prefix
     char ':' >> (many1 t_ws <?> "whitespace-after-@prefix-colon")
     uriFrag <- t_iriref
     (bUrl, dUrl, _, PrefixMappings pms, _, _, _, _) <- getState
     updatePMs $ Just (PrefixMappings $ Map.insert pre (absolutizeUrl bUrl dUrl uriFrag) pms)
     return ()

-- grammar rule: [5] base
t_base :: GenParser ParseState ()
t_base =
  do try (string "@base" <?> "@base-directive")
     many1 t_ws <?> "whitespace-after-@base"
     urlFrag <- t_iriref
     (many t_ws <?> "base-whitespace")
     (char '.' <?> "end-of-base-period")
     bUrl <- currBaseUrl
     dUrl <- currDocUrl
     updateBaseUrl (Just $ Just $ newBaseUrl bUrl (absolutizeUrl bUrl dUrl urlFrag))

-- grammar rule: [5s] sparqlBase
t_sparql_base :: GenParser ParseState ()
t_sparql_base =
  do try (caseInsensitiveString "BASE" <?> "@sparql-base-directive")
     many1 t_ws <?> "whitespace-after-BASE"
     urlFrag <- t_iriref
     bUrl <- currBaseUrl
     dUrl <- currDocUrl
     updateBaseUrl (Just $ Just $ newBaseUrl bUrl (absolutizeUrl bUrl dUrl urlFrag))

t_verb :: GenParser ParseState ()
t_verb = (try t_predicate <|> (char 'a' >> return rdfTypeNode)) >>= pushPred

-- grammar rule: [11] predicate
t_predicate :: GenParser ParseState Node
t_predicate = liftM UNode (t_iri <?> "resource")

t_nodeID  :: GenParser ParseState T.Text
t_nodeID = do { try (string "_:"); cs <- t_name; return $! "_:" `T.append` cs }

-- grammar rules: [139s] PNAME_NS
t_pname_ns :: GenParser ParseState T.Text
t_pname_ns =do
  pre <- option T.empty (try t_pn_prefix)
  char ':'
  return pre

-- grammar rules: [168s] PN_LOCAL
t_pn_local :: GenParser ParseState T.Text
t_pn_local = do
  x <- t_pn_chars_u_str <|> string ":" <|> satisfy_str <|> t_plx
  xs <- option "" $ do
               ys <- many ( t_pn_chars_str <|> string "." <|> string ":" <|> t_plx )
               return (concat ys)
  return (T.pack (x ++ xs))
    where
      satisfy_str = satisfy (flip in_range [('0', '9')]) >>= \c -> return [c]
      t_pn_chars_str = t_pn_chars >>= \c -> return [c]
      t_pn_chars_u_str = t_pn_chars_u >>= \c -> return [c]

-- PERCENT | PN_LOCAL_ESC
-- grammar rules: [169s] PLX
t_plx = t_percent <|> t_pn_local_esc_str
    where
      t_pn_local_esc_str = do
        c <- t_pn_local_esc
        return ([c])

--        '%' HEX HEX
-- grammar rules: [170s] PERCENT
t_percent = do
  char '%'
  h1 <- t_hex
  h2 <- t_hex
  return ([h1,h2])

-- grammar rules: [172s] PN_LOCAL_ESC
t_pn_local_esc = char '\\' >> oneOf "_~.-!$&'()*+,;=/?#@%"

-- grammar rules: [140s] PNAME_LN
t_pname_ln :: GenParser ParseState T.Text
t_pname_ln =
  do pre <- t_pname_ns
     name <- t_pn_local
     (bUrl, _, _, pms, _, _, _, _) <- getState
     case resolveQName bUrl pre pms of
       Just n -> return $ n `T.append` name
       Nothing -> error ("Cannot resolve QName prefix: " ++ T.unpack pre)

-- grammar rule: [10] subject
t_subject :: GenParser ParseState ()
t_subject =
  iri <|>
  simpleBNode <|>
  nodeId <|>
  between (char '[') (char ']') poList
  where
    iri         = liftM UNode (try t_iri <?> "subject resource") >>= pushSubj
    nodeId      = liftM BNode (try t_nodeID <?> "subject nodeID") >>= pushSubj
    simpleBNode = try (string "[]") >> nextIdCounter >>=  pushSubj . BNodeGen
    poList      = void
                (nextIdCounter >>= pushSubj . BNodeGen >> many t_ws >>
                t_predicateObjectList >>
                many t_ws)

-- verb objectList (';' (verb objectList)?)*
--
-- verb ws+ objectList ( ws* ';' ws* verb ws+ objectList )* (ws* ';')?
-- grammar rule: [7] predicateObjectlist
t_predicateObjectList :: GenParser ParseState ()
t_predicateObjectList =
  do sepEndBy1 (try (t_verb >> many1 t_ws >> t_objectList >> popPred)) (try (many t_ws >> char ';' >> many t_ws))
     return ()

-- grammar rule: [8] objectlist
t_objectList :: GenParser ParseState ()
t_objectList = -- t_object actually adds the triples
  void
  ((t_object <?> "object") >>
  many (try (many t_ws >> char ',' >> many t_ws >> t_object)))

-- grammar rule: [12] object
t_object :: GenParser ParseState ()
t_object =
  do inColl      <- isInColl          -- whether this object is in a collection
     onFirstItem <- onCollFirstItem   -- whether we're on the first item of the collection
     let processObject = (t_literal >>= addTripleForObject) <|>
                          (liftM UNode t_iri >>= addTripleForObject) <|>
                          blank_as_obj <|> t_collection
     case (inColl, onFirstItem) of
       (False, _)    -> processObject
       (True, True)  -> liftM BNodeGen nextIdCounter >>= \bSubj -> addTripleForObject bSubj >>
                          pushSubj bSubj >> pushPred rdfFirstNode >> processObject >> collFirstItemProcessed
       (True, False) -> liftM BNodeGen nextIdCounter >>= \bSubj -> pushPred rdfRestNode >>
                          addTripleForObject bSubj >> popPred >> popSubj >>
                          pushSubj bSubj >> processObject

-- collection: '(' ws* itemList? ws* ')'
-- itemList:      object (ws+ object)*
-- grammar rule: [15] collection
t_collection:: GenParser ParseState ()
t_collection = 
  -- ( object1 object2 ) is short for:
  -- [ rdf:first object1; rdf:rest [ rdf:first object2; rdf:rest rdf:nil ] ]
  -- ( ) is short for the resource:  rdf:nil
  between (char '(') (char ')') $
    do beginColl
       many t_ws
       emptyColl <- option True (try t_object  >> many t_ws >> return False)
       if emptyColl then void (addTripleForObject rdfNilNode) else
        void
         (many (many t_ws >> try t_object >> many t_ws) >> popPred >>
         pushPred rdfRestNode >>
         addTripleForObject rdfNilNode >>
         popPred >> popSubj)
       finishColl
       return ()

blank_as_obj :: GenParser ParseState ()
blank_as_obj =
  -- if a node id, like _:a1, then create a BNode and add the triple
  (liftM BNode t_nodeID >>= addTripleForObject) <|>
  -- if a simple blank like [], do likewise
  (genBlank >>= addTripleForObject) <|>
  -- if a blank containing a predicateObjectList, like [ :b :c; :b :d ]
  poList
  where
    genBlank = liftM BNodeGen (try (string "[]") >> nextIdCounter)
    poList   = between (char '[') (char ']') $ 
                 liftM BNodeGen nextIdCounter >>= \bSubj ->   -- generate new bnode
                  void
                  (addTripleForObject bSubj >>   -- add triple with bnode as object
                  many t_ws >> pushSubj bSubj >> -- push bnode as new subject
                  t_predicateObjectList >> popSubj >> many t_ws) -- process polist, which uses bnode as subj, then pop bnode


rdfTypeNode, rdfNilNode, rdfFirstNode, rdfRestNode :: Node
rdfTypeNode   = UNode $ mkUri rdf "type"
rdfNilNode    = UNode $ mkUri rdf "nil"
rdfFirstNode  = UNode $ mkUri rdf "first"
rdfRestNode   = UNode $ mkUri rdf "rest"

xsdIntUri, xsdDoubleUri, xsdDecimalUri, xsdBooleanUri :: T.Text
xsdIntUri     = mkUri xsd "integer"
xsdDoubleUri  = mkUri xsd "double"
xsdDecimalUri = mkUri xsd "decimal"
xsdBooleanUri = mkUri xsd "boolean"

t_literal :: GenParser ParseState Node
t_literal =
  try str_literal <|>
  liftM (`mkLNode` xsdIntUri) (try t_integer)   <|>
  liftM (`mkLNode` xsdDoubleUri) (try t_double)  <|>
  liftM (`mkLNode` xsdDecimalUri) (try t_decimal) <|>
  liftM (`mkLNode` xsdBooleanUri) t_boolean
  where
    mkLNode :: T.Text -> T.Text -> Node
    mkLNode bsType bs' = LNode (typedL bsType bs')

str_literal :: GenParser ParseState Node
str_literal =
  do str <- t_quotedString <?> "quotedString"
     liftM (LNode . typedL str)
      (try (count 2 (char '^')) >> t_iri) <|>
      liftM (lnode . plainLL str) (char '@' >> t_language) <|>
      return (lnode $ plainL str)

t_quotedString :: GenParser ParseState T.Text
t_quotedString = try t_longString <|> t_string

-- a non-long string: any number of scharacters (echaracter without ") inside doublequotes or singlequotes.
t_string :: GenParser ParseState T.Text
t_string = liftM T.concat (between (char '"') (char '"') (many t_scharacter_in_dquot) <|> between (char '\'') (char '\'') (many t_scharacter_in_squot))

t_longString :: GenParser ParseState T.Text
t_longString =
    try ( do { tripleQuoteDbl;
               strVal <- liftM T.concat (many longString_char);
               tripleQuoteDbl;
               return strVal }) <|>
    try ( do { tripleQuoteSingle;
               strVal <- liftM T.concat (many longString_char);
               tripleQuoteSingle;
               return strVal })
  where
    tripleQuoteDbl = count 3 (char '"')
    tripleQuoteSingle = count 3 (char '\'')

t_integer :: GenParser ParseState T.Text
t_integer =
  do sign <- sign_parser <?> "+-"
     ds <- many1 digit   <?> "digit"
     notFollowedBy (char '.')
     -- integer must be in canonical format, with no leading plus sign or leading zero
     return $! ( T.pack sign `T.append` T.pack ds)

-- grammar rule: [21] DOUBLE
t_double :: GenParser ParseState T.Text
t_double =
  do sign <- sign_parser <?> "+-"
     rest <- try (do { ds <- many1 digit <?> "digit";
                      char '.';
                      ds' <- many digit <?> "digit";
                      e <- t_exponent <?> "exponent";
                      return ( T.pack ds `T.snoc` '.' `T.append`  T.pack ds' `T.append` e) }) <|>
             try (do { char '.';
                       ds <- many1 digit <?> "digit";
                       e <- t_exponent <?> "exponent";
                       return ('.' `T.cons`  T.pack ds `T.append` e) }) <|>
             try (do { ds <- many1 digit <?> "digit";
                       e <- t_exponent <?> "exponent";
                       return ( T.pack ds `T.append` e) })
     return $! T.pack sign `T.append` rest

sign_parser :: GenParser ParseState String
sign_parser = option "" (oneOf "-+" >>= (\c -> return [c]))

t_decimal :: GenParser ParseState T.Text
t_decimal =
  do sign <- sign_parser
     rest <- try (do ds <- many digit <?> "digit"; char '.'; ds' <- option "" (many digit); return (ds ++ ('.':ds')))
             <|> try (do { char '.'; ds <- many1 digit <?> "digit"; return ('.':ds) })
             <|> many1 digit <?> "digit"
     return $ T.pack sign `T.append`  T.pack rest

t_exponent :: GenParser ParseState T.Text
t_exponent = do e <- oneOf "eE"
                s <- option "" (oneOf "-+" >>= \c -> return [c])
                ds <- many1 digit;
                return $! (e `T.cons` ( T.pack s `T.append` T.pack ds))

t_boolean :: GenParser ParseState T.Text
t_boolean =
  try (liftM T.pack (string "true") <|>
  liftM T.pack (string "false"))

t_comment :: GenParser ParseState ()
t_comment =
  void (char '#' >> many (satisfy (\ c -> c /= '\n' && c /= '\r')))

t_ws :: GenParser ParseState ()
t_ws =
    (void (try (char '\t' <|> char '\n' <|> char '\r' <|> char ' '))
     <|> try t_comment)
    <?> "whitespace-or-comment"


t_language :: GenParser ParseState T.Text
t_language =
  do initial <- many1 lower;
     rest <- many (do {char '-'; cs <- many1 (lower <|> digit); return ( T.pack ('-':cs))})
     return $! ( T.pack initial `T.append` T.concat rest)

identifier :: GenParser ParseState Char -> GenParser ParseState Char -> GenParser ParseState T.Text
identifier initial rest = initial >>= \i -> many rest >>= \r -> return ( T.pack (i:r))

-- grammar rule: [167s] PN_PREFIX
t_pn_prefix :: GenParser ParseState T.Text
t_pn_prefix = do
  i <- try t_pn_chars_base
  r <- option "" (many (try t_pn_chars <|> char '.'))
  return (T.pack (i:r))

t_name :: GenParser ParseState T.Text
t_name = identifier t_pn_chars_u t_pn_chars

t_iriref :: GenParser ParseState T.Text
t_iriref = between (char '<') (char '>') t_relativeURI

t_relativeURI :: GenParser ParseState T.Text
t_relativeURI =
  do frag <- liftM (T.pack . concat) (many t_ucharacter)
     bUrl <- currBaseUrl
     dUrl <- currDocUrl
     return $ absolutizeUrl bUrl dUrl frag

-- We make this String rather than T.Text because we want
-- t_relativeURI (the only place it's used) to have chars so that
-- when it creates a T.Text it can all be in one chunk.
t_ucharacter :: GenParser ParseState String
t_ucharacter =
  try (liftM T.unpack unicode_escape) <|>
  try (string "\\>") <|>
  liftM T.unpack (non_ctrl_char_except ">")

t_pn_chars :: GenParser ParseState Char
t_pn_chars = t_pn_chars_u <|> char '-' <|> char '\x00B7' <|> satisfy f
  where
    f = flip in_range [('0', '9'), ('\x0300', '\x036F'), ('\x203F', '\x2040')]

longString_char :: GenParser ParseState T.Text
longString_char =
  specialChar        <|> -- \r|\n|\t as single char
  try escapedChar    <|> -- an backslash-escaped tab, newline, linefeed, backslash or doublequote
  try twoDoubleQuote <|> -- two doublequotes not followed by a doublequote
  try oneDoubleQuote <|> -- a single doublequote
  safeNonCtrlChar    <|> -- anything but a single backslash or doublequote
  try unicode_escape     -- a unicode escape sequence (\uxxxx or \Uxxxxxxxx)
  where
    specialChar     = oneOf "\t\n\r" >>= bs1
    escapedChar     =
      do char '\\'
         (char 't'  >> bs1 '\t') <|> (char 'n' >> bs1 '\n') <|> (char 'r' >> bs1 '\r') <|>
          (char '\\' >> bs1 '\\') <|> (char '"' >> bs1 '"')
    twoDoubleQuote  = string "\"\"" >> notFollowedBy (char '"') >> bs "\"\""
    oneDoubleQuote  = char '"' >> notFollowedBy (char '"') >> bs1 '"'
    safeNonCtrlChar = non_ctrl_char_except "\\\""

bs1 :: Char -> GenParser ParseState T.Text
bs1 = return . T.singleton

bs :: String -> GenParser ParseState T.Text
bs = return . T.pack

-- grammar rule: [163s] PN_CHARS_BASE
t_pn_chars_base :: GenParser ParseState Char
t_pn_chars_base = try $ satisfy $ flip in_range blocks
  where
    blocks = [('A', 'Z'), ('a', 'z'), ('\x00C0', '\x00D6'),
              ('\x00D8', '\x00F6'), ('\x00F8', '\x02FF'),
              ('\x0370', '\x037D'), ('\x037F', '\x1FFF'),
              ('\x200C', '\x200D'), ('\x2070', '\x218F'),
              ('\x2C00', '\x2FEF'), ('\x3001', '\xD7FF'),
              ('\xF900', '\xFDCF'), ('\xFDF0', '\xFFFD'),
              ('\x10000', '\xEFFFF')]

-- grammar rule: [164s] PN_CHARS_U
t_pn_chars_u :: GenParser ParseState Char
t_pn_chars_u = t_pn_chars_base <|> char '_'

-- grammar rules: [171s] HEX
t_hex :: GenParser ParseState Char
t_hex = satisfy (\c -> isDigit c || (c >= 'A' && c <= 'F')) <?> "hexadecimal digit"

-- characters used in (non-long) strings; any echaracters except ", or an escaped \"
-- echaracter - #x22 ) | '\"'
t_scharacter_in_dquot :: GenParser ParseState T.Text
t_scharacter_in_dquot =
  (try (string "\\\"") >> return (T.singleton '"'))
     <|> (try (string "\\'") >> return (T.singleton '\''))
     <|> try (do {char '\\';
                  (char 't' >> return (T.singleton '\t')) <|>
                  (char 'n' >> return (T.singleton '\n')) <|>
                  (char 'r' >> return (T.singleton '\r'))}) -- echaracter part 1
     <|> unicode_escape
     <|> (non_ctrl_char_except "\\\"" >>= \s -> return $! s) -- echaracter part 2 minus "

-- characters used in (non-long) strings; any echaracters except ', or an escaped \'
-- echaracter - #x22 ) | '\''
t_scharacter_in_squot :: GenParser ParseState T.Text
t_scharacter_in_squot =
  (try (string "\\'") >> return (T.singleton '\''))
     <|> (try (string "\\\"") >> return (T.singleton '"'))
     <|> try (do {char '\\';
                  (char 't' >> return (T.singleton '\t')) <|>
                  (char 'n' >> return (T.singleton '\n')) <|>
                  (char 'r' >> return (T.singleton '\r'))}) -- echaracter part 1
     <|> unicode_escape
     <|> (non_ctrl_char_except "\\'" >>= \s -> return $! s) -- echaracter part 2 minus '

unicode_escape :: GenParser ParseState T.Text
unicode_escape =
 (char '\\' >> return (T.singleton '\\')) >>
 ((char '\\' >> return "\\\\") <|>
  (char 'u' >> count 4 t_hex >>= \cs -> return $!  "\\u" `T.append`  T.pack cs) <|>
  (char 'U' >> count 8 t_hex >>= \cs -> return $!  "\\U" `T.append` T.pack cs))

non_ctrl_char_except :: String -> GenParser ParseState T.Text
non_ctrl_char_except cs =
  liftM T.singleton
    (satisfy (\ c -> c <= '\1114111' && (c >= ' ' && c `notElem` cs)))

{-# INLINE in_range #-}
in_range :: Char -> [(Char, Char)] -> Bool
in_range c = any (\(c1, c2) -> c >= c1 && c <= c2)

newBaseUrl :: Maybe BaseUrl -> T.Text -> BaseUrl
newBaseUrl Nothing               url = BaseUrl url
newBaseUrl (Just (BaseUrl bUrl)) url = BaseUrl $! mkAbsoluteUrl bUrl url

currBaseUrl :: GenParser ParseState (Maybe BaseUrl)
currBaseUrl = getState >>= \(bUrl, _, _, _, _, _, _, _) -> return bUrl

currDocUrl :: GenParser ParseState (Maybe T.Text)
currDocUrl = getState >>= \(_, dUrl, _, _, _, _, _, _) -> return dUrl

pushSubj :: Subject -> GenParser ParseState ()
pushSubj s = getState >>= \(bUrl, dUrl, i, pms, ss, ps, cs, ts) ->
                  setState (bUrl, dUrl, i, pms, s:ss, ps, cs, ts)

popSubj :: GenParser ParseState Subject
popSubj = getState >>= \(bUrl, dUrl, i, pms, ss, ps, cs, ts) ->
                setState (bUrl, dUrl, i, pms, tail ss, ps, cs, ts) >>
                  when (null ss) (error "Cannot pop subject off empty stack.") >>
                  return (head ss)

pushPred :: Predicate -> GenParser ParseState ()
pushPred p = getState >>= \(bUrl, dUrl, i, pms, ss, ps, cs, ts) ->
                  setState (bUrl, dUrl, i, pms, ss, p:ps, cs, ts)

popPred :: GenParser ParseState Predicate
popPred = getState >>= \(bUrl, dUrl, i, pms, ss, ps, cs, ts) ->
                setState (bUrl, dUrl, i, pms, ss, tail ps, cs, ts) >>
                  when (null ps) (error "Cannot pop predicate off empty stack.") >>
                  return (head ps)

isInColl :: GenParser ParseState Bool
isInColl = getState >>= \(_, _, _, _, _, _, cs, _) -> return . not . null $ cs

updateBaseUrl :: Maybe (Maybe BaseUrl) -> GenParser ParseState ()
updateBaseUrl val = _modifyState val no no no no no

-- combines get_current and increment into a single function
nextIdCounter :: GenParser ParseState Int
nextIdCounter = getState >>= \(bUrl, dUrl, i, pms, s, p, cs, ts) ->
                setState (bUrl, dUrl, i+1, pms, s, p, cs, ts) >> return i

updatePMs :: Maybe PrefixMappings -> GenParser ParseState ()
updatePMs val = _modifyState no no val no no no

-- Register that we have begun processing a collection
beginColl :: GenParser ParseState ()
beginColl = getState >>= \(bUrl, dUrl, i, pms, s, p, cs, ts) ->
            setState (bUrl, dUrl, i, pms, s, p, True:cs, ts)

onCollFirstItem :: GenParser ParseState Bool
onCollFirstItem = getState >>= \(_, _, _, _, _, _, cs, _) -> return (not (null cs) && head cs)

collFirstItemProcessed :: GenParser ParseState ()
collFirstItemProcessed = getState >>= \(bUrl, dUrl, i, pms, s, p, _:cs, ts) ->
                         setState (bUrl, dUrl, i, pms, s, p, False:cs, ts)

-- Register that a collection is finished being processed; the bool value
-- in the monad is *not* the value that was popped from the stack, but whether
-- we are still processing a parent collection or have finished processing
-- all collections and are no longer in a collection at all.
finishColl :: GenParser ParseState Bool
finishColl = getState >>= \(bUrl, dUrl, i, pms, s, p, cs, ts) ->
             let cs' = drop 1 cs
             in setState (bUrl, dUrl, i, pms, s, p, cs', ts) >> return (not $ null cs')

-- Alias for Nothing for use with _modifyState calls, which can get very long with
-- many Nothing values.
no :: Maybe a
no = Nothing

-- Update the subject and predicate values of the ParseState to Nothing.
resetSubjectPredicate :: GenParser ParseState ()
resetSubjectPredicate =
  getState >>= \(bUrl, dUrl, n, pms, _, _, cs, ts) ->
  setState (bUrl, dUrl, n, pms, [], [], cs, ts)

-- Modifies the current parser state by updating any state values among the parameters
-- that have non-Nothing values.
_modifyState :: Maybe (Maybe BaseUrl) -> Maybe (Int -> Int) -> Maybe PrefixMappings ->
                Maybe Subject -> Maybe Predicate -> Maybe (Seq Triple) ->
                GenParser ParseState ()
_modifyState mb_bUrl mb_n mb_pms mb_subj mb_pred mb_trps =
  do (_bUrl, _dUrl, _n, _pms, _s, _p, _cs, _ts) <- getState
     setState (fromMaybe _bUrl mb_bUrl,
              _dUrl,
              maybe _n (const _n) mb_n,
              fromMaybe _pms mb_pms,
              maybe _s (: _s) mb_subj,
              maybe _p (: _p) mb_pred,
              _cs,
              fromMaybe _ts mb_trps)

addTripleForObject :: Object -> GenParser ParseState ()
addTripleForObject obj =
  do (bUrl, dUrl, i, pms, ss, ps, cs, ts) <- getState
     when (null ss) $
       error $ "No Subject with which to create triple for: " ++ show obj
     when (null ps) $
       error $ "No Predicate with which to create triple for: " ++ show obj
     setState (bUrl, dUrl, i, pms, ss, ps, cs, ts |> Triple (head ss) (head ps) obj)

-- |Parse the document at the given location URL as a Turtle document, using an optional @BaseUrl@
-- as the base URI, and using the given document URL as the URI of the Turtle document itself.
--
-- The @BaseUrl@ is used as the base URI within the document for resolving any relative URI references.
-- It may be changed within the document using the @\@base@ directive. At any given point, the current
-- base URI is the most recent @\@base@ directive, or if none, the @BaseUrl@ given to @parseURL@, or 
-- if none given, the document URL given to @parseURL@. For example, if the @BaseUrl@ were
-- @http:\/\/example.org\/@ and a relative URI of @\<b>@ were encountered (with no preceding @\@base@ 
-- directive), then the relative URI would expand to @http:\/\/example.org\/b@.
--
-- The document URL is for the purpose of resolving references to 'this document' within the document,
-- and may be different than the actual location URL from which the document is retrieved. Any reference
-- to @\<>@ within the document is expanded to the value given here. Additionally, if no @BaseUrl@ is 
-- given and no @\@base@ directive has appeared before a relative URI occurs, this value is used as the
-- base URI against which the relative URI is resolved.
--
-- Returns either a @ParseFailure@ or a new RDF containing the parsed triples.
parseURL' :: forall rdf. (RDF rdf) => 
                 Maybe BaseUrl       -- ^ The optional base URI of the document.
                 -> Maybe T.Text     -- ^ The document URI (i.e., the URI of the document itself); if Nothing, use location URI.
                 -> String           -- ^ The location URI from which to retrieve the Turtle document.
                 -> IO (Either ParseFailure rdf)
                                     -- ^ The parse result, which is either a @ParseFailure@ or the RDF
                                     --   corresponding to the Turtle document.
parseURL' bUrl docUrl = _parseURL (parseString' bUrl docUrl)

-- |Parse the given file as a Turtle document. The arguments and return type have the same semantics
-- as 'parseURL', except that the last @String@ argument corresponds to a filesystem location rather
-- than a location URI.
--
-- Returns either a @ParseFailure@ or a new RDF containing the parsed triples.
parseFile' :: forall rdf. (RDF rdf) => Maybe BaseUrl -> Maybe T.Text -> String -> IO (Either ParseFailure rdf)
parseFile' bUrl docUrl fpath =
  TIO.readFile fpath >>= \bs' -> return $ handleResult bUrl (runParser t_turtleDoc initialState (maybe "" T.unpack docUrl) bs')
  where initialState = (bUrl, docUrl, 1, PrefixMappings Map.empty, [], [], [], Seq.empty)

-- |Parse the given string as a Turtle document. The arguments and return type have the same semantics 
-- as <parseURL>, except that the last @String@ argument corresponds to the Turtle document itself as
-- a string rather than a location URI.
parseString' :: forall rdf. (RDF rdf) => Maybe BaseUrl -> Maybe T.Text -> T.Text -> Either ParseFailure rdf
parseString' bUrl docUrl ttlStr = handleResult bUrl (runParser t_turtleDoc initialState "" ttlStr)
  where initialState = (bUrl, docUrl, 1, PrefixMappings Map.empty, [], [], [], Seq.empty)

handleResult :: RDF rdf => Maybe BaseUrl -> Either ParseError (Seq Triple, PrefixMappings) -> Either ParseFailure rdf
handleResult bUrl result =
  case result of
    (Left err)         -> Left (ParseFailure $ show err)
    (Right (ts, pms))  -> Right $! mkRdf (F.toList ts) bUrl pms

--------------
-- auxiliary parsing functions

-- Match the lowercase or uppercase form of 'c'
caseInsensitiveChar :: Char -> GenParser ParseState Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

-- Match the string 's', accepting either lowercase or uppercase form of each character
caseInsensitiveString :: String -> GenParser ParseState String
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""
