-- |An 'RdfParser' implementation for the Turtle format
-- <http://www.w3.org/TeamSubmission/turtle/>.

module Text.RDF.RDF4H.TurtleParser(
  TurtleParser(TurtleParser)
)

where

import Data.Char (isLetter,isAlphaNum,toLower,toUpper)
import Data.Maybe
import Data.RDF.Types
import Data.RDF.Namespace
import Text.RDF.RDF4H.ParserUtils
import Text.Parsec
import Text.Parsec.Text
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Sequence(Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Char (isDigit)
import Control.Monad
import Data.List

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
-- subject predicateObjectList | blankNodePropertyList predicateObjectList?
t_triples :: GenParser ParseState ()
t_triples = do
  try (t_subject >> many t_ws >> t_predicateObjectList >> resetSubjectPredicate) <|> (liftM BNodeGen nextIdCounter >>= \bSubj -> pushSubj bSubj >> t_blankNodePropertyList >> many t_ws >> optional t_predicateObjectList >> resetSubjectPredicate)

-- [14]	blankNodePropertyList ::= '[' predicateObjectList ']'
t_blankNodePropertyList :: GenParser ParseState ()
t_blankNodePropertyList = between (char '[') (char ']') (many t_ws >> t_predicateObjectList >> void (many t_ws))

-- grammar rule: [3] directive
t_directive :: GenParser ParseState ()
t_directive = t_prefixID <|> t_base <|> t_sparql_prefix <|> t_sparql_base

-- grammar rule: [135s] iri
-- IRIREF | PrefixedName
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
  do void (try (string "@prefix" <?> "@prefix-directive"))
     pre <- (many1 t_ws <?> "whitespace-after-@prefix") >> option T.empty t_pn_prefix
     void (char ':' >> (many1 t_ws <?> "whitespace-after-@prefix-colon"))
     uriFrag <- t_iriref
     void (many t_ws <?> "prefixID-whitespace")
     void (char '.' <?> "end-of-prefixID-period")
     (bUrl, dUrl, _, PrefixMappings pms, _, _, _, _) <- getState
     updatePMs $ Just (PrefixMappings $ Map.insert pre (absolutizeUrl bUrl dUrl uriFrag) pms)
     return ()

-- grammar rule: [6s] sparqlPrefix
t_sparql_prefix :: GenParser ParseState ()
t_sparql_prefix =
  do void (try (caseInsensitiveString "PREFIX" <?> "@prefix-directive"))
     pre <- (many1 t_ws <?> "whitespace-after-@prefix") >> option T.empty t_pn_prefix
     void (char ':' >> (many1 t_ws <?> "whitespace-after-@prefix-colon"))
     uriFrag <- t_iriref
     (bUrl, dUrl, _, PrefixMappings pms, _, _, _, _) <- getState
     updatePMs $ Just (PrefixMappings $ Map.insert pre (absolutizeUrl bUrl dUrl uriFrag) pms)
     return ()

-- grammar rule: [5] base
t_base :: GenParser ParseState ()
t_base =
  do void (try (string "@base" <?> "@base-directive"))
     void (many1 t_ws <?> "whitespace-after-@base")
     urlFrag <- t_iriref
     void (many t_ws <?> "base-whitespace")
     (void (char '.') <?> "end-of-base-period")
     bUrl <- currBaseUrl
     dUrl <- currDocUrl
     updateBaseUrl (Just $ Just $ newBaseUrl bUrl (absolutizeUrl bUrl dUrl urlFrag))

-- grammar rule: [5s] sparqlBase
t_sparql_base :: GenParser ParseState ()
t_sparql_base =
  do void (try (caseInsensitiveString "BASE" <?> "@sparql-base-directive"))
     void (many1 t_ws <?> "whitespace-after-BASE")
     urlFrag <- t_iriref
     bUrl <- currBaseUrl
     dUrl <- currDocUrl
     updateBaseUrl (Just $ Just $ newBaseUrl bUrl (absolutizeUrl bUrl dUrl urlFrag))

t_verb :: GenParser ParseState ()
-- [9]	verb ::= predicate | 'a'
t_verb = (try t_predicate <|> (char 'a' >> return rdfTypeNode)) >>= pushPred

-- grammar rule: [11] predicate
t_predicate :: GenParser ParseState Node
t_predicate = liftM UNode (t_iri <?> "resource")

t_nodeID  :: GenParser ParseState T.Text
t_nodeID = do { void (try (string "_:")); cs <- t_name; return $! "_:" `T.append` cs }

-- grammar rules: [139s] PNAME_NS
t_pname_ns :: GenParser ParseState T.Text
t_pname_ns =do
  pre <- option T.empty (try t_pn_prefix)
  void (char ':')
  (bUrl, _, _, pms, _, _, _, _) <- getState
  case resolveQName bUrl pre pms of
    Just n  -> return n
    Nothing -> unexpected ("Cannot resolve QName prefix: " ++ T.unpack pre)

-- grammar rules: [168s] PN_LOCAL
-- [168s] PN_LOCAL ::= (PN_CHARS_U | ':' | [0-9] | PLX) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))?
t_pn_local :: GenParser ParseState T.Text
t_pn_local = do
  x <- t_pn_chars_u_str <|> string ":" <|> satisfy_str <|> t_plx
  xs <- option "" $ try $ do
               let recsve = (t_pn_chars_str <|> string ":" <|> t_plx) <|>
                            (t_pn_chars_str <|> string ":" <|> t_plx <|> try (string "." <* lookAhead (try recsve))) <|>
                            (t_pn_chars_str <|> string ":" <|> t_plx <|> try (string "." >> notFollowedBy t_ws >> return "."))
               concat <$> many recsve
  return (T.pack (x ++ xs))
    where
      satisfy_str = satisfy (flip in_range [('0', '9')]) >>= \c -> return [c]
      t_pn_chars_str = t_pn_chars >>= \c -> return [c]
      t_pn_chars_u_str = t_pn_chars_u >>= \c -> return [c]

-- PERCENT | PN_LOCAL_ESC
-- grammar rules: [169s] PLX
t_plx :: GenParser ParseState String
t_plx = t_percent <|> t_pn_local_esc_str
    where
      t_pn_local_esc_str = do
        c <- t_pn_local_esc
        return ([c])

--        '%' HEX HEX
-- grammar rules: [170s] PERCENT
t_percent :: GenParser ParseState String
t_percent = do
  void (char '%')
  h1 <- t_hex
  h2 <- t_hex
  return (['%',h1,h2])

-- grammar rules: [172s] PN_LOCAL_ESC
t_pn_local_esc :: GenParser ParseState Char
t_pn_local_esc = char '\\' >> oneOf "_~.-!$&'()*+,;=/?#@%"

-- grammar rules: [140s] PNAME_LN
t_pname_ln :: GenParser ParseState T.Text
t_pname_ln =
  do pre <- t_pname_ns
     name <- t_pn_local
     return (pre `T.append` name)

-- grammar rule: [10] subject
-- [10] subject	::= iri | BlankNode | collection
t_subject :: GenParser ParseState ()
t_subject =
  iri <|>
  t_blankNode <|>
  t_collection
  where
    iri         = liftM unode (try t_iri <?> "subject resource") >>= \s -> pushSubj s

-- [137s] BlankNode ::= BLANK_NODE_LABEL | ANON
t_blankNode :: GenParser ParseState ()
t_blankNode = (try t_blank_node_label <|> t_anon) >> nextIdCounter >>= \i -> (pushSubj . BNodeGen) i

-- TODO replicate the recursion technique from [168s] for ((..)* something)?
-- [141s] BLANK_NODE_LABEL ::= '_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
t_blank_node_label :: GenParser ParseState ()
t_blank_node_label = do
  void (string "_:")
  void (t_pn_chars_u <|> oneOf ['0'..'9'])
  optional $ try $ do
    ss <- option "" $ do
            xs <- many (t_pn_chars <|> char '.')
            if null xs
            then return xs
            else if last xs == '.'
                 then unexpected "'.' at the end of a blank node label"
                 else return xs
    return ss

-- [162s] ANON ::= '[' WS* ']'
t_anon :: GenParser ParseState ()
t_anon = between (char '[') (char ']') (void (many t_ws))

-- [7] predicateObjectList ::= verb objectList (';' (verb objectList)?)*
t_predicateObjectList :: GenParser ParseState ()
t_predicateObjectList = do
  void (sepEndBy1 (try (t_verb >> many1 t_ws >> t_objectList >> popPred)) (try (many t_ws >> char ';' >> optional (char ';') >> many t_ws)))

-- grammar rule: [8] objectlist
-- [8] objectList ::= object (',' object)*
t_objectList :: GenParser ParseState ()
t_objectList = -- t_object actually adds the triples
  void
  ((t_object <?> "object") >>
  many (try (many t_ws >> char ',' >> many t_ws >> t_object)))

-- grammar rule: [12] object
-- [12]	object ::= iri | BlankNode | collection | blankNodePropertyList | literal
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
       void (many t_ws)
       emptyColl <- option True (try t_object  >> many t_ws >> return False)
       if emptyColl then void (addTripleForObject rdfNilNode) else
        void
         (many (many t_ws >> try t_object >> many t_ws) >> popPred >>
         pushPred rdfRestNode >>
         addTripleForObject rdfNilNode >>
         popPred >> popSubj)
       void finishColl


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
                  many t_ws >> (pushSubj bSubj) >> -- push bnode as new subject
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
  (try t_rdf_literal >>= \l -> return (LNode l))   <|>
  liftM (`mkLNode` xsdDoubleUri) (try t_double)    <|>
  liftM (`mkLNode` xsdDecimalUri) (try t_decimal)  <|>
  liftM (`mkLNode` xsdIntUri) (try t_integer)      <|>
  liftM (`mkLNode` xsdBooleanUri) t_boolean
   where
    mkLNode :: T.Text -> T.Text -> Node
    mkLNode bsType bs' = LNode (typedL bsType bs')

-- [128s] RDFLiteral
-- String (LANGTAG | '^^' iri)?
t_rdf_literal :: GenParser ParseState LValue
t_rdf_literal = do
  str' <- t_string
  let str = escapeRDFSyntax str'
  option (plainL str) $ do
                  (try (t_langtag >>= \lang -> return (plainLL str lang)) <|>
                   ((count 2 (char '^') >> t_iri >>= \iri -> return (typedL str iri))))

-- [17] String
-- STRING_LITERAL_QUOTE | STRING_LITERAL_SINGLE_QUOTE | STRING_LITERAL_LONG_SINGLE_QUOTE | STRING_LITERAL_LONG_QUOTE
t_string :: GenParser ParseState T.Text
t_string = try t_string_literal_long_quote <|>
           try t_string_literal_long_single_quote <|>
           try t_string_literal_quote <|>
           t_string_literal_single_quote

-- [22]	STRING_LITERAL_QUOTE
-- '"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'
t_string_literal_quote :: GenParser ParseState T.Text
t_string_literal_quote =
     between (char '"') (char '"') $ do
      T.concat <$> many (T.singleton <$> noneOf ['\x22','\x5C','\xA','\xD'] <|>
            t_echar <|>
            t_uchar)

-- [23] STRING_LITERAL_SINGLE_QUOTE
-- "'" ([^#x27#x5C#xA#xD] | ECHAR | UCHAR)* "'"
t_string_literal_single_quote :: GenParser ParseState T.Text
t_string_literal_single_quote =
    between (char '\'') (char '\'') $ do
      T.concat <$>
       many (T.singleton <$> noneOf ['\x27','\x5C','\xA','\xD'] <|>
             t_echar <|>
             t_uchar)

-- [24] STRING_LITERAL_LONG_SINGLE_QUOTE
-- "'''" (("'" | "''")? ([^'\] | ECHAR | UCHAR))* "'''"
t_string_literal_long_single_quote :: GenParser ParseState T.Text
t_string_literal_long_single_quote =
    between ((string "'''")) ((string "'''")) $ do
      ss <- many $ try $ do
        s1 <- T.pack <$> option "" (try (string "''") <|> string "'")
        s2 <- T.singleton <$> noneOf ['\'','\\'] <|> t_echar <|> t_uchar
        return (s1 `T.append` s2)
      return (T.concat ss)

-- [25] STRING_LITERAL_LONG_QUOTE
-- '"""' (('"' | '""')? ([^"\] | ECHAR | UCHAR))* '"""'
t_string_literal_long_quote :: GenParser ParseState T.Text
t_string_literal_long_quote =
     between (string "\"\"\"") (string "\"\"\"") $ do
      ss <- many $ try $ do
              s1 <- T.pack <$> option "" (try (string "\"\"") <|> string "\"")
              s2 <- (T.singleton <$> noneOf ['"','\\']) <|> t_echar <|> t_uchar
              return (s1 `T.append` s2)
      return (T.concat ss)

-- [144s] LANGTAG
-- '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
t_langtag :: GenParser ParseState T.Text
t_langtag = do
    void (char '@')
    ss   <- many1 (satisfy (\ c -> isLetter c))
    rest <- concat <$> many (char '-' >> many1 (satisfy (\ c -> isAlphaNum c)) >>= \lang_str -> return ('-':lang_str))
    return (T.pack (ss ++ rest))

-- [159s]	ECHAR
-- '\' [tbnrf"'\]
t_echar :: GenParser ParseState T.Text
t_echar = try $ do
    void (char '\\')
    c2 <- oneOf ['t','b','n','r','f','"','\'','\\']
    return $ case c2 of
               't'  -> T.singleton '\t'
               'b'  -> T.singleton '\b'
               'n'  -> T.singleton '\n'
               'r'  -> T.singleton '\r'
               'f'  -> T.singleton '\f'
               '"'  -> T.singleton '\"'
               '\'' -> T.singleton '\''
               '\\' -> T.singleton '\\'
               _    -> error "nt_echar: impossible error."

-- [26]	UCHAR
-- '\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
t_uchar :: GenParser ParseState T.Text
t_uchar =
    (try (string "\\u" >> count 4 hexDigit) >>= \cs -> return $ T.pack ('\\':'u':cs)) <|>
     (char '\\' >> char 'U' >> count 8 hexDigit >>= \cs -> return $ T.pack ('\\':'U':cs))

-- [19] INTEGER ::= [+-]? [0-9]+
t_integer :: GenParser ParseState T.Text
t_integer = try $
  do sign <- sign_parser <?> "+-"
     ds <- many1 (oneOf ['0'..'9'] <?> "digit")
     return $! ( T.pack sign `T.append` T.pack ds)

-- grammar rule: [21] DOUBLE
-- [21] DOUBLE ::= [+-]? ([0-9]+ '.' [0-9]* EXPONENT | '.' [0-9]+ EXPONENT | [0-9]+ EXPONENT)
t_double :: GenParser ParseState T.Text
t_double =
  do sign <- sign_parser <?> "+-"
     rest <- try (do { ds <- many1 (oneOf ['0'..'9']) <?> "digit";
                      void (char '.');
                      ds' <- many (oneOf ['0'..'9']) <?> "digit";
                      e <- t_exponent <?> "exponent";
                      return ( T.pack ds `T.snoc` '.' `T.append`  T.pack ds' `T.append` e) }) <|>
             try (do { void (char '.');
                       ds <- many1 (oneOf ['0'..'9']) <?> "digit";
                       e <- t_exponent <?> "exponent";
                       return ('.' `T.cons`  T.pack ds `T.append` e) }) <|>
                 (do { ds <- many1 (oneOf ['0'..'9']) <?> "digit";
                       e <- t_exponent <?> "exponent";
                       return ( T.pack ds `T.append` e) })
     return $! T.pack sign `T.append` rest

sign_parser :: GenParser ParseState String
sign_parser = option "" (oneOf "-+" >>= (\c -> return [c]))

-- [20]	DECIMAL ::= [+-]? [0-9]* '.' [0-9]+
t_decimal :: GenParser ParseState T.Text
t_decimal = try $ do
              sign <- sign_parser
              dig1 <- many (oneOf ['0'..'9'])
              void (char '.')
              dig2 <- many1 (oneOf ['0'..'9'])
              return (T.pack sign `T.append`  T.pack dig1 `T.append` T.pack "." `T.append` T.pack dig2)

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

-- [161s] WS ::= #x20 | #x9 | #xD | #xA
t_ws :: GenParser ParseState ()
t_ws =
    (void (try (char '\t' <|> char '\n' <|> char '\r' <|> char ' '))
     <|> try t_comment)
    <?> "whitespace-or-comment"

-- grammar rule: [167s] PN_PREFIX
t_pn_prefix :: GenParser ParseState T.Text
t_pn_prefix = do
  i <- try t_pn_chars_base
  r <- option "" (many (try t_pn_chars <|> char '.')) -- TODO: ensure t_pn_chars is last char
  return (T.pack (i:r))

-- (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
t_name :: GenParser ParseState T.Text
t_name = try $ do
           ch  <- t_pn_chars_u <|> oneOf ['0'..'9']
           str <- many (t_pn_chars <|> char '.')
           if last (ch:str) == '.'
           then unexpected ("t_name: literal " ++ (ch:str) ++ " doesn't end with PN_CHARS")
           else return (T.pack (ch : str))

-- [18] IRIREF
t_iriref :: GenParser ParseState T.Text
t_iriref =
  between (char '<') (char '>') $ do
    iri <- T.concat <$> many ( T.singleton <$> noneOf (['\x00'..'\x20'] ++ ['<','>','"','{','}','|','^','`','\\']) <|>
                               t_uchar )
    bUrl <- currBaseUrl
    dUrl <- currDocUrl
    let iri' = escapeRDFSyntax iri
    validateURI (absolutizeUrl bUrl dUrl iri')

t_pn_chars :: GenParser ParseState Char
t_pn_chars = t_pn_chars_u <|> char '-' <|> char '\x00B7' <|> satisfy f
  where
    f = flip in_range [('0', '9'), ('\x0300', '\x036F'), ('\x203F', '\x2040')]

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
       unexpected $ "No Subject with which to create triple for: " ++ show obj
     when (null ps) $
       unexpected $ "No Predicate with which to create triple for: " ++ show obj
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
parseFile' bUrl docUrl fpath = do
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

validateUNode :: T.Text -> GenParser ParseState Node
validateUNode t =
    case unodeValidate t of
      Nothing        -> unexpected ("Invalid URI in Turtle parser URI validation: " ++ show t)
      Just u@(UNode{}) -> return u
      Just node      -> unexpected ("Unexpected node in Turtle parser URI validation: " ++ show node)

validateURI :: T.Text -> GenParser ParseState T.Text
validateURI t = do
    UNode uri <- validateUNode t
    return uri

--------------
-- auxiliary parsing functions

-- Match the lowercase or uppercase form of 'c'
caseInsensitiveChar :: Char -> GenParser ParseState Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

-- Match the string 's', accepting either lowercase or uppercase form of each character
caseInsensitiveString :: String -> GenParser ParseState String
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""
