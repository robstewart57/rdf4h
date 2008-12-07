module Text.RDF.TurtleParser(
  parseFile, parseURL, parseString, ParseFailure
)

where

import Text.RDF.Core
import Text.RDF.Namespace
import Text.RDF.ParserUtils

import Text.Parsec
import Text.Parsec.ByteString.Lazy

import qualified Data.Map as Map

import Data.ByteString.Lazy.Char8(ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy as BL

import Data.Sequence(Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F

import Control.Monad

import Debug.Trace(trace)

-- http://www.w3.org/TeamSubmission/turtle/

type ParseState =
  (Maybe BaseUrl,    -- the current BaseUrl, may be Nothing initially, but not after it is once set
   Maybe ByteString, -- the docUrl, which never changes but may or may not be used
   Int,              -- the id counter, containing the value of the next id to be used
   PrefixMappings,   -- the mappings from prefix to URI that are encountered while parsing
   [Subject],        -- stack of current subject nodes, if we have parsed a subject but not finished the triple
   [Predicate],      -- stack of current predicate nodes, if we've parsed a predicate but not finished the triple
   [Bool],           -- a stack of values to indicate that we're processing a (possibly nested) collection; top True indicates just started (on first element)
   Seq Triple)       -- the triples encountered while parsing; always added to on the right side

t_turtleDoc :: GenParser ByteString ParseState (Seq Triple, PrefixMappings)
t_turtleDoc =
  many t_statement >> (eof <?> "eof") >> getState >>= \(_, _, _, pms, _, _, _, ts) -> return (ts, pms)

t_statement :: GenParser ByteString ParseState ()
t_statement = d <|> t <|> ((many1 t_ws <?> "blankline-whitespace") >> return ())
  where
    d = try t_directive >> (many t_ws <?> "directive-whitespace1") >> (char '.' <?> "end-of-directive-period") >> (many t_ws <?> "directive-whitespace2") >> return ()
    t = t_triples >> (many t_ws <?> "triple-whitespace1") >> (char '.' <?> "end-of-triple-period") >> (many t_ws <?> "triple-whitespace2") >> return ()

t_triples :: GenParser ByteString ParseState ()
t_triples = t_subject >> (many1 t_ws <?> "subject-predicate-whitespace") >> t_predicateObjectList >> resetSubjectPredicate

t_directive :: GenParser ByteString ParseState ()
t_directive = (t_prefixID <|> t_base)

t_resource :: GenParser ByteString ParseState (ByteString)
t_resource =  (try t_uriref) <|> t_qname

t_prefixID :: GenParser ByteString ParseState ()
t_prefixID =
  do try (string "@prefix" <?> "@prefix-directive")
     pre <- (many1 t_ws <?> "whitespace-after-@prefix") >> option B.empty t_prefixName
     char ':' >> (many1 t_ws <?> "whitespace-after-@prefix-colon")
     uriFrag <- t_uriref
     (bUrl, dUrl, _, (PrefixMappings pms), _, _, _, _) <- getState
     updatePMs $ Just (PrefixMappings $ Map.insert pre (absolutizeUrl bUrl dUrl uriFrag) pms)
     return ()

t_base :: GenParser ByteString ParseState ()
t_base =
  do try (string "@base" <?> "@base-directive")
     many1 t_ws <?> "whitespace-after-@base"
     urlFrag <- t_uriref
     bUrl <- currBaseUrl
     dUrl <- currDocUrl
     updateBaseUrl (Just $ Just $ newBaseUrl bUrl (absolutizeUrl bUrl dUrl urlFrag))

t_verb :: GenParser ByteString ParseState ()
t_verb = ((try t_predicate) <|> (char 'a' >> return rdfTypeNode)) >>= pushPred

t_predicate :: GenParser ByteString ParseState Node
t_predicate = (t_resource <?> "resource") >>= return . UNode . mkFastString

t_nodeID  :: GenParser ByteString ParseState ByteString
t_nodeID = do { try (string "_:"); cs <- t_name; return $! s2b "_:" `B.append` cs }

t_qname :: GenParser ByteString ParseState ByteString
t_qname =
  do pre <- option B.empty (try t_prefixName)
     char ':'
     name <- option B.empty t_name
     (bUrl, _, _, pms, _, _, _, _) <- getState
     return $ resolveQName bUrl pre pms `B.append` name

t_subject :: GenParser ByteString ParseState ()
t_subject =
  simpleBNode <|>
  resource <|>
  nodeId <|>
  between (char '[') (char ']') poList
  where
    resource    = (t_resource <?> "subject resource") >>= return . UNode . mkFastString >>= pushSubj
    nodeId      = (t_nodeID   <?> "subject nodeID")   >>= return . BNode . mkFastString >>= pushSubj
    simpleBNode = try (string "[]") >> nextIdCounter >>=  pushSubj . BNodeGen
    poList      = nextIdCounter >>= pushSubj . BNodeGen >>
                    many t_ws >> t_predicateObjectList >> many t_ws >> return ()

-- verb ws+ objectList ( ws* ';' ws* verb ws+ objectList )* (ws* ';')?
t_predicateObjectList :: GenParser ByteString ParseState ()
t_predicateObjectList =
  do t_verb <?> "verb"     -- pushes pred onto pred stack
     many1 t_ws   <?> "polist-whitespace-after-verb"
     t_objectList <?> "polist-objectList"
     many (try (many t_ws >> char ';') >> many t_ws >> t_verb >> many1 t_ws >> t_objectList >> popPred)
     popPred               -- pop off the predicate pushed by 1st t_verb
     return ()

t_objectList :: GenParser ByteString ParseState ()
t_objectList = -- t_object actually adds the triples
  (t_object <?> "object") >>
  many (try (many t_ws >> char ',' >> many t_ws >> t_object)) >> return ()

t_object :: GenParser ByteString ParseState ()
t_object =
  do inColl      <- isInColl          -- whether this object is in a collection
     onFirstItem <- onCollFirstItem   -- whether we're on the first item of the collection
     let processObject = ((t_literal >>= addTripleForObject) <|>
                          (t_resource >>= return . UNode . mkFastString >>= addTripleForObject) <|>
                          blank_as_obj <|> t_collection)
     case (inColl, onFirstItem) of
       (False, _)    -> processObject
       (True, True)  -> nextIdCounter >>= return . BNodeGen >>= \bSubj -> addTripleForObject bSubj >>
                          pushSubj bSubj >> pushPred rdfFirstNode >> processObject >> collFirstItemProcessed
       (True, False) -> nextIdCounter >>= return . BNodeGen >>= \bSubj -> pushPred rdfRestNode >>
                          addTripleForObject bSubj >> popPred >> popSubj >>
                          pushSubj bSubj >> processObject

-- collection: '(' ws* itemList? ws* ')'
-- itemList:      object (ws+ object)*
t_collection:: GenParser ByteString ParseState ()
t_collection =
  -- ( object1 object2 ) is short for:
  -- [ rdf:first object1; rdf:rest [ rdf:first object2; rdf:rest rdf:nil ] ]
  -- ( ) is short for the resource:  rdf:nil
  between (char '(') (char ')') $
    do beginColl
       many t_ws
       emptyColl <- option (True) (try t_object  >> many t_ws >> return False)
       case emptyColl of
         True  -> addTripleForObject rdfNilNode >> return ()
         False -> many (many t_ws >> try t_object >> many t_ws) >> popPred >>
                    pushPred rdfRestNode >> addTripleForObject rdfNilNode >> popPred >> return ()
       finishColl
       return ()

blank_as_obj :: GenParser ByteString ParseState ()
blank_as_obj =
  -- if a node id, like _:a1, then create a BNode and add the triple
  (t_nodeID >>= return . BNode . mkFastString >>= addTripleForObject) <|>
  -- if a simple blank like [], do likewise
  (genBlank >>= addTripleForObject) <|>
  -- if a blank containing a predicateObjectList, like [ :b :c; :b :d ]
  poList
  where
    genBlank = try (string "[]") >> nextIdCounter >>=  return . BNodeGen
    poList   = between (char '[') (char ']') $ 
                 nextIdCounter >>= return . BNodeGen >>= \bSubj ->          -- generate new bnode
                 addTripleForObject bSubj >>                                -- add triple with bnode as object
                 many t_ws >> pushSubj bSubj >>                             -- push bnode as new subject
                 t_predicateObjectList >> popSubj >> many t_ws >> return () -- process polist, which uses bnode as subj, then pop bnode

rdfTypeNode, rdfNilNode, rdfFirstNode, rdfRestNode :: Node
rdfTypeNode   = UNode $ mkFastString $ makeUri rdf $ s2b "type"
rdfNilNode    = UNode $ mkFastString $ makeUri rdf $ s2b "nil"
rdfFirstNode  = UNode $ mkFastString $ makeUri rdf $ s2b "first"
rdfRestNode   = UNode $ mkFastString $ makeUri rdf $ s2b "rest"

xsdIntUri, xsdDoubleUri, xsdDecimalUri, xsdBooleanUri :: FastString
xsdIntUri     =  mkFastString $! makeUri xsd $! s2b "integer"
xsdDoubleUri  =  mkFastString $! makeUri xsd $! s2b "double"
xsdDecimalUri =  mkFastString $! makeUri xsd $! s2b "decimal"
xsdBooleanUri =  mkFastString $! makeUri xsd $! s2b "boolean"

t_literal :: GenParser ByteString ParseState Node
t_literal =
  try str_literal <|>
  (try t_integer >>= return . flip mkLNode xsdIntUri)   <|>
  (try t_double  >>= return . flip mkLNode xsdDoubleUri)  <|>
  (try t_decimal >>= return . flip mkLNode xsdDecimalUri) <|>
  (t_boolean     >>= return . flip mkLNode xsdBooleanUri)
  where
    mkLNode :: ByteString -> FastString -> Node
    mkLNode bs fs = LNode (typedL bs fs)

str_literal :: GenParser ByteString ParseState Node
str_literal =
  do str <- (t_quotedString <?> "quotedString")
     ((try (count 2 (char '^')) >> t_resource >>= return . LNode . typedL str . mkFastString) <|>
      (char '@' >> t_language >>= return . lnode . plainLL str) <|>
      (return $ lnode $ plainL str))

t_quotedString  :: GenParser ByteString ParseState ByteString
t_quotedString = t_longString <|> t_string

-- a non-long string: any number of scharacters (echaracter without ") inside doublequotes.
t_string  :: GenParser ByteString ParseState ByteString
t_string = between (char '"') (char '"') (many t_scharacter) >>= return . B.concat

t_longString  :: GenParser ByteString ParseState ByteString
t_longString =
  do
    try tripleQuote
    strVal <- many longString_char >>= return . B.concat
    tripleQuote
    return strVal
  where
    tripleQuote = count 3 (char '"')

t_integer :: GenParser ByteString ParseState ByteString
t_integer =
  do sign <- sign_parser <?> "+-"
     ds <- many1 digit   <?> "digit"
     notFollowedBy (char '.')
     -- integer must be in canonical format, with no leading plus sign or leading zero
     return $! (s2b sign `B.append` s2b ds)

t_double :: GenParser ByteString ParseState ByteString
t_double =
  do sign <- sign_parser <?> "+-"
     rest <- try (do { ds <- many1 digit <?> "digit";  char '.'; ds' <- many digit <?> "digit"; e <- t_exponent <?> "exponent"; return (s2b ds `B.snoc` '.' `B.append` s2b ds' `B.append` e) }) <|>
             try (do { char '.'; ds <- many1 digit <?> "digit"; e <- t_exponent <?> "exponent"; return ('.' `B.cons` s2b ds `B.append` e) }) <|>
             try (do { ds <- many1 digit <?> "digit"; e <- t_exponent <?> "exponent"; return (s2b ds `B.append` e) })
     return $! s2b sign `B.append` rest

sign_parser :: GenParser ByteString ParseState String
sign_parser = option "" (oneOf "-+" >>= (\c -> return $! (c:[])))

t_decimal :: GenParser ByteString ParseState ByteString
t_decimal =
  do sign <- sign_parser
     rest <- try (do ds <- many digit <?> "digit"; char '.'; ds' <- option "" (many digit); return (ds ++ ('.':ds')))
             <|> try (do { char '.'; ds <- many1 digit <?> "digit"; return ('.':ds) })
             <|> many1 digit <?> "digit"
     return $ s2b sign `B.append` s2b rest

t_exponent :: GenParser ByteString ParseState ByteString
t_exponent = do e <- oneOf "eE"
                s <- option "" (oneOf "-+" >>= \c -> return (c:[]))
                ds <- many1 digit;
                return $! (e `B.cons` (s2b s `B.append` s2b ds))

t_boolean :: GenParser ByteString ParseState ByteString
t_boolean =
  try ((string "true" >>= return . s2b) <|>
  (string "false" >>= return . s2b))

t_comment :: GenParser ByteString ParseState ()
t_comment =
  char '#' >>
  many (satisfy (\c -> c /= '\x000A' && c /= '\x000D')) >>
  return ()

t_ws  :: GenParser ByteString ParseState ()
t_ws =
    ((try (char '\x0009' <|> char '\x000A' <|> char '\x000D' <|> char '\x0020') >> return ())
    <|> try t_comment)
   <?> "whitespace-or-comment"


t_language  :: GenParser ByteString ParseState ByteString
t_language =
  do init <- many1 lower;
     rest <- many (do {char '-'; cs <- many1 (lower <|> digit); return (s2b ('-':cs))})
     return $! (s2b init `B.append` (B.concat rest))

identifier :: GenParser ByteString ParseState Char -> GenParser ByteString ParseState Char -> GenParser ByteString ParseState ByteString
identifier initial rest = initial >>= \i -> many rest >>= \r -> return (s2b (i:r))

t_prefixName :: GenParser ByteString ParseState ByteString
t_prefixName = identifier t_nameStartCharMinusUnderscore t_nameChar

t_name :: GenParser ByteString ParseState ByteString
t_name = identifier t_nameStartChar t_nameChar

t_uriref :: GenParser ByteString ParseState ByteString
t_uriref = between (char '<') (char '>') t_relativeURI

t_relativeURI  :: GenParser ByteString ParseState ByteString
t_relativeURI =
  do frag <- many t_ucharacter >>= return . B.pack . concat
     bUrl <- currBaseUrl
     dUrl <- currDocUrl
     return $ absolutizeUrl bUrl dUrl frag

-- We make this String rather than ByteString because we want
-- t_relativeURI (the only place it's used) to have chars so that
-- when it creates a ByteString it can all be in one chunk.
t_ucharacter  :: GenParser ByteString ParseState String
t_ucharacter =
  try (unicode_escape >>= return . B.unpack) <|>
  try (string "\\>") <|>
  (non_ctrl_char_except ['>'] >>= return . B.unpack)

t_nameChar :: GenParser ByteString ParseState Char
t_nameChar = t_nameStartChar <|> char '-' <|> char '\x00B7' <|> satisfy f
  where
    f = flip in_range [('0', '9'), ('\x0300', '\x036F'), ('\x203F', '\x2040')]

longString_char  :: GenParser ByteString ParseState ByteString
longString_char  =
  specialChar        <|> -- \r|\n|\t as single char
  try escapedChar    <|> -- an backslash-escaped tab, newline, linefeed, backslash or doublequote
  try twoDoubleQuote <|> -- two doublequotes not followed by a doublequote
  try oneDoubleQuote <|> -- a single doublequote
  safeNonCtrlChar    <|> -- anything but a single backslash or doublequote
  try unicode_escape     -- a unicode escape sequence (\uxxxx or \Uxxxxxxxx)
  where
    specialChar     = oneOf ['\x0009', '\x000A', '\x000D'] >>= bs1
    escapedChar     =
      do char '\\'
         ((char 't'  >> bs1 '\t') <|> (char 'n' >> bs1 '\n') <|> (char 'r' >> bs1 '\r') <|>
          (char '\\' >> bs1 '\\') <|> (char '"' >> bs1 '"'))
    twoDoubleQuote  = string "\"\"" >> notFollowedBy (char '"') >> bs "\"\""
    oneDoubleQuote  = char '"' >> notFollowedBy (char '"') >> bs1 '"'
    safeNonCtrlChar = non_ctrl_char_except ['\\', '"']

bs1 :: Char -> GenParser ByteString ParseState ByteString
bs1 = return . B.singleton

bs :: String -> GenParser ByteString ParseState ByteString
bs = return . B.pack

t_nameStartChar  :: GenParser ByteString ParseState Char
t_nameStartChar = char '_' <|> t_nameStartCharMinusUnderscore

t_nameStartCharMinusUnderscore  :: GenParser ByteString ParseState Char
t_nameStartCharMinusUnderscore = try $ satisfy $ flip in_range blocks
  where
    blocks = [('A', 'Z'), ('a', 'z'), ('\x00C0', '\x00D6'),
              ('\x00D8', '\x00F6'), ('\x00F8', '\x02FF'),
              ('\x0370', '\x037D'), ('\x037F', '\x1FFF'),
              ('\x200C', '\x200D'), ('\x2070', '\x218F'),
              ('\x2C00', '\x2FEF'), ('\x3001', '\xD7FF'),
              ('\xF900', '\xFDCF'), ('\xFDF0', '\xFFFD'),
              ('\x10000', '\xEFFFF')]

t_hex  :: GenParser ByteString ParseState Char
t_hex = (satisfy $! (\c -> (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F'))) <?> "hexadecimal digit"

-- characters used in (non-long) strings; any echaracters except ", or an escaped \"
-- echaracter - #x22 ) | '\"'
t_scharacter  :: GenParser ByteString ParseState ByteString
t_scharacter =
  do (try (string "\\\"") >> return (B.singleton '"'))
     <|> try (do {char '\\';
                  (char 't' >> return (B.singleton '\t')) <|>
                  (char 'n' >> return (B.singleton '\n')) <|>
                  (char 'r' >> return (B.singleton '\r'))}) -- echaracter part 1
     <|> unicode_escape
     <|> (non_ctrl_char_except ['\\', '"'] >>= \s -> return $! s) -- echaracter part 2 minus "

unicode_escape  :: GenParser ByteString ParseState ByteString
unicode_escape =
 (char '\\' >> return (B.singleton '\\')) >>
 ((char '\\' >> return (s2b "\\\\")) <|>
  (char 'u' >> count 4 t_hex >>= \cs -> return $! s2b "\\u" `B.append` s2b cs) <|>
  (char 'U' >> count 8 t_hex >>= \cs -> return $! s2b "\\U" `B.append` s2b cs))

non_ctrl_char_except  :: String -> GenParser ByteString ParseState ByteString
non_ctrl_char_except cs =
  satisfy (\c -> c <= '\x10FFFF' && (c >= '\x0020' && c `notElem` cs)) >>=
  return . B.singleton

{-# INLINE in_range #-}
in_range :: Char -> [(Char, Char)] -> Bool
in_range c = any (\(c1, c2) -> c >= c1 && c <= c2)

-- Resolve a prefix using the given prefix mappings and base URL. If the prefix is
-- empty, then the base URL will be used if there is a base URL and if the map
-- does not contain an entry for the empty prefix.
resolveQName :: Maybe BaseUrl -> ByteString -> PrefixMappings -> ByteString
resolveQName mbaseUrl prefix (PrefixMappings pms') =
  case (mbaseUrl, B.null prefix) of
    (Just (BaseUrl base), True)  ->  Map.findWithDefault base BL.empty pms'
    (Nothing,             True)  ->  err1
    (_,                   _   )  ->  Map.findWithDefault err2 prefix pms'
  where
    err1 = error  "Cannot resolve empty QName prefix to a Base URL."
    err2 = error ("Cannot resolve QName prefix: " ++ B.unpack prefix)

-- Resolve a URL fragment found on the right side of a prefix mapping by converting it to an absolute URL if possible.
absolutizeUrl :: Maybe BaseUrl -> Maybe ByteString -> ByteString -> ByteString
absolutizeUrl mbUrl mdUrl urlFrag =
  case isAbsoluteUri urlFrag of
    True   ->  urlFrag
    False  ->
      case (mbUrl, mdUrl) of
        (Nothing,             Nothing    )  -> urlFrag
        (Just (BaseUrl bUrl), Nothing    )  -> bUrl `B.append` urlFrag
        (Nothing,             (Just dUrl))  -> if isHash urlFrag then dUrl `B.append` urlFrag else urlFrag
        (Just (BaseUrl bUrl), (Just dUrl))  -> (if isHash urlFrag then dUrl else bUrl) `B.append` urlFrag
  where
    isHash bs = B.length bs == 1 && B.head bs == '#'

{-# INLINE isAbsoluteUri #-}
isAbsoluteUri :: ByteString -> Bool
isAbsoluteUri = B.elem ':'

newBaseUrl :: Maybe BaseUrl -> ByteString -> BaseUrl
newBaseUrl Nothing                url = BaseUrl url
newBaseUrl (Just (BaseUrl bUrl)) url = BaseUrl $! mkAbsoluteUrl bUrl url

{-# INLINE mkAbsoluteUrl #-}
-- Make an absolute URL by returning as is if already an absolute URL and otherwise
-- appending the URL to the given base URL.
mkAbsoluteUrl :: ByteString -> ByteString -> ByteString
mkAbsoluteUrl base url =
  case isAbsoluteUri url of
    True  ->  url
    False ->  base `B.append` url

currBaseUrl :: GenParser ByteString ParseState (Maybe BaseUrl)
currBaseUrl = getState >>= \(bUrl, _, _, _, _, _, _, _) -> return bUrl

currDocUrl :: GenParser ByteString ParseState (Maybe ByteString)
currDocUrl = getState >>= \(_, dUrl, _, _, _, _, _, _) -> return dUrl

pushSubj :: Subject -> GenParser ByteString ParseState ()
pushSubj s = getState >>= \(bUrl, dUrl, i, pms, ss, ps, cs, ts) ->
                  setState (bUrl, dUrl, i, pms, (s:ss), ps, cs, ts)

popSubj :: GenParser ByteString ParseState (Subject)
popSubj = getState >>= \(bUrl, dUrl, i, pms, ss, ps, cs, ts) ->
                setState (bUrl, dUrl, i, pms, tail ss, ps, cs, ts) >>
                  when (null ss) (error "Cannot pop subject off empty stack.") >>
                  return (head ss)

pushPred :: Predicate -> GenParser ByteString ParseState ()
pushPred p = getState >>= \(bUrl, dUrl, i, pms, ss, ps, cs, ts) ->
                  setState (bUrl, dUrl, i, pms, ss, (p:ps), cs, ts)

popPred :: GenParser ByteString ParseState (Predicate)
popPred = getState >>= \(bUrl, dUrl, i, pms, ss, ps, cs, ts) ->
                setState (bUrl, dUrl, i, pms, ss, tail ps, cs, ts) >>
                  when (null ps) (error "Cannot pop predicate off empty stack.") >>
                  -- trace (show ps) (return ()) >>
                  return (head ps)

isInColl :: GenParser ByteString ParseState Bool
isInColl = getState >>= \(_, _, _, _, _, _, cs, _) -> return . not . null $ cs

updateBaseUrl :: Maybe (Maybe BaseUrl) -> GenParser ByteString ParseState ()
updateBaseUrl val = _modifyState val no no no no no

-- combines get_current and increment into a single function
nextIdCounter :: GenParser ByteString ParseState Int
nextIdCounter = getState >>= \(bUrl, dUrl, i, pms, s, p, cs, ts) ->
                setState (bUrl, dUrl, i+1, pms, s, p, cs, ts) >> return i

updatePMs :: Maybe PrefixMappings -> GenParser ByteString ParseState ()
updatePMs val = _modifyState no no val no no no

-- Register that we have begun processing a collection
beginColl :: GenParser ByteString ParseState ()
beginColl = getState >>= \(bUrl, dUrl, i, pms, s, p, cs, ts) ->
            setState (bUrl, dUrl, i, pms, s, p, (True:cs), ts)

onCollFirstItem :: GenParser ByteString ParseState Bool
onCollFirstItem = getState >>= \(_, _, _, _, _, _, cs, _) -> return (not (null cs) && head cs)

collFirstItemProcessed :: GenParser ByteString ParseState ()
collFirstItemProcessed = getState >>= \(bUrl, dUrl, i, pms, s, p, (_:cs), ts) ->
                         setState (bUrl, dUrl, i, pms, s, p, (False:cs), ts)

-- Register that a collection is finished being processed; the bool value
-- in the monad is *not* the value that was popped from the stack, but whether
-- we are still processing a parent collection or have finished processing
-- all collections and are no longer in a collection at all.
finishColl :: GenParser ByteString ParseState (Bool)
finishColl = getState >>= \(bUrl, dUrl, i, pms, s, p, cs, ts) ->
             let cs' = drop 1 cs
             in setState (bUrl, dUrl, i, pms, s, p, cs', ts) >> return (not $ null cs')

-- Alias for Nothing for use with _modifyState calls, which can get very long with
-- many Nothing values.
no :: Maybe a
no = Nothing

-- Update the subject and predicate values of the ParseState to Nothing.
resetSubjectPredicate :: GenParser ByteString ParseState ()
resetSubjectPredicate =
  getState >>= \(bUrl, dUrl, n, pms, _, _, cs, ts) ->
  setState (bUrl, dUrl, n, pms, [], [], cs, ts)

-- Modifies the current parser state by updating any state values among the parameters
-- that have non-Nothing values.
_modifyState :: Maybe (Maybe BaseUrl) -> Maybe (Int -> Int) -> Maybe PrefixMappings ->
                Maybe Subject -> Maybe Predicate -> Maybe (Seq Triple) ->
                GenParser ByteString ParseState ()
_modifyState mb_bUrl mb_n mb_pms mb_subj mb_pred mb_trps =
  do (_bUrl, _dUrl, _n, _pms, _s, _p, _cs, _ts) <- getState
     setState (maybe _bUrl id mb_bUrl,
              _dUrl,
              maybe _n (const _n) mb_n,
              maybe _pms id mb_pms,
              maybe _s (\s -> s : _s) mb_subj,
              maybe _p (\p -> p : _p) mb_pred,
              _cs,
              maybe _ts id mb_trps)

addTripleForObject :: Object -> GenParser ByteString ParseState ()
addTripleForObject obj =
  do (bUrl, dUrl, i, pms, ss, ps, cs, ts) <- getState
     when (null ss) $
       error $ "No Subject with which to create triple for: " ++ show obj
     when (null ps) $
       error $ "No Predicate with which to create triple for: " ++ show obj
     setState (bUrl, dUrl, i, pms, ss, ps, cs, ts |> (triple (head ss) (head ps) obj))

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
-- Returns either a @ParseFailure@ or a new graph containing the parsed triples.
parseURL :: Graph gr => 
                 Maybe BaseUrl -- ^ The optional base URI of the document.
                 -> String     -- ^ The document URI (i.e., the URI of the document itself).
                 -> String     -- ^ The location URI from which to retrieve the Turtle document.
                 -> IO (Either ParseFailure gr)
                               -- ^ The parse result, which is either a @ParseFailure@ or the graph
                               --   corresponding to the Turtle document.
parseURL bUrl docUrl locUrl = _parseURL (parseString bUrl docUrl) locUrl

-- |Parse the given file as a Turtle document. The arguments and return type have the same semantics
-- as 'parseURL', except that the last @String@ argument corresponds to a filesystem location rather
-- than a location URI.
--
-- Returns either a @ParseFailure@ or a new graph containing the parsed triples.
parseFile :: Graph gr => Maybe BaseUrl -> String -> String -> IO (Either ParseFailure gr)
parseFile bUrl docUrl fpath =
  B.readFile fpath >>= \bs -> return $ handleResult bUrl (runParser t_turtleDoc initialState docUrl bs)
  where initialState = (bUrl, Just (s2b docUrl), 1, PrefixMappings Map.empty, [], [], [], Seq.empty)

-- |Parse the given string as a Turtle document. The arguments and return type have the same semantics 
-- as <parseURL>, except that the last @String@ argument corresponds to the Turtle document itself as
-- a a string rather than a location URI.
parseString :: Graph gr => Maybe BaseUrl -> String -> ByteString -> Either ParseFailure gr
parseString bUrl docUrl ttlStr = handleResult bUrl (runParser t_turtleDoc initialState "" (ttlStr))
  where initialState = (bUrl, Just (s2b docUrl), 1, PrefixMappings Map.empty, [], [], [], Seq.empty)


handleResult :: Graph gr => Maybe BaseUrl -> Either ParseError (Seq Triple, PrefixMappings) -> Either ParseFailure gr
handleResult bUrl result =
  case result of
    (Left err)         -> Left (ParseFailure $ show err)
    (Right (ts, pms))  -> Right $! mkGraph (F.toList ts) bUrl pms

_testParseState :: ParseState
_testParseState = (Nothing, Nothing, 1, PrefixMappings (Map.empty), [], [], [], Seq.empty)
