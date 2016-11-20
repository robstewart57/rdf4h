-- |An 'RdfParser' implementation for the Turtle format
-- <http://www.w3.org/TeamSubmission/turtle/>.

module Text.RDF.RDF4H.TurtleParser(
  TurtleParser(TurtleParser),
  parseTurtleStringAttoparsec,parseTurtleFileAttoparsec,
  parseTurtleStringParsec,parseTurtleFileParsec
)

where

import Data.Attoparsec.ByteString (parse,IResult(..))
import Data.Char (isLetter,isAlphaNum,toLower,toUpper,isDigit,isHexDigit)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.RDF.Types
import Data.RDF.Namespace
import Text.RDF.RDF4H.ParserUtils
import Text.Parsec (runParser,ParseError)
-- import Text.Parsec.Text (GenParser)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import Data.Sequence(Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Control.Monad

import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.LookAhead
import Control.Applicative
import Control.Monad.State.Class
import Control.Monad.State.Strict

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
   [Bool],           -- when in a collection, is it a subject collection or not
   Bool,           -- when in a blank node property list, is it a subject collection or not
   Seq Triple,       -- the triples encountered while parsing; always added to on the right side
   Map String Int)

-- grammar rule: [1] turtleDoc
-- t_turtleDoc :: (CharParsing m, Monad m) => m (Seq Triple, PrefixMappings)
t_turtleDoc :: (MonadState ParseState m,CharParsing m, LookAheadParsing m) => m (Seq Triple, PrefixMappings)
t_turtleDoc =
  many t_statement *> (eof <?> "eof") *> gets (\(_, _, _, pms, _, _, _, _, _, ts,_) -> (ts, pms))

-- grammar rule: [2] statement
t_statement :: (MonadState ParseState m,CharParsing m, LookAheadParsing m) => m ()
t_statement = d <|> t <|> void (some t_ws <?> "blankline-whitespace")
  where
    d = void
      (try t_directive
      *> (many t_ws <?> "directive-whitespace2"))
    t = void
      (t_triples
      *> (many t_ws <?> "triple-whitespace1")
      *> (char '.' <?> "end-of-triple-period")
      *> (many t_ws <?> "triple-whitespace2"))

-- grammar rule: [6] triples
-- subject predicateObjectList | blankNodePropertyList predicateObjectList?
t_triples :: (MonadState ParseState m,CharParsing m, LookAheadParsing m) => m ()
t_triples =
  try (t_subject *> many t_ws *> t_predicateObjectList *> resetSubjectPredicate)
  <|> (setSubjBlankNodePropList
      *> t_blankNodePropertyList
      *> many t_ws
      *> optional t_predicateObjectList
      *> resetSubjectPredicate
      *> setNotSubjBlankNodePropList)

-- [14]	blankNodePropertyList ::= '[' predicateObjectList ']'
t_blankNodePropertyList :: (MonadState ParseState m,CharParsing m, LookAheadParsing m) => m ()
t_blankNodePropertyList = between (char '[') (char ']') $ do
                            subjPropList <- isSubjPropList
                            blankNode <- BNodeGen <$> nextIdCounter
                            unless subjPropList $ addTripleForObject blankNode
                            pushSubj blankNode
                            many t_ws *> t_predicateObjectList *> void (many t_ws)
                            unless subjPropList $ void popSubj

-- grammar rule: [3] directive
t_directive :: (CharParsing m, MonadState ParseState m) => m ()
t_directive = t_prefixID <|> t_base <|> t_sparql_prefix <|> t_sparql_base

-- grammar rule: [135s] iri
-- IRIREF | PrefixedName
t_iri :: (MonadState ParseState m,CharParsing m, LookAheadParsing m) => m T.Text
t_iri =  try t_iriref <|> t_prefixedName

-- grammar rule: [136s] PrefixedName
t_prefixedName :: (MonadState ParseState m,CharParsing m, LookAheadParsing m) => m T.Text
t_prefixedName = try t_pname_ln <|> try t_pname_ns

-- grammar rule: [4] prefixID
t_prefixID :: (CharParsing m, MonadState ParseState m) => m ()
t_prefixID =
  do void (try (string "@prefix" <?> "@prefix-directive"))
     pre <- (some t_ws <?> "whitespace-after-@prefix") *> option T.empty t_pn_prefix
     void (char ':' *> (some t_ws <?> "whitespace-after-@prefix-colon"))
     uriFrag <- t_iriref
     void (many t_ws <?> "prefixID-whitespace")
     void (char '.' <?> "end-of-prefixID-period")
     (bUrl, dUrl, _, PrefixMappings pms, _, _, _, _, _, _, _) <- get
     updatePMs $ Just (PrefixMappings $ Map.insert pre (absolutizeUrl bUrl dUrl uriFrag) pms)
     pure ()

-- grammar rule: [6s] sparqlPrefix
t_sparql_prefix :: (CharParsing m, MonadState ParseState m) => m ()
t_sparql_prefix =
  do void (try (caseInsensitiveString "PREFIX" <?> "@prefix-directive"))
     pre <- (some t_ws <?> "whitespace-after-@prefix") *> option T.empty t_pn_prefix
     void (char ':' *> (some t_ws <?> "whitespace-after-@prefix-colon"))
     uriFrag <- t_iriref
     (bUrl, dUrl, _, PrefixMappings pms, _, _, _, _, _, _, _) <- get
     updatePMs $ Just (PrefixMappings $ Map.insert pre (absolutizeUrl bUrl dUrl uriFrag) pms)
     pure ()

-- grammar rule: [5] base
t_base :: (CharParsing m, MonadState ParseState m) => m ()
t_base =
  do void (try (string "@base" <?> "@base-directive"))
     void (some t_ws <?> "whitespace-after-@base")
     urlFrag <- t_iriref
     void (many t_ws <?> "base-whitespace")
     void (char '.') <?> "end-of-base-period"
     bUrl <- currBaseUrl
     dUrl <- currDocUrl
     updateBaseUrl (Just $ Just $ newBaseUrl bUrl (absolutizeUrl bUrl dUrl urlFrag))

-- grammar rule: [5s] sparqlBase
t_sparql_base :: (CharParsing m, MonadState ParseState m) => m ()
t_sparql_base =
  do void (try (caseInsensitiveString "BASE" <?> "@sparql-base-directive"))
     void (some t_ws <?> "whitespace-after-BASE")
     urlFrag <- t_iriref
     bUrl <- currBaseUrl
     dUrl <- currDocUrl
     updateBaseUrl (Just $ Just $ newBaseUrl bUrl (absolutizeUrl bUrl dUrl urlFrag))

t_verb :: (MonadState ParseState m,CharParsing m, LookAheadParsing m) => m ()
-- [9]	verb ::= predicate | 'a'
t_verb = (try t_predicate <|> (char 'a' *> pure rdfTypeNode)) >>= pushPred

-- grammar rule: [11] predicate
t_predicate :: (MonadState ParseState m,CharParsing m, LookAheadParsing m) => m Node
t_predicate = UNode <$> (t_iri <?> "resource")

-- grammar rules: [139s] PNAME_NS
t_pname_ns :: (CharParsing m, MonadState ParseState m) => m T.Text
t_pname_ns =do
  pre <- option T.empty (try t_pn_prefix) <* char ':'
  (bUrl, _, _, pms, _, _, _, _, _, _, _) <- get
  case resolveQName bUrl pre pms of
    Just n  -> pure n
    Nothing -> unexpected ("Cannot resolve QName prefix: " ++ T.unpack pre)

-- grammar rules: [168s] PN_LOCAL
-- [168s] PN_LOCAL ::= (PN_CHARS_U | ':' | [0-9] | PLX) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))?
t_pn_local :: (MonadState ParseState m,CharParsing m, LookAheadParsing m) => m T.Text
t_pn_local = do
  x <- t_pn_chars_u_str <|> string ":" <|> satisfy_str <|> t_plx
  xs <- option "" $ try $ do
               let recsve = (t_pn_chars_str <|> string ":" <|> t_plx) <|>
                            (t_pn_chars_str <|> string ":" <|> t_plx <|> try (string "." <* lookAhead (try recsve))) <|>
                            (t_pn_chars_str <|> string ":" <|> t_plx <|> try (string "." *> notFollowedBy t_ws *> pure "."))
               concat <$> many recsve
  pure (T.pack (x ++ xs))
    where
      satisfy_str      = pure <$> satisfy isDigit
      t_pn_chars_str   = pure <$> t_pn_chars
      t_pn_chars_u_str = pure <$> t_pn_chars_u

-- PERCENT | PN_LOCAL_ESC
-- grammar rules: [169s] PLX
t_plx :: (CharParsing m, Monad m) => m String
t_plx = t_percent <|> t_pn_local_esc_str
    where
      t_pn_local_esc_str = pure <$> t_pn_local_esc

--        '%' HEX HEX
-- grammar rules: [170s] PERCENT
t_percent :: (CharParsing m, Monad m) => m String
t_percent = sequence [char '%',t_hex,t_hex]


-- grammar rules: [172s] PN_LOCAL_ESC
t_pn_local_esc :: CharParsing m => m Char
t_pn_local_esc = char '\\' *> oneOf "_~.-!$&'()*+,;=/?#@%"

-- grammar rules: [140s] PNAME_LN
t_pname_ln :: (MonadState ParseState m,CharParsing m, LookAheadParsing m) => m T.Text
t_pname_ln = T.append <$> t_pname_ns <*> t_pn_local

-- grammar rule: [10] subject
-- [10] subject	::= iri | BlankNode | collection
t_subject :: (MonadState ParseState m,CharParsing m, LookAheadParsing m) => m ()
t_subject =
  iri <|>
  (t_blankNode >>= pushSubj) <|>
   (BNodeGen <$> nextIdCounter >>= \x -> pushSubj x
    *> pushPred rdfFirstNode
    *> pushSubjColl
    *> t_collection)
  where
    iri         = unode <$> (try t_iri <?> "subject resource") >>= pushSubj

-- [137s] BlankNode ::= BLANK_NODE_LABEL | ANON
t_blankNode :: (CharParsing m, MonadState ParseState m) => m Node
t_blankNode = do
  genID <- try t_blank_node_label <|> (t_anon *> pure "")
  mp <- currGenIdLookup
  case Map.lookup genID mp of
    Nothing -> do
      i <- nextIdCounter
      let node = BNodeGen i
      addGenIdLookup genID i
      pure node
    Just i ->
      pure $ BNodeGen i

-- TODO replicate the recursion technique from [168s] for ((..)* something)?
-- [141s] BLANK_NODE_LABEL ::= '_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
t_blank_node_label :: (CharParsing m, MonadState ParseState m) => m String
t_blank_node_label = do
  void (string "_:")
  firstChar <- t_pn_chars_u <|> satisfy isDigit
--  optional $ try $ do
  try $ do
    ss <- option "" $ do
            xs <- many (t_pn_chars <|> char '.')
            if null xs
            then pure xs
            else if last xs == '.'
                 then unexpected "'.' at the end of a blank node label"
                 else pure xs
    pure (firstChar : ss)

-- [162s] ANON ::= '[' WS* ']'
t_anon :: CharParsing m => m ()
t_anon = between (char '[') (char ']') (skipMany t_ws)

-- [7] predicateObjectList ::= verb objectList (';' (verb objectList)?)*
t_predicateObjectList :: (MonadState ParseState m,CharParsing m, LookAheadParsing m) => m ()
t_predicateObjectList =
  void (sepEndBy1
        (optional (try (do { t_verb
                           ; some t_ws
                           ; t_objectList
                           ; popPred})))
        (try (many t_ws *> char ';' *> many t_ws)))

-- grammar rule: [8] objectlist
-- [8] objectList ::= object (',' object)*
t_objectList :: (MonadState ParseState m,CharParsing m, LookAheadParsing m) => m ()
t_objectList = -- t_object actually adds the triples
  () <$ ((t_object <?> "object")
     *> many (try (many t_ws *> char ',' *> many t_ws *> t_object)))

-- grammar rule: [12] object
-- [12]	object ::= iri | BlankNode | collection | blankNodePropertyList | literal
t_object :: (MonadState ParseState m,CharParsing m, LookAheadParsing m) => m ()
t_object = do
  inColl <- isInColl
  inSubjColl <- isInSubjColl
  onFirstItem <- onCollFirstItem
  let processObject =
           (UNode <$> t_iri >>= addTripleForObject) <|>
           (try t_blankNode >>= addTripleForObject) <|>
           (try t_collection *> pushObjColl) <|>
           try t_blankNodePropertyList <|>
           (t_literal >>= addTripleForObject)
  case (inColl,inSubjColl,onFirstItem) of
    (False,_,_)    -> processObject
    (True,False,True)  -> BNodeGen <$> nextIdCounter >>= \bSubj -> addTripleForObject bSubj
                          *> pushSubj bSubj *> pushPred rdfFirstNode *> processObject *> collFirstItemProcessed
--    (True,True,True)  -> processObject *> collFirstItemProcessed
    (True,True,True)  -> processObject *> collFirstItemProcessed *> popColl
    (True,_,False) -> BNodeGen <$> nextIdCounter >>= \bSubj -> pushPred rdfRestNode *>
                      addTripleForObject bSubj *> popPred *> popSubj *>
                      pushSubj bSubj *> processObject

-- collection: '(' ws* itemList? ws* ')'
-- itemList:      object (ws+ object)*
-- grammar rule: [15] collection
-- 15]	collection ::= '(' object* ')'
t_collection :: (MonadState ParseState m,CharParsing m, LookAheadParsing m) => m ()
t_collection =
  between (char '(') (char ')') $ do
    beginColl
    try empty_list <|> non_empty_list
    void (many t_ws)
    void finishColl
    -- popColl
      where
        non_empty_list = do
          some (many t_ws *> t_object *> many t_ws)

          _inSubjColl <- isInSubjColl

          popPred
          pushPred rdfRestNode
          addTripleForObject rdfNilNode

          -- popPred
          -- if inSubjColl then trace "is sub" popColl else trace "not sub" $ void popSubj
          -- if inSubjColl then pure () else trace "not sub" $ void popSubj

        empty_list = do
          lookAhead (try (many t_ws *> char ')'))
          addTripleForObject rdfNilNode

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

t_literal :: (MonadState ParseState m,CharParsing m, LookAheadParsing m) => m Node
t_literal =
  LNode <$> try t_rdf_literal                 <|>
  (`mkLNode` xsdDoubleUri)  <$> try t_double  <|>
  (`mkLNode` xsdDecimalUri) <$> try t_decimal <|>
  (`mkLNode` xsdIntUri)     <$> try t_integer <|>
  (`mkLNode` xsdBooleanUri) <$> t_boolean
  where
    mkLNode :: T.Text -> T.Text -> Node
    mkLNode bsType bs' = LNode (typedL bsType bs')

-- [128s] RDFLiteral
-- String (LANGTAG | '^^' iri)?
t_rdf_literal :: (MonadState ParseState m,CharParsing m, LookAheadParsing m) => m LValue
t_rdf_literal = do
  str' <- t_string
  let str = escapeRDFSyntax str'
  option (plainL str) $
                  try (t_langtag >>= \lang -> pure (plainLL str lang)) <|>
                   (count 2 (char '^') *> t_iri >>= \iri -> pure (typedL str iri))

-- [17] String
-- STRING_LITERAL_QUOTE | STRING_LITERAL_SINGLE_QUOTE | STRING_LITERAL_LONG_SINGLE_QUOTE | STRING_LITERAL_LONG_QUOTE
t_string :: (CharParsing m, Monad m) => m T.Text
t_string = try t_string_literal_long_quote <|>
           try t_string_literal_long_single_quote <|>
           try t_string_literal_quote <|>
           t_string_literal_single_quote

-- [22]	STRING_LITERAL_QUOTE
-- '"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'
t_string_literal_quote :: (CharParsing m, Monad m) => m T.Text
t_string_literal_quote =
     between (char '"') (char '"') $
      T.concat <$> many (T.singleton <$> noneOf ['\x22','\x5C','\xA','\xD'] <|>
            t_echar <|>
            t_uchar)

-- [23] STRING_LITERAL_SINGLE_QUOTE
-- "'" ([^#x27#x5C#xA#xD] | ECHAR | UCHAR)* "'"
t_string_literal_single_quote :: (CharParsing m, Monad m) => m T.Text
t_string_literal_single_quote =
    between (char '\'') (char '\'') $
      T.concat <$>
       many (T.singleton <$> noneOf ['\x27','\x5C','\xA','\xD'] <|>
             t_echar <|>
             t_uchar)

-- [24] STRING_LITERAL_LONG_SINGLE_QUOTE
-- "'''" (("'" | "''")? ([^'\] | ECHAR | UCHAR))* "'''"
t_string_literal_long_single_quote :: (CharParsing m, Monad m) => m T.Text
t_string_literal_long_single_quote =
    between (string "'''") (string "'''") $ do
      ss <- many $ try $ do
        s1 <- T.pack <$> option "" (try (string "''") <|> string "'")
        s2 <- T.singleton <$> noneOf ['\'','\\'] <|> t_echar <|> t_uchar
        pure (s1 `T.append` s2)
      pure (T.concat ss)

-- [25] STRING_LITERAL_LONG_QUOTE
-- '"""' (('"' | '""')? ([^"\] | ECHAR | UCHAR))* '"""'
t_string_literal_long_quote :: (CharParsing m, Monad m) => m T.Text
t_string_literal_long_quote =
     between (string "\"\"\"") (string "\"\"\"") $ do
      ss <- many $ try $ do
              s1 <- T.pack <$> option "" (try (string "\"\"") <|> string "\"")
              s2 <- (T.singleton <$> noneOf ['"','\\']) <|> t_echar <|> t_uchar
              pure (s1 `T.append` s2)
      pure (T.concat ss)

-- [144s] LANGTAG
-- '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
t_langtag :: (CharParsing m, Monad m) => m T.Text
t_langtag = do
    ss   <- char '@' *> some (satisfy isLetter)
    rest <- concat <$> many (char '-' *> some (satisfy isAlphaNum) >>= \lang_str -> pure ('-':lang_str))
    pure (T.pack (ss ++ rest))

-- [159s]	ECHAR
-- '\' [tbnrf"'\]
t_echar :: (CharParsing m, Monad m) => m T.Text
t_echar = try $ do
    c2 <- char '\\' *> oneOf ['t','b','n','r','f','"','\'','\\']
    case c2 of
       't'  -> pure $ T.singleton '\t'
       'b'  -> pure $ T.singleton '\b'
       'n'  -> pure $ T.singleton '\n'
       'r'  -> pure $ T.singleton '\r'
       'f'  -> pure $ T.singleton '\f'
       '"'  -> pure $ T.singleton '\"'
       '\'' -> pure $ T.singleton '\''
       '\\' -> pure $ T.singleton '\\'
       _    -> fail "nt_echar: impossible error."

-- [26]	UCHAR
-- '\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
t_uchar :: (CharParsing m, Monad m) => m T.Text
t_uchar =
    (try (string "\\u" *> count 4 hexDigit) >>= \cs -> pure $ T.pack ('\\':'u':cs)) <|>
     (char '\\' *> char 'U' *> count 8 hexDigit >>= \cs -> pure $ T.pack ('\\':'U':cs))

-- [19] INTEGER ::= [+-]? [0-9]+
t_integer :: (CharParsing m, Monad m) => m T.Text
t_integer = try $
  do sign <- sign_parser <?> "+-"
     ds <- some (satisfy isDigit <?> "digit")
     pure $! ( T.pack sign `T.append` T.pack ds)

-- grammar rule: [21] DOUBLE
-- [21] DOUBLE ::= [+-]? ([0-9]+ '.' [0-9]* EXPONENT | '.' [0-9]+ EXPONENT | [0-9]+ EXPONENT)
t_double :: (CharParsing m, Monad m) => m T.Text
t_double =
  do sign <- sign_parser <?> "+-"
     rest <- try (do { ds <- (some (satisfy isDigit) <?> "digit") <* char '.';
                      ds' <- many (satisfy isDigit) <?> "digit";
                      e <- t_exponent <?> "exponent";
                      pure ( T.pack ds `T.snoc` '.' `T.append`  T.pack ds' `T.append` e) }) <|>
             try (do { ds <- char '.' *> some (satisfy isDigit) <?> "digit";
                       e <- t_exponent <?> "exponent";
                       pure ('.' `T.cons`  T.pack ds `T.append` e) }) <|>
                 (do { ds <- some (satisfy isDigit) <?> "digit";
                       e <- t_exponent <?> "exponent";
                       pure ( T.pack ds `T.append` e) })
     pure $! T.pack sign `T.append` rest

sign_parser :: CharParsing m => m String
sign_parser = option "" (pure <$> oneOf "-+")

-- [20]	DECIMAL ::= [+-]? [0-9]* '.' [0-9]+
t_decimal :: (CharParsing m, Monad m) => m T.Text
t_decimal = try $ do
              sign <- sign_parser
              dig1 <- many (satisfy isDigit) <* char '.'
              dig2 <- some (satisfy isDigit)
              pure (T.pack sign `T.append`  T.pack dig1 `T.append` T.pack "." `T.append` T.pack dig2)

t_exponent :: (CharParsing m, Monad m) => m T.Text
t_exponent = do e <- oneOf "eE"
                s <- option "" (pure <$> oneOf "-+")
                ds <- some digit
                pure $! (e `T.cons` ( T.pack s `T.append` T.pack ds))

t_boolean :: CharParsing m => m T.Text
t_boolean =
  T.pack <$> try (string "true" <|> string "false")

t_comment :: CharParsing m => m ()
t_comment =
  void (char '#' *> many (noneOf "\n\r"))

-- [161s] WS ::= #x20 | #x9 | #xD | #xA
t_ws :: CharParsing m => m ()
t_ws =
    (void (try (oneOf "\t\n\r "))) <|> try t_comment
    <?> "whitespace-or-comment"

-- grammar rule: [167s] PN_PREFIX
t_pn_prefix :: (CharParsing m, MonadState ParseState m) => m T.Text
t_pn_prefix = do
  i <- try t_pn_chars_base
  r <- option "" (many (try t_pn_chars <|> char '.')) -- TODO: ensure t_pn_chars is last char
  pure (T.pack (i:r))

-- [18] IRIREF
t_iriref :: (CharParsing m, MonadState ParseState m) => m T.Text
t_iriref =
  between (char '<') (char '>') $ do
    iri <- T.concat <$> many ( T.singleton <$> noneOf (['\x00'..'\x20'] ++ ['<','>','"','{','}','|','^','`','\\']) <|>
                               t_uchar )
    bUrl <- currBaseUrl
    dUrl <- currDocUrl
    let iri' = escapeRDFSyntax iri
    validateURI (absolutizeUrl bUrl dUrl iri')

t_pn_chars :: CharParsing m => m Char
t_pn_chars = t_pn_chars_u <|> char '-' <|> char '\x00B7' <|> satisfy f
  where
    f = flip in_range [('0', '9'), ('\x0300', '\x036F'), ('\x203F', '\x2040')]

-- grammar rule: [163s] PN_CHARS_BASE
t_pn_chars_base :: CharParsing m => m Char
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
t_pn_chars_u :: CharParsing m => m Char
t_pn_chars_u = t_pn_chars_base <|> char '_'

-- grammar rules: [171s] HEX
-- TODO Should this support lowercase hex characters? if so, use Char.isHexDigit
t_hex :: CharParsing m => m Char
-- t_hex = satisfy (\c -> isDigit c || (c >= 'A' && c <= 'F')) <?> "hexadecimal digit"
t_hex = satisfy isHexDigit <?> "hexadecimal digit"

{-# INLINE in_range #-}
in_range :: Char -> [(Char, Char)] -> Bool
in_range c = any (\(c1, c2) -> c >= c1 && c <= c2)

newBaseUrl :: Maybe BaseUrl -> T.Text -> BaseUrl
newBaseUrl Nothing               url = BaseUrl url
newBaseUrl (Just (BaseUrl bUrl)) url = BaseUrl $! mkAbsoluteUrl bUrl url

currGenIdLookup :: MonadState ParseState m => m (Map String Int)
currGenIdLookup = gets $ \(_, _, _, _, _, _, _, _, _, _,genMap) -> genMap

addGenIdLookup :: MonadState ParseState m => String -> Int -> m ()
addGenIdLookup genId counter =
  modify $ \(bUrl, dUrl, i, pms, ss, ps, cs, subjC, subjBNodeList, ts, genMap) ->
            (bUrl, dUrl, i, pms, ss, ps, cs, subjC, subjBNodeList, ts, Map.insert genId counter genMap)

currBaseUrl :: MonadState ParseState m => m (Maybe BaseUrl)
currBaseUrl = gets $ \(bUrl, _, _, _, _, _, _, _, _, _,_) -> bUrl

currDocUrl :: MonadState ParseState m => m (Maybe T.Text)
currDocUrl = gets $ \(_, dUrl, _, _, _, _, _, _, _, _,_) -> dUrl

pushSubj :: MonadState ParseState m => Subject -> m ()
pushSubj s = modify $ \(bUrl, dUrl, i, pms, ss, ps, cs, subjC, subjBNodeList, ts, genMap) ->
                       (bUrl, dUrl, i, pms, s:ss, ps, cs, subjC, subjBNodeList, ts, genMap)

popSubj :: (CharParsing m, MonadState ParseState m) => m Subject
popSubj = get >>= \(bUrl, dUrl, i, pms, ss, ps, cs, subjC, subjBNodeList, ts, genMap) ->
                put (bUrl, dUrl, i, pms, tail ss, ps, cs, subjC, subjBNodeList, ts, genMap)
                  *> when (null ss) (fail "Cannot pop subject off empty stack.")
                  *> pure (head ss)

pushPred :: MonadState ParseState m => Predicate -> m ()
pushPred p = modify $ \(bUrl, dUrl, i, pms, ss, ps, cs, subjC, subjBNodeList, ts, genMap) ->
                       (bUrl, dUrl, i, pms, ss, p:ps, cs, subjC, subjBNodeList, ts, genMap)

popPred :: MonadState ParseState m => m Predicate
popPred = get >>= \(bUrl, dUrl, i, pms, ss, ps, cs, subjC, subjBNodeList, ts, genMap) ->
                put (bUrl, dUrl, i, pms, ss, tail ps, cs, subjC, subjBNodeList, ts, genMap)
                  *> when (null ps) (fail "Cannot pop predicate off empty stack.")
                  *> pure (head ps)

isInColl :: MonadState ParseState m => m Bool
isInColl = gets $ \(_, _, _, _, _, _, cs, _, _, _, _) -> not . null $ cs

isInSubjColl :: MonadState ParseState m => m Bool
isInSubjColl = gets $ \(_, _, _, _, _, _, _, xs, _, _, _) ->
                   if null xs then False else (head xs)

{-
isInObjColl :: (CharParsing m, MonadState ParseState m) => m Bool
isInObjColl = get >>= \(_, _, _, _, _, _, _, xs, _, _) -> do
               when (null xs) $ error "null in isInObjColl"
               pure (not (head xs))
-}

pushSubjColl :: MonadState ParseState m => m ()
pushSubjColl = modify $ \(bUrl, dUrl, i, pms, s, p, cs, subjC, subjBNodeList, ts, genMap) ->
                         (bUrl, dUrl, i, pms, s, p, cs, True:subjC, subjBNodeList, ts, genMap)

popColl :: (CharParsing m, MonadState ParseState m) => m ()
popColl = get >>= \(bUrl, dUrl, i, pms, s, p, cs, subjC, subjBNodeList, ts, genMap) -> do
                when (null subjC) $ fail "null in popColl"
                put (bUrl, dUrl, i, pms, s, p, cs, tail subjC, subjBNodeList, ts, genMap)

pushObjColl :: MonadState ParseState m => m ()
pushObjColl = modify $ \(bUrl, dUrl, i, pms, s, p, cs, subjC, subjBNodeList, ts,genMap) ->
                        (bUrl, dUrl, i, pms, s, p, cs, False:subjC, subjBNodeList, ts,genMap)

isSubjPropList :: MonadState ParseState m => m Bool
isSubjPropList = gets $ \(_, _, _, _, _, _, _, _, subjBNodeList, _,_) -> subjBNodeList

{-
isObjPropList :: (CharParsing m, MonadState ParseState m) => m Bool
isObjPropList = get >>= \(_, _, _, _, _, _, _, _, subjBNodeList, _) -> do
                pure subjBNodeList
-}

setSubjBlankNodePropList :: MonadState ParseState m => m ()
setSubjBlankNodePropList =
  modify $ \(bUrl, dUrl, i, pms, s, p, cs, subjC, _, ts,genMap) ->
            (bUrl, dUrl, i, pms, s, p, cs, subjC, True, ts,genMap)

setNotSubjBlankNodePropList :: MonadState ParseState m => m ()
setNotSubjBlankNodePropList =
  modify $ \(bUrl, dUrl, i, pms, s, p, cs, subjC, _, ts,genMap) ->
            (bUrl, dUrl, i, pms, s, p, cs, subjC, True, ts,genMap)

-- setObjBlankNodePropList :: (CharParsing m, Monad m) => m ()
-- setObjBlankNodePropList = get >>= \(bUrl, dUrl, i, pms, s, p, cs, subjC, _, ts) ->
--                  put (bUrl, dUrl, i, pms, s, p, cs, subjC, False, ts)

-- popBlankNodePropList :: (CharParsing m, Monad m) => m ()
-- popBlankNodePropList = get >>= \(bUrl, dUrl, i, pms, s, p, cs, subjC, _:subjBNodeList, ts) ->
--                  when (null subjBNodeList) $ "no subj/obj flag to pop when exiting collection"
--                  put (bUrl, dUrl, i, pms, s, p, cs, subjC, subjBNodeList, ts)

updateBaseUrl :: MonadState ParseState m => Maybe (Maybe BaseUrl) -> m ()
updateBaseUrl val = _modifyState val no no no no no

-- combines get_current and increment into a single function
nextIdCounter :: MonadState ParseState m => m Int
nextIdCounter = get >>= \(bUrl, dUrl, i, pms, s, p, cs, subjC, subjBNodeList, ts,genMap) ->
                put (bUrl, dUrl, i+1, pms, s, p, cs, subjC, subjBNodeList, ts,genMap) *> pure i

updatePMs :: MonadState ParseState m => Maybe PrefixMappings -> m ()
updatePMs val = _modifyState no no val no no no

-- Register that we have begun processing a collection
beginColl :: MonadState ParseState m => m ()
beginColl = modify $ \(bUrl, dUrl, i, pms, s, p, cs, subjC, subjBNodeList, ts,genMap) ->
                      (bUrl, dUrl, i, pms, s, p, True:cs, subjC, subjBNodeList, ts,genMap)

onCollFirstItem :: MonadState ParseState m => m Bool
onCollFirstItem = gets $ \(_, _, _, _, _, _, cs, _, _, _,_) -> (not (null cs) && head cs)

collFirstItemProcessed :: MonadState ParseState m => m ()
collFirstItemProcessed =
  modify $ \(bUrl, dUrl, i, pms, s, p, _:cs, subjC, subjBNodeList, ts,genMap) ->
            (bUrl, dUrl, i, pms, s, p, False:cs, subjC, subjBNodeList, ts,genMap)

-- Register that a collection is finished being processed; the bool value
-- in the monad is *not* the value that was popped from the stack, but whether
-- we are still processing a parent collection or have finished processing
-- all collections and are no longer in a collection at all.
finishColl :: MonadState ParseState m => m Bool
finishColl = get >>= \(bUrl, dUrl, i, pms, s, p, cs, subjC, subjBNodeList, ts,genMap) ->
             let cs' = drop 1 cs
             in put (bUrl, dUrl, i, pms, s, p, cs', subjC, subjBNodeList, ts,genMap) *> pure (not $ null cs')

-- Alias for Nothing for use with _modifyState calls, which can get very long with
-- many Nothing values.
no :: Maybe a
no = Nothing

-- Update the subject and predicate values of the ParseState to Nothing.
resetSubjectPredicate :: MonadState ParseState m => m ()
resetSubjectPredicate =
  modify $ \(bUrl, dUrl, n, pms, _, _, cs, subjC, subjBNodeList, ts,genMap) ->
            (bUrl, dUrl, n, pms, [], [], cs, subjC, subjBNodeList, ts,genMap)

-- Modifies the current parser state by updating any state values among the parameters
-- that have non-Nothing values.
_modifyState :: MonadState ParseState m =>
                Maybe (Maybe BaseUrl) -> Maybe (Int -> Int) -> Maybe PrefixMappings ->
                Maybe Subject -> Maybe Predicate -> Maybe (Seq Triple) ->
                m ()
_modifyState mb_bUrl mb_n mb_pms mb_subj mb_pred mb_trps =
  do (_bUrl, _dUrl, _n, _pms, _s, _p, _cs, _subjC, _subjBNodeList, _ts,genMap) <- get
     put (fromMaybe _bUrl mb_bUrl,
              _dUrl,
              maybe _n (const _n) mb_n,
              fromMaybe _pms mb_pms,
              maybe _s (: _s) mb_subj,
              maybe _p (: _p) mb_pred,
              _cs,
              _subjC,
              _subjBNodeList,
              fromMaybe _ts mb_trps,genMap)

addTripleForObject :: (CharParsing m, MonadState ParseState m) => Object -> m ()
addTripleForObject obj =
  do (bUrl, dUrl, i, pms, ss, ps, cs, subjC, subjBNodeList, ts,genMap) <- get
     when (null ss) $
       unexpected $ "No Subject with which to create triple for: " ++ show obj
     when (null ps) $
       unexpected $ "No Predicate with which to create triple for: " ++ show obj
     put (bUrl, dUrl, i, pms, ss, ps, cs, subjC, subjBNodeList, ts |> Triple (head ss) (head ps) obj,genMap)

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
parseURL' :: (Rdf a) =>
                 Maybe BaseUrl       -- ^ The optional base URI of the document.
                 -> Maybe T.Text     -- ^ The document URI (i.e., the URI of the document itself); if Nothing, use location URI.
                 -> String           -- ^ The location URI from which to retrieve the Turtle document.
                 -> IO (Either ParseFailure (RDF a))
                                     -- ^ The parse result, which is either a @ParseFailure@ or the RDF
                                     --   corresponding to the Turtle document.
parseURL' bUrl docUrl = _parseURL (parseString' bUrl docUrl)

-- |Parse the given file as a Turtle document. The arguments and return type have the same semantics
-- as 'parseURL', except that the last @String@ argument corresponds to a filesystem location rather
-- than a location URI.
--
-- Returns either a @ParseFailure@ or a new RDF containing the parsed triples.
parseFile' :: (Rdf a) => Maybe BaseUrl -> Maybe T.Text -> String -> IO (Either ParseFailure (RDF a))
parseFile' bUrl docUrl fpath =
  TIO.readFile fpath >>= \bs' -> pure $ handleResult bUrl (runParser (evalStateT t_turtleDoc initialState) () (maybe "" T.unpack docUrl) bs')
  where initialState = (bUrl, docUrl, 1, PrefixMappings Map.empty, [], [], [], [], False, Seq.empty,Map.empty)

-- |Parse the given string as a Turtle document. The arguments and return type have the same semantics
-- as <parseURL>, except that the last @String@ argument corresponds to the Turtle document itself as
-- a string rather than a location URI.
parseString' :: (Rdf a) => Maybe BaseUrl -> Maybe T.Text -> T.Text -> Either ParseFailure (RDF a)
parseString' bUrl docUrl ttlStr = handleResult bUrl (runParser (evalStateT t_turtleDoc initialState) () "" ttlStr)
  where initialState = (bUrl, docUrl, 1, PrefixMappings Map.empty, [], [], [], [], False, Seq.empty,Map.empty)

handleResult :: Rdf a => Maybe BaseUrl -> Either ParseError (Seq Triple, PrefixMappings) -> Either ParseFailure (RDF a)
handleResult bUrl result =
  case result of
    (Left err)         -> Left (ParseFailure $ show err)
    (Right (ts, pms))  -> Right $! mkRdf (F.toList ts) bUrl pms

validateUNode :: CharParsing m => T.Text -> m Node
validateUNode t =
    case unodeValidate t of
      Nothing        -> unexpected ("Invalid URI in Turtle parser URI validation: " ++ show t)
      Just u@UNode{} -> pure u
      Just node      -> unexpected ("Unexpected node in Turtle parser URI validation: " ++ show node)

validateURI :: (CharParsing m, Monad m) => T.Text -> m T.Text
validateURI t = do
    UNode uri <- validateUNode t
    pure uri

---------------------------------
-- for benchmarking purposes only, exposed temporarily

parseTurtleStringAttoparsec :: (Rdf a) => Maybe BaseUrl -> Maybe T.Text -> T.Text -> Either ParseFailure (RDF a)
parseTurtleStringAttoparsec bUrl docUrl bs = handleResult' $ parse (evalStateT t_turtleDoc initialState) (T.encodeUtf8 bs)
  where
    handleResult' res = case res of
        Fail _ _ err -> error err
        Partial f -> handleResult' (f (T.encodeUtf8 T.empty))
        Done _ (ts,pms) -> Right $! mkRdf (F.toList ts) bUrl pms

    initialState = (bUrl, docUrl, 1, PrefixMappings Map.empty, [], [], [], [], False, Seq.empty,Map.empty)

parseTurtleFileAttoparsec :: (Rdf a) => Maybe BaseUrl -> Maybe T.Text -> String -> IO (Either ParseFailure (RDF a))
parseTurtleFileAttoparsec bUrl docUrl path = parseTurtleStringAttoparsec bUrl docUrl <$> TIO.readFile path

parseTurtleStringParsec :: (Rdf a) => Maybe BaseUrl -> Maybe T.Text -> T.Text -> Either ParseFailure (RDF a)
parseTurtleStringParsec = parseString'
parseTurtleFileParsec :: (Rdf a) => Maybe BaseUrl -> Maybe T.Text -> String -> IO (Either ParseFailure (RDF a))
parseTurtleFileParsec = parseFile'

-- end of benchmarks
---------------------------------


--------------
-- auxiliary parsing functions

-- Match the lowercase or uppercase form of 'c'
caseInsensitiveChar :: CharParsing m => Char -> m Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

-- Match the string 's', accepting either lowercase or uppercase form of each character
caseInsensitiveString :: (CharParsing m, Monad m) => String -> m String
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""
