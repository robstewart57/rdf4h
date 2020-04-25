{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | An 'RdfParser' implementation for the Turtle format
--  <http://www.w3.org/TeamSubmission/turtle/>.
module Text.RDF.RDF4H.TurtleParser
  ( TurtleParser (TurtleParser),
    TurtleParserCustom (TurtleParserCustom),
    parseTurtleDebug,
  )
where

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.State.Class
import Control.Monad.State.Strict
import Data.Attoparsec.Text (IResult (..), parse)
import Data.Char (isDigit, isHexDigit, toLower, toUpper)
import Data.Either
import qualified Data.Foldable as F
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.RDF.Graph.TList
import Data.RDF.IRI
import Data.RDF.Types
#if MIN_VERSION_base(4,9,0)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#else
#endif
#else
#endif
import Data.Sequence (Seq, (|>))
import qualified Data.Text as T
import Text.Parsec (ParseError, runParser)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.RDF.RDF4H.NTriplesParser
import Text.RDF.RDF4H.ParserUtils
import Prelude hiding (readFile)

-- | An 'RdfParser' implementation for parsing RDF in the
--  Turtle format. It is an implementation of W3C Turtle grammar rules at
--  http://www.w3.org/TR/turtle/#sec-grammar-grammar .
--  It takes optional arguments representing the base URL to use
--  for resolving relative URLs in the document (may be overridden in the document
--  itself using the \@base directive), and the URL to use for the document itself
--  for resolving references to <> in the document.
--  To use this parser, pass a 'TurtleParser' value as the first argument to any of
--  the 'parseString', 'parseFile', or 'parseURL' methods of the 'RdfParser' type
--  class.
data TurtleParser = TurtleParser (Maybe BaseUrl) (Maybe T.Text)

data TurtleParserCustom = TurtleParserCustom (Maybe BaseUrl) (Maybe T.Text) Parser

-- | 'TurtleParser' is an instance of 'RdfParser' using a parsec based parser.
instance RdfParser TurtleParser where
  parseString (TurtleParser bUrl dUrl) = parseStringParsec bUrl dUrl
  parseFile (TurtleParser bUrl dUrl) = parseFileParsec bUrl dUrl
  parseURL (TurtleParser bUrl dUrl) = parseURLParsec bUrl dUrl

-- | 'TurtleParser' is an instance of 'RdfParser' using either a
--  parsec or an attoparsec based parser.
instance RdfParser TurtleParserCustom where
  parseString (TurtleParserCustom bUrl dUrl Parsec) = parseStringParsec bUrl dUrl
  parseString (TurtleParserCustom bUrl dUrl Attoparsec) = parseStringAttoparsec bUrl dUrl
  parseFile (TurtleParserCustom bUrl dUrl Parsec) = parseFileParsec bUrl dUrl
  parseFile (TurtleParserCustom bUrl dUrl Attoparsec) = parseFileAttoparsec bUrl dUrl
  parseURL (TurtleParserCustom bUrl dUrl Parsec) = parseURLParsec bUrl dUrl
  parseURL (TurtleParserCustom bUrl dUrl Attoparsec) = parseURLAttoparsec bUrl dUrl

type ParseState =
  ( Maybe BaseUrl, -- the current BaseUrl, may be Nothing initially, but not after it is once set
    Maybe T.Text, -- the docUrl, which never changes and is used to resolve <> in the document.
    Integer, -- the id counter, containing the value of the next id to be used
    PrefixMappings, -- the mappings from prefix to URI that are encountered while parsing
    Maybe Subject, -- current subject node, if we have parsed a subject but not finished the triple
    Maybe Predicate, -- current predicate node, if we have parsed a predicate but not finished the triple
    Seq Triple, -- the triples encountered while parsing; always added to on the right side
    Map String Integer -- map blank node names to generated id.
  )

parseTurtleDebug :: String -> IO (RDF TList)
parseTurtleDebug f = fromRight empty <$> parseFile (TurtleParserCustom (Just . BaseUrl $ "http://base-url.com/") (Just "http://doc-url.com/") Attoparsec) f

-- grammar rule: [1] turtleDoc
t_turtleDoc :: (MonadState ParseState m, CharParsing m, LookAheadParsing m) => m (Seq Triple, Maybe BaseUrl, PrefixMappings)
t_turtleDoc =
  many t_statement *> (eof <?> "eof") *> gets (\(mb_bUrl, _, _, pms, _, _, ts, _) -> (ts, mb_bUrl, pms))

-- grammar rule: [2] statement
-- [2] statement ::= directive | triples '.'
t_statement :: (MonadState ParseState m, CharParsing m, LookAheadParsing m) => m ()
t_statement = directive <|> triples <|> void (some t_ws <?> "blankline-whitespace")
  where
    directive =
      void
        ( try t_directive
            *> (many t_ws <?> "directive-whitespace2")
        )
    triples =
      void
        ( try t_triples
            *> (many t_ws <?> "triple-whitespace1")
            *> (char '.' <?> "end-of-triple-period")
            *> (many t_ws <?> "triple-whitespace2")
        )

-- grammar rule: [6] triples
-- subject predicateObjectList | blankNodePropertyList predicateObjectList?
t_triples :: (MonadState ParseState m, CharParsing m, LookAheadParsing m) => m ()
t_triples = try subjectWithPOL <|> blankNodePropertyListWithPOL
  where
    subjectWithPOL = t_subject *> many t_ws *> t_predicateObjectList *> resetSubjectPredicate
    blankNodePropertyListWithPOL = t_blankNodePropertyList >>= \bn ->
      many t_ws
        *> setSubjectPredicate (Just bn) Nothing
        *> optional t_predicateObjectList
        *> resetSubjectPredicate

-- [14]	blankNodePropertyList ::= '[' predicateObjectList ']'
t_blankNodePropertyList :: (MonadState ParseState m, CharParsing m, LookAheadParsing m) => m Node
t_blankNodePropertyList = withConstantSubjectPredicate
  $ between (char '[') (char ']')
  $ do
    bn <- nextBlankNode
    setSubjectPredicate (Just bn) Nothing
    void (many t_ws *> t_predicateObjectList *> many t_ws)
    return bn

-- grammar rule: [3] directive
t_directive :: (CharParsing m, MonadState ParseState m) => m ()
t_directive = t_prefixID <|> t_base <|> t_sparql_prefix <|> t_sparql_base

-- grammar rule: [135s] iri ::= IRIREF | PrefixedName
t_iri :: (MonadState ParseState m, CharParsing m, LookAheadParsing m) => m T.Text
t_iri = try t_iriref <|> t_prefixedName

-- grammar rule: [136s] PrefixedName ::= PNAME_LN | PNAME_NS
t_prefixedName :: (MonadState ParseState m, CharParsing m, LookAheadParsing m) => m T.Text
t_prefixedName = try t_pname_ln <|> t_pname_ns

-- grammar rule: [4] prefixID ::= '@prefix' PNAME_NS IRIREF '.'
t_prefixID :: (CharParsing m, MonadState ParseState m) => m ()
t_prefixID = do
  void (try (string "@prefix" <?> "@prefix-directive"))
  void (some t_ws <?> "whitespace-after-@prefix")
  pre <- option mempty (try t_pn_prefix) <* char ':'
  void (some t_ws <?> "whitespace-after-@prefix-colon")
  iriFrag <- t_iriref
  void (many t_ws <?> "prefixID-whitespace")
  void (char '.' <?> "end-of-prefixID-period")
  (bUrl, dUrl, _, PrefixMappings pms, _, _, _, _) <- get
  iri <- tryIriResolution bUrl dUrl iriFrag
  updatePMs $ Just (PrefixMappings $ Map.insert pre iri pms)

-- grammar rule: [6s] sparqlPrefix ::= "PREFIX" PNAME_NS IRIREF
t_sparql_prefix :: (CharParsing m, MonadState ParseState m) => m ()
t_sparql_prefix = do
  void (try (caseInsensitiveString "PREFIX" <?> "@prefix-directive"))
  void (some t_ws <?> "whitespace-after-PREFIX")
  pre <- option mempty (try t_pn_prefix) <* char ':'
  void (some t_ws <?> "whitespace-after-PREFIX-colon")
  iriFrag <- t_iriref
  (bUrl, dUrl, _, PrefixMappings pms, _, _, _, _) <- get
  iri <- tryIriResolution bUrl dUrl iriFrag
  updatePMs $ Just (PrefixMappings $ Map.insert pre iri pms)

-- grammar rule: [5] base ::= '@base' IRIREF '.'
t_base :: (CharParsing m, MonadState ParseState m) => m ()
t_base = do
  void (try (string "@base" <?> "@base-directive"))
  void (some t_ws <?> "whitespace-after-@base")
  iriFrag <- t_iriref
  void (many t_ws <?> "base-whitespace")
  void (char '.') <?> "end-of-base-period"
  bUrl <- currBaseUrl
  dUrl <- currDocUrl
  newBaseIri <- BaseUrl <$> tryIriResolution bUrl dUrl iriFrag
  updateBaseUrl (Just $ Just newBaseIri)

-- grammar rule: [5s] sparqlBase ::= "BASE" IRIREF
t_sparql_base :: (CharParsing m, MonadState ParseState m) => m ()
t_sparql_base = do
  void (try (caseInsensitiveString "BASE" <?> "@sparql-base-directive"))
  void (some t_ws <?> "whitespace-after-BASE")
  iriFrag <- t_iriref
  bUrl <- currBaseUrl
  dUrl <- currDocUrl
  newBaseIri <- BaseUrl <$> tryIriResolution bUrl dUrl iriFrag
  updateBaseUrl (Just $ Just newBaseIri)

t_verb :: (MonadState ParseState m, CharParsing m, LookAheadParsing m) => m ()
t_verb = try t_predicate <|> (char 'a' $> rdfTypeNode) >>= setPredicate

-- grammar rule: [11] predicate ::= iri
t_predicate :: (MonadState ParseState m, CharParsing m, LookAheadParsing m) => m Node
t_predicate = UNode <$> (t_iri <?> "resource")

-- grammar rules: [139s] PNAME_NS ::= PN_PREFIX? ':'
t_pname_ns :: (CharParsing m, MonadState ParseState m) => m T.Text
t_pname_ns = do
  pre <- option mempty (try t_pn_prefix) <* char ':'
  (_, _, _, pms, _, _, _, _) <- get
  case resolveQName pre pms of
    Just n -> pure n
    Nothing -> unexpected ("Cannot resolve QName prefix: " <> T.unpack pre)

-- grammar rules: [168s] PN_LOCAL
-- [168s] PN_LOCAL ::= (PN_CHARS_U | ':' | [0-9] | PLX) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))?
t_pn_local :: (MonadState ParseState m, CharParsing m, LookAheadParsing m) => m T.Text
t_pn_local = do
  x <- t_pn_chars_u_str <|> string ":" <|> satisfy_str <|> t_plx
  xs <- option "" $ try $ do
    let recsve =
          (t_pn_chars_str <|> string ":" <|> t_plx)
            <|> (t_pn_chars_str <|> string ":" <|> t_plx <|> try (string "." <* lookAhead (try recsve)))
            <|> (t_pn_chars_str <|> string ":" <|> t_plx <|> try (string "." *> notFollowedBy t_ws $> "."))
    concat <$> many recsve
  pure (T.pack (x <> xs))
  where
    satisfy_str = pure <$> satisfy isDigit
    t_pn_chars_str = pure <$> t_pn_chars
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
t_percent = sequence [char '%', t_hex, t_hex]

-- grammar rules: [172s] PN_LOCAL_ESC
t_pn_local_esc :: CharParsing m => m Char
t_pn_local_esc = char '\\' *> oneOf "_~.-!$&'()*+,;=/?#@%"

-- grammar rules: [140s] PNAME_LN ::= PNAME_NS PN_LOCAL
t_pname_ln :: (MonadState ParseState m, CharParsing m, LookAheadParsing m) => m T.Text
t_pname_ln = T.append <$> t_pname_ns <*> t_pn_local

-- grammar rule: [10] subject
-- [10] subject	::= iri | BlankNode | collection
t_subject :: (MonadState ParseState m, CharParsing m, LookAheadParsing m) => m ()
t_subject = iri <|> t_blankNode <|> t_collection >>= setSubject
  where
    iri = unode <$> (try t_iri <?> "subject resource")

-- [137s] BlankNode ::= BLANK_NODE_LABEL | ANON
t_blankNode :: (CharParsing m, MonadState ParseState m) => m Node
t_blankNode = do
  genID <- try t_blank_node_label <|> (t_anon $> mempty)
  mp <- currGenIdLookup
  maybe (newBN genID) getExistingBN (Map.lookup genID mp)
  where
    newBN genID = do
      i <- nextIdCounter
      when (genID /= mempty) (addGenIdLookup genID i)
      return $ BNodeGen (fromIntegral i)
    getExistingBN = return . BNodeGen . fromIntegral

-- TODO replicate the recursion technique from [168s] for ((..)* something)?
-- [141s] BLANK_NODE_LABEL ::= '_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
t_blank_node_label :: (CharParsing m, MonadState ParseState m) => m String
t_blank_node_label = do
  void (string "_:")
  firstChar <- t_pn_chars_u <|> satisfy isDigit
  try $ (firstChar :) <$> otherChars
  where
    otherChars = option "" $ do
      xs <- many (t_pn_chars <|> char '.')
      if null xs
        then pure xs
        else
          if last xs == '.'
            then unexpected "'.' at the end of a blank node label"
            else pure xs

-- [162s] ANON ::= '[' WS* ']'
t_anon :: CharParsing m => m ()
t_anon = void (between (char '[') (char ']') (many t_ws))

-- [7] predicateObjectList ::= verb objectList (';' (verb objectList)?)*
t_predicateObjectList :: (MonadState ParseState m, CharParsing m, LookAheadParsing m) => m ()
t_predicateObjectList = void $ sepEndBy1 (try verbObjectList) (try separator)
  where
    verbObjectList = t_verb *> some t_ws *> t_objectList
    separator = some (many t_ws *> char ';' *> many t_ws)

-- grammar rule: [8] objectlist
-- [8] objectList ::= object (',' object)*
t_objectList :: (MonadState ParseState m, CharParsing m, LookAheadParsing m) => m ()
t_objectList = do
  (t_object <?> "object") >>= addTripleForObject
  void $ many (try (many t_ws *> char ',' *> many t_ws *> t_object >>= addTripleForObject))

-- grammar rule: [12] object
-- [12]	object ::= iri | BlankNode | collection | blankNodePropertyList | literal
t_object :: (MonadState ParseState m, CharParsing m, LookAheadParsing m) => m Node
t_object =
  try (UNode <$> t_iri)
    <|> try t_blankNode
    <|> try t_collection
    <|> try t_blankNodePropertyList
    <|> t_literal

-- grammar rule: [15] collection
-- [15]	collection ::= '(' object* ')'
t_collection :: (MonadState ParseState m, CharParsing m, LookAheadParsing m) => m Node
t_collection = withConstantSubjectPredicate
  $ between (char '(') (char ')')
  $ do
    void (many t_ws)
    root <- try empty_list <|> non_empty_list
    void (many t_ws)
    return root
  where
    empty_list = lookAhead (char ')') $> rdfNilNode
    non_empty_list = do
      ns <- sepEndBy1 element (some t_ws)
      addTripleForObject rdfNilNode
      return (head ns)
    element = do
      o <- t_object
      bn <- nextBlankNode
      s <- getSubject
      when (isJust s) (addTripleForObject bn)
      setSubjectPredicate (Just bn) (Just rdfFirstNode)
      addTripleForObject o
      setPredicate rdfRestNode
      return bn
    getSubject = get >>= \(_, _, _, _, s, _, _, _) -> pure s

t_literal :: (MonadState ParseState m, CharParsing m, LookAheadParsing m) => m Node
t_literal =
  LNode <$> try t_rdf_literal
    <|> (`mkLNode` xsdDoubleUri) <$> try t_double
    <|> (`mkLNode` xsdDecimalUri) <$> try t_decimal
    <|> (`mkLNode` xsdIntUri) <$> try t_integer
    <|> (`mkLNode` xsdBooleanUri) <$> t_boolean
  where
    mkLNode :: T.Text -> T.Text -> Node
    mkLNode bsType bs' = LNode (typedL bsType bs')

-- [128s] RDFLiteral
-- String (LANGTAG | '^^' iri)?
t_rdf_literal :: (MonadState ParseState m, CharParsing m, LookAheadParsing m) => m LValue
t_rdf_literal = do
  str <- t_string
  option (plainL str) (langTag str <|> typeIRI str)
  where
    langTag str = plainLL str <$> try t_langtag
    typeIRI str = typedL str <$> try (count 2 (char '^') *> t_iri)

-- [17] String
-- STRING_LITERAL_QUOTE | STRING_LITERAL_SINGLE_QUOTE | STRING_LITERAL_LONG_SINGLE_QUOTE | STRING_LITERAL_LONG_QUOTE
t_string :: (CharParsing m, Monad m) => m T.Text
t_string =
  try t_string_literal_long_double_quote
    <|> try t_string_literal_long_single_quote
    <|> try t_string_literal_double_quote
    <|> t_string_literal_single_quote

-- [22]	STRING_LITERAL_QUOTE
-- '"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'
t_string_literal_double_quote :: (CharParsing m, Monad m) => m T.Text
t_string_literal_double_quote = nt_string_literal_quote

-- [23] STRING_LITERAL_SINGLE_QUOTE
-- "'" ([^#x27#x5C#xA#xD] | ECHAR | UCHAR)* "'"
t_string_literal_single_quote :: (CharParsing m, Monad m) => m T.Text
t_string_literal_single_quote = string_literal_quote '\''

-- [24] STRING_LITERAL_LONG_SINGLE_QUOTE
-- "'''" (("'" | "''")? ([^'\] | ECHAR | UCHAR))* "'''"
t_string_literal_long_single_quote :: (CharParsing m, Monad m) => m T.Text
t_string_literal_long_single_quote = between (string "'''") (string "'''") $ do
  ss <- many $ try $ do
    s1 <- T.pack <$> option "" (try (string "''") <|> string "'")
    s2 <- T.singleton <$> (noneOf ['\'', '\\'] <|> t_echar <|> t_uchar)
    pure (s1 `T.append` s2)
  pure (T.concat ss)

-- [25] STRING_LITERAL_LONG_QUOTE
-- '"""' (('"' | '""')? ([^"\] | ECHAR | UCHAR))* '"""'
t_string_literal_long_double_quote :: (CharParsing m, Monad m) => m T.Text
t_string_literal_long_double_quote = between (string "\"\"\"") (string "\"\"\"") $ do
  ss <- many $ try $ do
    s1 <- T.pack <$> option "" (try (string "\"\"") <|> string "\"")
    s2 <- T.singleton <$> (noneOf ['"', '\\'] <|> t_echar <|> t_uchar)
    pure (s1 `T.append` s2)
  pure (T.concat ss)

-- [144s] LANGTAG
t_langtag :: (CharParsing m, Monad m) => m T.Text
t_langtag = nt_langtag

-- [159s]	ECHAR
t_echar :: (CharParsing m, Monad m) => m Char
t_echar = nt_echar

-- [26]	UCHAR
t_uchar :: (CharParsing m, Monad m) => m Char
t_uchar = nt_uchar

-- [19] INTEGER ::= [+-]? [0-9]+
t_integer :: (CharParsing m, Monad m) => m T.Text
t_integer = try $ do
  sign <- sign_parser <?> "+-"
  ds <- some (satisfy isDigit <?> "digit")
  pure $! (T.pack sign `T.append` T.pack ds)

-- grammar rule: [21] DOUBLE
-- [21] DOUBLE ::= [+-]? ([0-9]+ '.' [0-9]* EXPONENT | '.' [0-9]+ EXPONENT | [0-9]+ EXPONENT)
t_double :: (CharParsing m, Monad m) => m T.Text
t_double = do
  sign <- sign_parser <?> "+-"
  rest <-
    try
      ( do
          ds <- (some (satisfy isDigit) <?> "digit") <* char '.'
          ds' <- many (satisfy isDigit) <?> "digit"
          e <- t_exponent <?> "exponent"
          pure (T.pack ds `T.snoc` '.' `T.append` T.pack ds' `T.append` e)
      )
      <|> try
        ( do
            ds <- char '.' *> some (satisfy isDigit) <?> "digit"
            e <- t_exponent <?> "exponent"
            pure ('.' `T.cons` T.pack ds `T.append` e)
        )
      <|> ( do
              ds <- some (satisfy isDigit) <?> "digit"
              e <- t_exponent <?> "exponent"
              pure (T.pack ds `T.append` e)
          )
  pure $! T.pack sign `T.append` rest

sign_parser :: CharParsing m => m String
sign_parser = option "" (pure <$> oneOf "-+")

-- [20]	DECIMAL ::= [+-]? [0-9]* '.' [0-9]+
t_decimal :: (CharParsing m, Monad m) => m T.Text
t_decimal = try $ do
  sign <- sign_parser
  dig1 <- many (satisfy isDigit) <* char '.'
  dig2 <- some (satisfy isDigit)
  pure (T.pack sign `T.append` T.pack dig1 `T.append` T.pack "." `T.append` T.pack dig2)

-- [154s] EXPONENT ::= [eE] [+-]? [0-9]+
t_exponent :: (CharParsing m, Monad m) => m T.Text
t_exponent = do
  e <- oneOf "eE"
  s <- option "" (pure <$> oneOf "-+")
  ds <- some digit
  pure $! (e `T.cons` (T.pack s `T.append` T.pack ds))

-- [133s] BooleanLiteral ::= 'true' | 'false'
t_boolean :: CharParsing m => m T.Text
t_boolean = T.pack <$> try (string "true" <|> string "false")

t_comment :: CharParsing m => m ()
t_comment = void (char '#' *> many (noneOf "\n\r"))

--[TODO] t_comment = nt_comment

-- [161s] WS ::= #x20 | #x9 | #xD | #xA
t_ws :: CharParsing m => m ()
t_ws =
  (void (try (oneOf "\t\n\r "))) <|> try t_comment
    <?> "whitespace-or-comment"

-- [167s] PN_PREFIX ::= PN_CHARS_BASE ((PN_CHARS | '.')* PN_CHARS)?
t_pn_prefix :: (CharParsing m, MonadState ParseState m) => m T.Text
t_pn_prefix = do
  i <- try t_pn_chars_base
  r <- option "" (many (try t_pn_chars <|> char '.')) -- TODO: ensure t_pn_chars is last char
  pure (T.pack (i : r))

-- [18] IRIREF ::= '<' ([^#x00-#x20<>"{}|^`\] | UCHAR)* '>'
t_iriref :: (CharParsing m, MonadState ParseState m) => m T.Text
t_iriref = between (char '<') (char '>') $ do
  iriFrag <- iriFragment
  bUrl <- currBaseUrl
  dUrl <- currDocUrl
  tryIriResolution bUrl dUrl iriFrag

-- [163s] PN_CHARS_BASE
t_pn_chars_base :: CharParsing m => m Char
t_pn_chars_base = nt_pn_chars_base

-- [164s] PN_CHARS_U ::= PN_CHARS_BASE | '_'
t_pn_chars_u :: CharParsing m => m Char
t_pn_chars_u = t_pn_chars_base <|> char '_'

-- [166s] PN_CHARS ::= PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
t_pn_chars :: CharParsing m => m Char
t_pn_chars = t_pn_chars_u <|> char '-' <|> char '\x00B7' <|> satisfy f
  where
    f = flip in_range [('0', '9'), ('\x0300', '\x036F'), ('\x203F', '\x2040')]

-- grammar rules: [171s] HEX
t_hex :: CharParsing m => m Char
t_hex = satisfy isHexDigit <?> "hexadecimal digit"

{-# INLINE in_range #-}
in_range :: Char -> [(Char, Char)] -> Bool
in_range c = any (\(c1, c2) -> c >= c1 && c <= c2)

currGenIdLookup :: MonadState ParseState m => m (Map String Integer)
currGenIdLookup = gets $ \(_, _, _, _, _, _, _, genMap) -> genMap

addGenIdLookup :: MonadState ParseState m => String -> Integer -> m ()
addGenIdLookup genId counter =
  modify $ \(bUrl, dUrl, i, pms, s, p, ts, genMap) ->
    (bUrl, dUrl, i, pms, s, p, ts, Map.insert genId counter genMap)

currBaseUrl :: MonadState ParseState m => m (Maybe BaseUrl)
currBaseUrl = gets $ \(bUrl, _, _, _, _, _, _, _) -> bUrl

currDocUrl :: MonadState ParseState m => m (Maybe T.Text)
currDocUrl = gets $ \(_, dUrl, _, _, _, _, _, _) -> dUrl

updateBaseUrl :: MonadState ParseState m => Maybe (Maybe BaseUrl) -> m ()
updateBaseUrl val = _modifyState val no no no no no

-- combines get_current and increment into a single function
nextIdCounter :: MonadState ParseState m => m Integer
nextIdCounter = get >>= \(bUrl, dUrl, i, pms, s, p, ts, genMap) ->
  put (bUrl, dUrl, i + 1, pms, s, p, ts, genMap) $> i

nextBlankNode :: MonadState ParseState m => m Node
nextBlankNode = BNodeGen . fromIntegral <$> nextIdCounter

updatePMs :: MonadState ParseState m => Maybe PrefixMappings -> m ()
updatePMs val = _modifyState no no val no no no

-- Alias for Nothing for use with _modifyState calls, which can get very long with
-- many Nothing values.
no :: Maybe a
no = Nothing

withConstantSubjectPredicate :: MonadState ParseState m => m a -> m a
withConstantSubjectPredicate a = do
  (_, _, _, _, s, p, _, _) <- get
  a' <- a
  (bUrl, dUrl, n, pms, _, _, ts, genMap) <- get
  put (bUrl, dUrl, n, pms, s, p, ts, genMap)
  return a'

-- Update the subject and predicate values of the ParseState
setSubjectPredicate :: MonadState ParseState m => Maybe Subject -> Maybe Predicate -> m ()
setSubjectPredicate s p =
  modify $ \(bUrl, dUrl, n, pms, _, _, ts, genMap) ->
    (bUrl, dUrl, n, pms, s, p, ts, genMap)

setSubject :: MonadState ParseState m => Subject -> m ()
setSubject s =
  modify $ \(bUrl, dUrl, n, pms, _, p, ts, genMap) ->
    (bUrl, dUrl, n, pms, Just s, p, ts, genMap)

setPredicate :: MonadState ParseState m => Predicate -> m ()
setPredicate p =
  modify $ \(bUrl, dUrl, n, pms, s, _, ts, genMap) ->
    (bUrl, dUrl, n, pms, s, Just p, ts, genMap)

-- Update the subject and predicate values of the ParseState to Nothing.
resetSubjectPredicate :: MonadState ParseState m => m ()
resetSubjectPredicate = setSubjectPredicate Nothing Nothing

-- Modifies the current parser state by updating any state values among the parameters
-- that have non-Nothing values.
_modifyState ::
  MonadState ParseState m =>
  Maybe (Maybe BaseUrl) ->
  Maybe (Integer -> Integer) ->
  Maybe PrefixMappings ->
  Maybe (Maybe Subject) ->
  Maybe (Maybe Predicate) ->
  Maybe (Seq Triple) ->
  m ()
_modifyState mb_bUrl mb_n mb_pms mb_subj mb_pred mb_trps = do
  (_bUrl, _dUrl, _n, _pms, _s, _p, _ts, genMap) <- get
  put
    ( fromMaybe _bUrl mb_bUrl,
      _dUrl,
      maybe _n (const _n) mb_n,
      fromMaybe _pms mb_pms,
      maybe _s (const _s) mb_subj,
      maybe _p (const _p) mb_pred,
      fromMaybe _ts mb_trps,
      genMap
    )

addTripleForObject :: (CharParsing m, MonadState ParseState m) => Object -> m ()
addTripleForObject obj = do
  (bUrl, dUrl, i, pms, s, p, ts, genMap) <- get
  t <- getTriple s p
  put (bUrl, dUrl, i, pms, s, p, ts |> t, genMap)
  where
    getTriple Nothing _ = unexpected $ "No Subject with which to create triple for: " <> show obj
    getTriple _ Nothing = unexpected $ "No Predicate with which to create triple for: " <> show obj
    getTriple (Just s') (Just p') = pure $ Triple s' p' obj

---------------------------------
-- parsec based parsers

-- | Parse the document at the given location URL as a Turtle document, using an optional @BaseUrl@
--  as the base URI, and using the given document URL as the URI of the Turtle document itself.
--
--  The @BaseUrl@ is used as the base URI within the document for resolving any relative URI references.
--  It may be changed within the document using the @\@base@ directive. At any given point, the current
--  base URI is the most recent @\@base@ directive, or if none, the @BaseUrl@ given to @parseURL@, or
--  if none given, the document URL given to @parseURL@. For example, if the @BaseUrl@ were
--  @http:\/\/example.org\/@ and a relative URI of @\<b>@ were encountered (with no preceding @\@base@
--  directive), then the relative URI would expand to @http:\/\/example.org\/b@.
--
--  The document URL is for the purpose of resolving references to 'this document' within the document,
--  and may be different than the actual location URL from which the document is retrieved. Any reference
--  to @\<>@ within the document is expanded to the value given here. Additionally, if no @BaseUrl@ is
--  given and no @\@base@ directive has appeared before a relative URI occurs, this value is used as the
--  base URI against which the relative URI is resolved.
--
--  Returns either a @ParseFailure@ or a new RDF containing the parsed triples.
parseURLParsec ::
  (Rdf a) =>
  -- | The optional base URI of the document.
  Maybe BaseUrl ->
  -- | The document URI (i.e., the URI of the document itself); if Nothing, use location URI.
  Maybe T.Text ->
  -- | The location URI from which to retrieve the Turtle document.
  String ->
  -- | The parse result, which is either a @ParseFailure@ or the RDF
  --   corresponding to the Turtle document.
  IO (Either ParseFailure (RDF a))
parseURLParsec bUrl docUrl = parseFromURL (parseStringParsec bUrl docUrl)

-- | Parse the given file as a Turtle document. The arguments and return type have the same semantics
--  as 'parseURL', except that the last @String@ argument corresponds to a filesystem location rather
--  than a location URI.
--
--  Note: it does not relies on OS specificities (encoding, newline convention).
--
--  Returns either a @ParseFailure@ or a new RDF containing the parsed triples.
parseFileParsec :: (Rdf a) => Maybe BaseUrl -> Maybe T.Text -> String -> IO (Either ParseFailure (RDF a))
parseFileParsec bUrl docUrl fpath =
  readFile fpath >>= \c -> pure $ handleResult bUrl (runParser (evalStateT t_turtleDoc (initialState bUrl docUrl)) () (maybe "" T.unpack docUrl) c)

-- | Parse the given string as a Turtle document. The arguments and return type have the same semantics
--  as <parseURL>, except that the last @String@ argument corresponds to the Turtle document itself as
--  a string rather than a location URI.
parseStringParsec :: (Rdf a) => Maybe BaseUrl -> Maybe T.Text -> T.Text -> Either ParseFailure (RDF a)
parseStringParsec bUrl docUrl ttlStr = handleResult bUrl (runParser (evalStateT t_turtleDoc (initialState bUrl docUrl)) () "" ttlStr)

---------------------------------
-- attoparsec based parsers

parseStringAttoparsec :: (Rdf a) => Maybe BaseUrl -> Maybe T.Text -> T.Text -> Either ParseFailure (RDF a)
parseStringAttoparsec bUrl docUrl t = handleResult' $ parse (evalStateT t_turtleDoc (initialState bUrl docUrl)) t
  where
    handleResult' res = case res of
      Fail _ _ err ->
        Left $ ParseFailure $ "Parse failure: \n" <> show err
      Partial f -> handleResult' (f mempty)
      Done _ (ts, mb_bUrl, pms) ->
        let chosenBaseUrl =
              if isJust mb_bUrl
                then mb_bUrl
                else bUrl
         in Right $! mkRdf (F.toList ts) chosenBaseUrl pms

parseFileAttoparsec :: (Rdf a) => Maybe BaseUrl -> Maybe T.Text -> String -> IO (Either ParseFailure (RDF a))
parseFileAttoparsec bUrl docUrl path = parseStringAttoparsec bUrl docUrl <$> readFile path

parseURLAttoparsec ::
  (Rdf a) =>
  -- | The optional base URI of the document.
  Maybe BaseUrl ->
  -- | The document URI (i.e., the URI of the document itself); if Nothing, use location URI.
  Maybe T.Text ->
  -- | The location URI from which to retrieve the Turtle document.
  String ->
  -- | The parse result, which is either a @ParseFailure@ or the RDF
  --   corresponding to the Turtle document.
  IO (Either ParseFailure (RDF a))
parseURLAttoparsec bUrl docUrl = parseFromURL (parseStringAttoparsec bUrl docUrl)

---------------------------------

initialState :: Maybe BaseUrl -> Maybe T.Text -> ParseState
initialState bUrl docUrl = (BaseUrl <$> docUrl <|> bUrl, docUrl, 1, PrefixMappings mempty, Nothing, Nothing, mempty, mempty)

handleResult :: Rdf a => Maybe BaseUrl -> Either ParseError (Seq Triple, Maybe BaseUrl, PrefixMappings) -> Either ParseFailure (RDF a)
handleResult bUrl result = case result of
  (Left err) -> Left (ParseFailure $ "Parse failure: \n" <> show err)
  (Right (ts, mb_bUrl, pms)) ->
    -- the base URL may have been overwritten by a @base statement.
    let chosenBaseUrl =
          if isJust mb_bUrl
            then mb_bUrl
            else bUrl
     in Right $! mkRdf (F.toList ts) chosenBaseUrl pms

--------------
-- auxiliary parsing functions

-- Match the lowercase or uppercase form of 'c'
caseInsensitiveChar :: CharParsing m => Char -> m Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

-- Match the string 's', accepting either lowercase or uppercase form of each character
caseInsensitiveString :: (CharParsing m, Monad m) => String -> m String
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" <> s <> "\""

tryIriResolution :: (CharParsing m) => Maybe BaseUrl -> Maybe T.Text -> T.Text -> m T.Text
tryIriResolution mbUrl mdUrl iriFrag = tryIriResolution' mbUrl mdUrl
  where
    tryIriResolution' (Just (BaseUrl bIri)) _ = either err pure (resolveIRI bIri iriFrag)
    tryIriResolution' _ (Just dIri) = either err pure (resolveIRI dIri iriFrag)
    tryIriResolution' _ _ = either err pure (resolveIRI mempty iriFrag)
    err m = unexpected $ mconcat ["Cannot resolve IRI: ", m, " ", show (mbUrl, mdUrl, iriFrag)]
