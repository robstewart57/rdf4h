{-# LANGUAGE CPP #-}

-- | A parser for RDF in N-Triples format
--  <http://www.w3.org/TR/rdf-testcases/#ntriples>.
module Text.RDF.RDF4H.NTriplesParser
  ( NTriplesParser (NTriplesParser),
    NTriplesParserCustom (NTriplesParserCustom),
    ParseFailure,
    nt_echar,
    nt_uchar,
    nt_langtag,
    string_literal_quote,
    nt_string_literal_quote,
    nt_pn_chars_base,
    nt_comment,
    readFile,
  )
where

#if MIN_VERSION_base(4,9,0)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#else
#endif
#else
#endif

import Control.Applicative
import Control.Monad (void)
import Data.Attoparsec.ByteString (IResult (..), parse)
import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper, isDigit, isLetter)
import Data.RDF.IRI
import Data.RDF.Types hiding (empty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import System.IO (IOMode (..), hSetEncoding, hSetNewlineMode, noNewlineTranslation, utf8, withFile)
import Text.Parsec (ParseError, runParser)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.RDF.RDF4H.ParserUtils
import Prelude hiding (readFile)

-- | NTriplesParser is an 'RdfParser' implementation for parsing RDF in the
--  NTriples format. It requires no configuration options. To use this parser,
--  pass an 'NTriplesParser' value as the first argument to any of the
--  'parseString', 'parseFile', or 'parseURL' methods of the 'RdfParser' type
--  class.
data NTriplesParser = NTriplesParser

newtype NTriplesParserCustom = NTriplesParserCustom Parser

-- | 'NTriplesParser' is an instance of 'RdfParser' using parsec based parsers.
instance RdfParser NTriplesParser where
  parseString _ = parseStringParsec
  parseFile _ = parseFileParsec
  parseURL _ = parseURLParsec

-- | 'NTriplesParser' is an instance of 'RdfParser'.
instance RdfParser NTriplesParserCustom where
  parseString (NTriplesParserCustom Parsec) = parseStringParsec
  parseString (NTriplesParserCustom Attoparsec) = parseStringAttoparsec
  parseFile (NTriplesParserCustom Parsec) = parseFileParsec
  parseFile (NTriplesParserCustom Attoparsec) = parseFileAttoparsec
  parseURL (NTriplesParserCustom Parsec) = parseURLParsec
  parseURL (NTriplesParserCustom Attoparsec) = parseURLAttoparsec

-- We define or redefine all here using same names as the spec, but with an
-- 'nt_' prefix in order to avoid name clashes (e.g., ntripleDoc becomes
-- nt_ntripleDoc).

-- | nt_ntripleDoc is simply zero or more lines.
--  grammar [1] ntriplesDoc ::= triple? (EOL triple)* EOL?
nt_ntripleDoc :: (CharParsing m, LookAheadParsing m, Monad m) => m [Triple]
nt_ntripleDoc = many sep *> sepEndBy (try nt_triple) (many sep) <* eof
  where
    sep = many nt_space *> (try nt_comment <|> try nt_eoln) *> many nt_space

-- A triple consists of whitespace-delimited subject, predicate, and object,
-- followed by optional whitespace and a period, and possibly more
-- whitespace.
--
-- NTriples W3C test "minimal_whitespace" proposes no space:
--
-- "tests absense of whitespace between subject, predicate, object and
-- end-of-statement"
--
-- `optional` lets this nt_triple parser succeed even if there is not
-- a space or tab character between resources or the object and the '.'.
-- Grammar [2] triple ::= subject predicate object '.'
nt_triple :: (CharParsing m, LookAheadParsing m, Monad m) => m Triple
nt_triple =
  Triple
    <$> (nt_subject <* optional (skipSome nt_space))
    <*> (nt_predicate <* optional (skipSome nt_space))
    <*> (nt_object <* optional (skipSome nt_space) <* char '.' <* many nt_space)

-- Grammar [6] literal ::= STRING_LITERAL_QUOTE ('^^' IRIREF | LANGTAG)?
nt_literal :: (CharParsing m, Monad m) => m LValue
nt_literal = do
  str <- nt_string_literal_quote
  option (plainL str) (langTag str <|> typeIRI str)
  where
    langTag str = plainLL str <$> try nt_langtag
    typeIRI str = typedL str <$> try (count 2 (char '^') *> nt_iriref)

-- Grammar [9] STRING_LITERAL_QUOTE ::= '"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'
nt_string_literal_quote :: (CharParsing m, Monad m) => m T.Text
nt_string_literal_quote = string_literal_quote '"'

string_literal_quote :: (CharParsing m, Monad m) => Char -> m T.Text
string_literal_quote d = between (char d) (char d) string_literal
  where
    string_literal = T.pack <$> many (try validLiteralChar)
    validLiteralChar =
      noneOf [d, '\x5C', '\xA', '\xD']
        <|> nt_echar
        <|> nt_uchar

-- Grammar [144s] LANGTAG ::= '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
nt_langtag :: (CharParsing m, Monad m) => m T.Text
nt_langtag = do
  ss <- char '@' *> some (satisfy isLetter)
  rest <- concat <$> many (char '-' *> some (satisfy isAlphaNum) >>= \lang_str -> pure ('-' : lang_str))
  pure (T.pack (ss <> rest))

-- [8] IRIREF
nt_iriref :: (CharParsing m, Monad m) => m T.Text
nt_iriref = between (char '<') (char '>') $ do
  raw_iri <- iriFragment
  either (const empty) pure (validateIRI raw_iri) <?> "Only absolute IRIs allowed in NTriples format, which this isn't: " <> show raw_iri

-- [153s] ECHAR
nt_echar :: (CharParsing m, Monad m) => m Char
nt_echar = try $ do
  c2 <- char '\\' *> anyChar
  case c2 of
    't' -> pure '\t'
    'b' -> pure '\b'
    'n' -> pure '\n'
    'r' -> pure '\r'
    'f' -> pure '\f'
    '"' -> pure '\"'
    '\'' -> pure '\''
    '\\' -> pure '\\'
    _ -> empty

-- [10] UCHAR ::= '\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
nt_uchar :: (CharParsing m, Monad m) => m Char
nt_uchar = uchar

-- A subject is either a URI reference for a resource or a node id for a
-- blank node.
nt_subject :: (CharParsing m, LookAheadParsing m, Monad m) => m Node
nt_subject =
  unode <$> try nt_iriref
    <|> bnodeUnsafe <$> nt_blank_node_label

-- A predicate may only be a URI reference to a resource.
nt_predicate :: (CharParsing m, Monad m) => m Node
nt_predicate = unode <$> nt_iriref

-- An object may be either a resource (represented by a URI reference),
-- a blank node (represented by a node id), or an object literal.
nt_object :: (CharParsing m, LookAheadParsing m, Monad m) => m Node
nt_object =
  unode <$> try nt_iriref
    <|> bnodeUnsafe <$> try nt_blank_node_label
    <|> LNode <$> nt_literal

-- [141s] BLANK_NODE_LABEL ::= '_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
nt_blank_node_label :: (CharParsing m, LookAheadParsing m, Monad m) => m T.Text
nt_blank_node_label = do
  void (string "_:")
  firstChar <- nt_pn_chars_u <|> satisfy isDigit
  otherChars <-
    option "" $
      try $
        many (nt_pn_chars <|> try (char '.' <* lookAhead (try nt_pn_chars)))
  pure $ T.pack (firstChar : otherChars)

-- [157s] PN_CHARS_BASE ::= [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
nt_pn_chars_base :: CharParsing m => m Char
nt_pn_chars_base = try $ satisfy isBaseChar
  where
    isBaseChar c =
      (isAsciiUpper c)
        || (isAsciiLower c)
        || (c >= '\x00C0' && c <= '\x00D6')
        || (c >= '\x00D8' && c <= '\x00F6')
        || (c >= '\x00F8' && c <= '\x02FF')
        || (c >= '\x0370' && c <= '\x037D')
        || (c >= '\x037F' && c <= '\x1FFF')
        || (c >= '\x200C' && c <= '\x200D')
        || (c >= '\x2070' && c <= '\x218F')
        || (c >= '\x2C00' && c <= '\x2FEF')
        || (c >= '\x3001' && c <= '\xD7FF')
        || (c >= '\xF900' && c <= '\xFDCF')
        || (c >= '\xFDF0' && c <= '\xFFFD')
        || (c >= '\x10000' && c <= '\xEFFFF')

-- [158s] PN_CHARS_U
nt_pn_chars_u :: CharParsing m => m Char
nt_pn_chars_u = nt_pn_chars_base <|> try (char '_') <|> try (char ':')

-- [160s] PN_CHARS
nt_pn_chars :: CharParsing m => m Char
nt_pn_chars =
  nt_pn_chars_u
    <|> try (char '-')
    <|> try (char '\x00B7')
    <|> try (satisfy f)
  where
    f c =
      isDigit c
        || (c >= '\x0300' && c <= '\x036F')
        || (c >= '\x203F' && c <= '\x2040')

-- End-of-line consists of either lf or crlf.
-- We also test for eof and consider that to match as well.
nt_eoln :: CharParsing m => m ()
nt_eoln = try (void (string "\r\n")) <|> void (char '\n')

-- Whitespace is either a space or a tabulation.
-- The built-in space combinator must be avoided here, because it includes newline.
nt_space :: CharParsing m => m ()
nt_space = void (try (char ' ') <|> try (char '\t'))

nt_comment :: CharParsing m => m ()
nt_comment = void (char '#' *> manyTill anyChar (try nt_eoln))

---------------------------------
-- parsec based parsers

parseStringParsec :: (Rdf a) => T.Text -> Either ParseFailure (RDF a)
parseStringParsec bs = handleParsec mkRdf (runParser nt_ntripleDoc () "" bs)

parseFileParsec :: (Rdf a) => String -> IO (Either ParseFailure (RDF a))
parseFileParsec path =
  handleParsec mkRdf . runParser nt_ntripleDoc () path
    <$> readFile path

readFile :: FilePath -> IO T.Text
readFile fpath = withFile fpath ReadMode $ \h -> do
  hSetNewlineMode h noNewlineTranslation
  hSetEncoding h utf8
  T.hGetContents h

parseURLParsec :: (Rdf a) => String -> IO (Either ParseFailure (RDF a))
parseURLParsec = parseFromURL parseStringParsec

handleParsec ::
  (Triples -> Maybe BaseUrl -> PrefixMappings -> RDF a) ->
  Either ParseError [Triple] ->
  Either ParseFailure (RDF a)
handleParsec _mkRdf result = case result of
  Left err -> Left $ ParseFailure $ "Parse failure: \n" <> show err
  Right ts -> Right $ _mkRdf ts Nothing (PrefixMappings mempty)

---------------------------------

---------------------------------
-- attoparsec based parsers

parseFileAttoparsec :: (Rdf a) => String -> IO (Either ParseFailure (RDF a))
parseFileAttoparsec path = handleAttoparsec <$> readFile path

parseURLAttoparsec :: (Rdf a) => String -> IO (Either ParseFailure (RDF a))
parseURLAttoparsec = parseFromURL handleAttoparsec

parseStringAttoparsec :: (Rdf a) => T.Text -> Either ParseFailure (RDF a)
parseStringAttoparsec = handleAttoparsec

handleAttoparsec :: (Rdf a) => T.Text -> Either ParseFailure (RDF a)
handleAttoparsec bs = handleResult $ parse nt_ntripleDoc (T.encodeUtf8 bs)
  where
    handleResult res = case res of
      Fail _i _contexts err -> Left $ ParseFailure $ "Parse failure: \n" <> show err
      -- error $
      -- "\nnot consumed: " <> show i
      -- <> "\ncontexts: " <> show contexts
      -- <> "\nerror: " <> show err
      Partial f -> handleResult (f (T.encodeUtf8 mempty))
      Done _ ts -> Right $ mkRdf ts Nothing (PrefixMappings mempty)

---------------------------------
