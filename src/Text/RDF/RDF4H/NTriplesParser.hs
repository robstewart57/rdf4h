-- |A parser for RDF in N-Triples format
-- <http://www.w3.org/TR/rdf-testcases/#ntriples>.

module Text.RDF.RDF4H.NTriplesParser
  (NTriplesParser(NTriplesParser), ParseFailure)
  where

import Prelude hiding (init,pred)
import Data.RDF.Types
import Text.RDF.RDF4H.ParserUtils
import Data.Char (isLetter, isDigit,isAlphaNum, isAsciiUpper, isAsciiLower)
import Data.Map as Map (empty)
import Text.Parsec (runParser,ParseError)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (void)

import Text.Parser.Char
import Text.Parser.Combinators
import Control.Applicative

-- |NTriplesParser is an 'RdfParser' implementation for parsing RDF in the
-- NTriples format. It requires no configuration options. To use this parser,
-- pass an 'NTriplesParser' value as the first argument to any of the
-- 'parseString', 'parseFile', or 'parseURL' methods of the 'RdfParser' type
-- class.
data NTriplesParser = NTriplesParser

-- |'NTriplesParser' is an instance of 'RdfParser'.
instance RdfParser NTriplesParser where
  parseString _  = parseString'
  parseFile   _  = parseFile'
  parseURL    _  = parseURL'

-- We define or redefine all here using same names as the spec, but with an
-- 'nt_' prefix in order to avoid name clashes (e.g., ntripleDoc becomes
-- nt_ntripleDoc).

-- |nt_ntripleDoc is simply zero or more lines.
nt_ntripleDoc :: (CharParsing m, Monad m) => m [Maybe Triple]
nt_ntripleDoc = manyTill nt_line eof

nt_line :: (CharParsing m, Monad m) => m (Maybe Triple)
nt_line =
    skipMany nt_space >>
     ((nt_comment >>= \res -> pure res)
      <|> try (nt_triple >>= \res -> nt_eoln *> pure res)
      <|> try (nt_triple >>= \res -> char '#' *> manyTill anyChar nt_eoln *> pure res)
      <|> (nt_empty >>= \res -> nt_eoln *> pure res))
     >>= \res -> pure res

nt_comment :: CharParsing m => m (Maybe Triple)
nt_comment   = char '#' *> manyTill anyChar nt_eoln *> pure Nothing

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
nt_triple :: (CharParsing m, Monad m) => m (Maybe Triple)
nt_triple    =
  do
    subj <- nt_subject
    optional (skipSome nt_space)
    pred <- nt_predicate
    optional (skipSome nt_space)
    obj <- nt_object
    optional (skipSome nt_space)
    void (char '.')
    void (many nt_space)
    pure $ Just (Triple subj pred obj)

-- [6] literal
nt_literal :: (CharParsing m, Monad m) => m LValue
nt_literal = do
  s' <- nt_string_literal_quote
  let s = escapeRDFSyntax s'
  option (plainL s) $
               (count 2 (char '^') *> nt_iriref >>= validateURI >>= isAbsoluteParser >>= \iri -> pure (typedL s iri))
                <|> (nt_langtag >>= \lang -> pure (plainLL s lang))

-- [9] STRING_LITERAL_QUOTE
nt_string_literal_quote :: (CharParsing m, Monad m) => m T.Text
nt_string_literal_quote =
    between (char '"') (char '"') $
      T.concat <$> many ((T.singleton <$> noneOf ['\x22','\x5C','\xA','\xD']) <|>
                          nt_echar <|>
                          nt_uchar)

-- [144s] LANGTAG
nt_langtag :: (CharParsing m, Monad m) => m T.Text
nt_langtag = do
  void (char '@')
  ss   <- some (satisfy isLetter)
  rest <- concat <$> many (char '-' *> some (satisfy isAlphaNum) >>= \lang_str -> pure ('-':lang_str))
  pure (T.pack (ss ++ rest))

-- [8] IRIREF
nt_iriref ::(CharParsing m, Monad m) => m T.Text
nt_iriref =
  between (char '<') (char '>') $
              T.concat <$> many ( T.singleton <$> noneOf (['\x00'..'\x20'] ++ ['<','>','"','{','}','|','^','`','\\']) <|>
                                  nt_uchar )

-- [153s] ECHAR
nt_echar :: (CharParsing m, Monad m) => m T.Text
nt_echar = try $ do
  void (char '\\')
  c2 <- satisfy isEchar
  pure (T.pack [c2])

isEchar :: Char -> Bool
isEchar = (`elem` ['t','b','n','r','f','"','\'','\\'])

-- [10] UCHAR
nt_uchar :: (CharParsing m, Monad m) => m T.Text
nt_uchar =
    try (char '\\' *> char 'u' *> count 4 hexDigit >>= \cs -> pure $ T.pack ('\\':'u':cs)) <|>
    try (char '\\' *> char 'U' *> count 8 hexDigit >>= \cs -> pure $ T.pack ('\\':'U':cs))

-- nt_empty is a line that isn't a comment or a triple. They appear in the
-- parsed output as Nothing, whereas a real triple appears as (Just triple).
nt_empty :: CharParsing m => m (Maybe Triple)
nt_empty     = skipMany nt_space *> pure Nothing

-- A subject is either a URI reference for a resource or a node id for a
-- blank node.
nt_subject :: (CharParsing m, Monad m) => m Node
nt_subject   =
  fmap unode nt_uriref <|>
  fmap bnode nt_blank_node_label

-- A predicate may only be a URI reference to a resource.
nt_predicate :: (CharParsing m, Monad m) => m Node
nt_predicate = fmap unode nt_uriref

-- An object may be either a resource (represented by a URI reference),
-- a blank node (represented by a node id), or an object literal.
nt_object :: (CharParsing m, Monad m) => m Node
nt_object =
  fmap unode nt_uriref <|>
  fmap bnode nt_blank_node_label <|>
  fmap LNode nt_literal

validateUNode :: CharParsing m => T.Text -> m Node
validateUNode t =
    case unodeValidate t of
      Nothing        -> unexpected ("Invalid URI in NTriples parser URI validation: " ++ show t)
      Just u@UNode{} -> pure u
      Just node      -> unexpected ("Unexpected node in NTriples parser URI validation: " ++ show node)

validateURI :: (CharParsing m, Monad m) => T.Text -> m T.Text
validateURI t = do
    UNode uri <- validateUNode t
    pure uri

isAbsoluteParser :: CharParsing m => T.Text -> m T.Text
isAbsoluteParser t =
    if isAbsoluteUri t
    then pure t
    else unexpected ("Only absolute IRIs allowed in NTriples format, which this isn't: " ++ show t)

absoluteURI :: CharParsing m => T.Text -> m T.Text
absoluteURI = isAbsoluteParser

-- A URI reference is one or more nrab_character inside angle brackets.
nt_uriref :: (CharParsing m, Monad m) => m T.Text
nt_uriref = between (char '<') (char '>') $ do
              unvalidatedUri <- many (satisfy ( /= '>'))
              t <- validateURI (T.pack unvalidatedUri)
              absoluteURI t

-- [141s] BLANK_NODE_LABEL
nt_blank_node_label :: (CharParsing m, Monad m) => m T.Text
nt_blank_node_label = do
  void (char '_' *> char ':')
  s1 <- nt_pn_chars_u <|> satisfy isDigit
  s2 <- option "" $ try $ do
          sub_dots <- many (char '.')
          sub_s1   <- some nt_pn_chars
          pure (sub_dots ++ sub_s1)
  pure (T.pack ("_:" ++ [s1] ++ s2))

-- [157s] PN_CHARS_BASE
-- Not used. It used to be. What's happened?
{-
nt_pn_chars_base :: CharParsing m => m Char
nt_pn_chars_base = try $ satisfy isBaseChar c

-}

isBaseChar :: Char -> Bool
isBaseChar c
    =  isAsciiUpper c
    || isAsciiLower c
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
nt_pn_chars_u = try $ satisfy $ \c -> c == '_' || c == ':' || isBaseChar c


-- [160s] PN_CHARS
nt_pn_chars :: CharParsing m => m Char
nt_pn_chars = nt_pn_chars_u
  <|> try (satisfy $ \c ->
           c == '-' || c == '\x00B7'
           || isDigit c
           || (c >= '\x0300' && c <= '\x036F')
           || (c >= '\x203F' && c <= '\x2040')
    )

-- End-of-line consists of either lf or crlf.
-- We also test for eof and consider that to match as well.
nt_eoln :: CharParsing m => m ()
nt_eoln =  eof <|> void (nt_cr *> nt_lf) <|> void nt_lf

-- Whitespace is either a space or tab character. We must avoid using the
-- built-in space combinator here, because it includes newline.
nt_space :: CharParsing m => m Char
nt_space = char ' ' <|> nt_tab

-- Carriage pure is \r.
nt_cr :: CharParsing m => m Char
nt_cr          =   char '\r'

-- Line feed is \n.
nt_lf :: CharParsing m => m Char
nt_lf          =   char '\n'

-- Tab is \t.
nt_tab :: CharParsing m => m Char
nt_tab         =   char '\t'

parseString' :: (Rdf a) => T.Text -> Either ParseFailure (RDF a)
parseString' bs = handleParse mkRdf (runParser nt_ntripleDoc () "" bs)

parseURL' :: (Rdf a) => String -> IO (Either ParseFailure (RDF a))
parseURL' = _parseURL parseString'

parseFile' :: (Rdf a) => String -> IO (Either ParseFailure (RDF a))
parseFile' path = fmap (handleParse mkRdf . runParser nt_ntripleDoc () path)
                   (TIO.readFile path)

handleParse :: {-forall rdf. (RDF rdf) => -} (Triples -> Maybe BaseUrl -> PrefixMappings -> RDF a) ->
                                        Either ParseError [Maybe Triple] ->
                                        Either ParseFailure (RDF a)
handleParse _mkRdf result
--  | T.length rem /= 0 = (Left $ ParseFailure $ "Invalid Document. Unparseable end of document: " ++ T.unpack rem)
  | otherwise          =
      case result of
        Left err -> Left  $ ParseFailure $ "Parse failure: \n" ++ show err
        Right ts -> Right $ _mkRdf (conv ts) Nothing (PrefixMappings Map.empty)
  where
    conv []            = []
    conv (Nothing:ts)  = conv ts
    conv (Just t:ts) = t : conv ts
