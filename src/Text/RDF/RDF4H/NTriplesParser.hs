-- |A parser for RDF in N-Triples format
-- <http://www.w3.org/TR/rdf-testcases/#ntriples>.

module Text.RDF.RDF4H.NTriplesParser(
  NTriplesParser(NTriplesParser), ParseFailure
) where

import Prelude hiding (init,pred)
import Data.RDF.Types
import Text.RDF.RDF4H.ParserUtils
import Data.Char(isLetter, isDigit,isAlphaNum)
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (liftM,void)

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
nt_ntripleDoc :: GenParser () [Maybe Triple]
nt_ntripleDoc = manyTill nt_line eof

nt_line :: GenParser () (Maybe Triple)
nt_line       =
    skipMany nt_space >>
     ((nt_comment >>= \res -> return res)
      <|> (nt_triple >>= \res -> nt_eoln >> return res)
      <|> (nt_empty >>= \res -> nt_eoln >> return res))
     >>= \res -> return res

nt_comment :: GenParser () (Maybe Triple)
nt_comment   = char '#' >> manyTill anyChar nt_eoln >> return Nothing

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
nt_triple :: GenParser () (Maybe Triple)
nt_triple    =
  do
    subj <- nt_subject
    optional (skipMany1 nt_space)
    pred <- nt_predicate
    optional (skipMany1 nt_space)
    obj <- nt_object
    optional (skipMany1 nt_space)
    void (char '.')
    void (many nt_space)
    return $ Just (Triple subj pred obj)

-- [6] literal
nt_literal :: GenParser () LValue
nt_literal = do
  s <- nt_string_literal_quote
  option (plainL s) $ do
               ((count 2 (char '^') >> nt_iriref >>= \iri -> return (typedL s iri))
                <|> (nt_langtag  >>= \lang -> return (plainLL s lang)))

-- [9] STRING_LITERAL_QUOTE
nt_string_literal_quote :: GenParser () T.Text
nt_string_literal_quote =
    between (char '"') (char '"') $ do
      T.concat <$> (many ((T.singleton <$> noneOf ['\x22','\x5C','\xA','\xD']) <|>
                          try nt_echar <|>
                          nt_uchar))

-- [144s] LANGTAG
nt_langtag :: GenParser () T.Text
nt_langtag = do
  char '@'
  ss   <- many1 (satisfy (\ c -> isLetter c))
  rest <- concat <$> many (char '-' >> many1 (satisfy (\ c -> isAlphaNum c)))
  return (T.pack (ss ++ rest))

-- [8] IRIREF
nt_iriref :: GenParser () T.Text
nt_iriref = do
  between (char '<') (char '>') $ do
              T.concat <$> many ( T.singleton <$> noneOf ['\x00','\x20','<','>','"','{','}','|','^','`','\\'] <|>
                                  nt_uchar )

-- [153s] ECHAR
nt_echar :: GenParser () T.Text
nt_echar = do
  char '\\'
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

-- [10] UCHAR
nt_uchar :: GenParser () T.Text
nt_uchar =
    (try (char '\\' >> char 'u' >> count 4 hexDigit >>= \cs -> return $ T.pack ('\\':'u':cs)) <|>
     try (char '\\' >> char 'U' >> count 8 hexDigit >>= \cs -> return $ T.pack ('\\':'U':cs)))

-- nt_empty is a line that isn't a comment or a triple. They appear in the
-- parsed output as Nothing, whereas a real triple appears as (Just triple).
nt_empty :: GenParser () (Maybe Triple)
nt_empty     = skipMany nt_space >> return Nothing

-- A subject is either a URI reference for a resource or a node id for a
-- blank node.
nt_subject :: GenParser () Node
nt_subject   =
  liftM unode nt_uriref <|>
  liftM bnode nt_nodeID

-- A predicate may only be a URI reference to a resource.
nt_predicate :: GenParser () Node
nt_predicate = liftM unode nt_uriref

-- An object may be either a resource (represented by a URI reference),
-- a blank node (represented by a node id), or an object literal.
nt_object :: GenParser () Node
nt_object =
  liftM unode nt_uriref <|>
  liftM bnode nt_nodeID <|>
  liftM LNode nt_literal

validateUNode :: T.Text -> GenParser () Node
validateUNode t =
    case unodeValidate t of
      Nothing        -> unexpected ("Invalid URI in NTriples parser URI validation: " ++ show t)
      Just u@(UNode{}) -> return u
      Just node      -> unexpected ("Unexpected node in NTriples parser URI validation: " ++ show node)

validateURI :: T.Text -> GenParser () T.Text
validateURI t = do
    UNode uri <- validateUNode t
    return uri

-- A URI reference is one or more nrab_character inside angle brackets.
nt_uriref :: GenParser () T.Text
nt_uriref = between (char '<') (char '>') $ do
              unvalidatedUri <- many (satisfy ( /= '>'))
              validateURI (T.pack unvalidatedUri)

-- A node id is "_:" followed by a name.
nt_nodeID :: GenParser () T.Text
nt_nodeID = char '_' >> char ':' >> nt_name >>= \n ->
                return ('_' `T.cons` (':' `T.cons` n))

-- A name is a letter followed by any number of alpha-numeric characters.
nt_name :: GenParser () T.Text
nt_name =
  do init <- letter
     rest <- many (satisfy isLetterOrDigit)
     return $ T.pack (init:rest)

isLetterOrDigit :: Char -> Bool
isLetterOrDigit c = isLetter c || isDigit c

-- End-of-line consists of either lf or crlf.
-- We also test for eof and consider that to match as well.
nt_eoln :: GenParser () ()
nt_eoln =  eof <|> void (nt_cr >> nt_lf) <|> void nt_lf

-- Whitespace is either a space or tab character. We must avoid using the
-- built-in space combinator here, because it includes newline.
nt_space :: GenParser () Char
nt_space = char ' ' <|> nt_tab

-- Carriage return is \r.
nt_cr :: GenParser () Char
nt_cr          =   char '\r'

-- Line feed is \n.
nt_lf :: GenParser () Char
nt_lf          =   char '\n'

-- Tab is \t.
nt_tab :: GenParser () Char
nt_tab         =   char '\t'

parseString' :: forall rdf. (RDF rdf) => T.Text -> Either ParseFailure rdf
parseString' bs = handleParse mkRdf (runParser nt_ntripleDoc () "" bs)

parseURL' :: forall rdf. (RDF rdf) => String -> IO (Either ParseFailure rdf)
parseURL' = _parseURL parseString'

parseFile' :: forall rdf. (RDF rdf) => String -> IO (Either ParseFailure rdf)
parseFile' path = liftM (handleParse mkRdf . runParser nt_ntripleDoc () path)
                   (TIO.readFile path)

handleParse :: forall rdf. (RDF rdf) => (Triples -> Maybe BaseUrl -> PrefixMappings -> rdf) ->
                                        Either ParseError [Maybe Triple] ->
                                        Either ParseFailure rdf
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
