-- |A parser for RDF in N-Triples format 
-- <http://www.w3.org/TR/rdf-testcases/#ntriples>.

module Text.RDF.RDF4H.NTriplesParser(
  NTriplesParser(NTriplesParser), ParseFailure
) where

import Prelude hiding (init,pred)
import Data.RDF.Types
import Text.RDF.RDF4H.ParserUtils
import Data.Char(isLetter, isDigit, isLower)
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.Text.Lazy
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
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
     (nt_comment <|> nt_triple <|> nt_empty) >>=
     \res -> nt_eoln >> return res

-- A comment consists of an initial # character, followed by any number of
-- characters except cr or lf. The spec is redundant in specifying that
-- comment is hash followed by "character - (cr | lf)", since character
-- is already defined as the range #x0020-#x007E, so cr #x000D and
-- lf #x000A are both already excluded. This returns Nothing as we are
-- ignoring comments for now.
nt_comment :: GenParser () (Maybe Triple)
nt_comment   = char '#' >> skipMany nt_character >> return Nothing

-- A triple consists of whitespace-delimited subject, predicate, and object,
-- followed by optional whitespace and a period, and possibly more
-- whitespace.
nt_triple :: GenParser () (Maybe Triple)
nt_triple    =
  do
    subj <- nt_subject
    skipMany1 nt_space
    pred <- nt_predicate
    skipMany1 nt_space
    obj <- nt_object
    skipMany nt_space
    char '.'
    many nt_space
    return $ Just (Triple subj pred obj)

-- A literal is either a language literal (with optional language
-- specified) or a datatype literal (with required datatype
-- specified). The literal value is always enclosed in double
-- quotes. A language literal may have '@' after the closing quote,
-- followed by a language specifier. A datatype literal follows
-- the closing quote with ^^ followed by the URI of the datatype.
nt_literal :: GenParser () LValue
nt_literal = 
  do lit_str <- between_chars '"' '"' inner_literal 
     liftM (plainLL lit_str) (char '@' >> nt_language) <|>
       liftM (typedL lit_str) (count 2 (char '^') >> nt_uriref) <|>
       return (plainL lit_str)
  where inner_literal = liftM T.concat (manyTill inner_string (lookAhead $ char '"'))

-- A language specifier of a language literal is any number of lowercase
-- letters followed by any number of blocks consisting of a hyphen followed
-- by one or more lowercase letters or digits.
nt_language :: GenParser () T.Text
nt_language =
  do str <- liftM T.pack (many (satisfy (\ c -> c == '-' || isLower c)))
     if T.null str || T.last str == '-' || T.head str == '-'
        then fail ("Invalid language string: '" ++ T.unpack str ++ "'")
        else return str

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

-- A URI reference is one or more nrab_character inside angle brackets.
nt_uriref :: GenParser () T.Text
nt_uriref = between_chars '<' '>' (liftM T.pack (many (satisfy ( /= '>'))))

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

-- An nt_character is any character except a double quote character.
nt_character :: GenParser () Char
nt_character   =   satisfy is_nonquote_char

-- A character is any Unicode value from ASCII space to decimal 126 (tilde).
is_character :: Char -> Bool
is_character c =   c >= '\x0020' && c <= '\x007E'

-- A non-quote character is a character that isn't the double-quote character.
is_nonquote_char :: Char -> Bool
is_nonquote_char c = is_character c && c/= '"'

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

-- An inner_string is a fragment of a string (this is used inside double
-- quotes), and consists of the non-quote characters allowed and the
-- standard escapes for a backslash (\\), a tab (\t), a carriage  return (\r),
-- a newline (\n), a double-quote (\"), a 4-digit Unicode escape (\uxxxx
-- where x is a hexadecimal digit), and an 8-digit Unicode escape
-- (\Uxxxxxxxx where x is a hexadecimaldigit).
inner_string :: GenParser () T.Text
inner_string =
  try (char '\\' >>
         ((char 't' >> return b_tab)  <|>
          (char 'r' >> return b_ret)  <|>
          (char 'n' >> return b_nl)  <|>
          (char '\\' >> return b_slash) <|>
          (char '"' >> return b_quote)   <|>
          (char 'u' >> count 4 hexDigit >>= \cs -> return $ T.pack ('\\':'u':cs)) <|>
          (char 'U' >> count 8 hexDigit >>= \cs -> return $ T.pack ('\\':'U':cs))))
  <|> liftM T.pack
    (many (satisfy (\ c -> is_nonquote_char c && c /= '\\')))

b_tab, b_ret, b_nl, b_slash, b_quote :: T.Text
b_tab = T.singleton '\t'
b_ret = T.singleton '\r'
b_nl  = T.singleton '\n'
b_slash = T.singleton '\\'
b_quote = T.singleton '"'

between_chars :: Char -> Char -> GenParser () T.Text -> GenParser () T.Text
between_chars start end parser = char start >> parser >>= \res -> char end >> return res

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
