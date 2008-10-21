module Text.RDF.NTriplesParser(
  parseFile, parseURL, parseString, ParseFailure
)

where

-- TODO: switch to OverloadedStrings and use ByteString literals
import Debug.Trace

import Text.RDF.Core
import Text.RDF.Namespace
import Text.RDF.ParserUtils

import Data.Char(isHexDigit, isLetter, isDigit, isLower, isUpper)
import qualified Data.Map as Map

import Data.ParserCombinators.Attoparsec.Char8

import Data.ByteString.Lazy.Char8(ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC

import Control.Applicative
import Control.Monad
import Prelude hiding (takeWhile)

type Payload = ([Maybe Triple], Maybe BaseUrl, PrefixMappings)

between_chars :: Char -> Char -> Parser ByteString -> Parser ByteString
between_chars start end parser = char start >> parser >>= \res -> char end >> return res

-- We define or redefine all here using same names as the spec, but with an
-- 'nt_' prefix in order to avoid name clashes (e.g., ntripleDoc becomes
-- nt_ntripleDoc).

-- |nt_ntripleDoc is simply zero or more lines.
nt_ntripleDoc :: Parser Payload
nt_ntripleDoc =
    manyTill nt_line eof >>= \lines ->
        return (lines, Nothing, PrefixMappings Map.empty)

--many :: Parser a -> Parser [a]
--many p =
--  do{ x <- p; xs <- many p; return (x:xs) }


nt_line :: Parser (Maybe Triple)
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
nt_comment :: Parser (Maybe Triple)
nt_comment   = (char '#') >> skipMany nt_character >> return Nothing

-- A triple consists of whitespace-delimited subject, predicate, and object,
-- followed by optional whitespace and a period, and possibly more
-- whitespace.
nt_triple :: Parser (Maybe Triple)
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
    return $ Just (triple subj pred obj)

-- A literal is either a language literal (with optional language
-- specified) or a datatype literal (with required datatype
-- specified). The literal value is always enclosed in double
-- quotes. A language literal may have '@' after the closing quote,
-- followed by a language specifier. A datatype literal follows
-- the closing quote with ^^ followed by the URI of the datatype.
nt_literal :: Parser LValue
nt_literal = 
  do lit_str <- between_chars '"' '"' inner_literal 
     (char '@' >> nt_language >>=  return . plainLL lit_str) <|>
       (count 2 (char '^') >> nt_uriref >>= return . typedL lit_str . mkFastString) <|>
       (return $ plainL lit_str)

  where inner_literal = (manyTill inner_string (lookAhead $ char '"') >>= return . BC.concat)

-- A language specifier of a language literal is any number of lowercase
-- letters followed by any number of blocks consisting of a hyphen followed
-- by one or more lowercase letters or digits.
nt_language :: Parser ByteString
nt_language =
  do str <- takeWhile (\c -> c == '-' || isLower c)
     if BC.null str || BC.last str == '-' || BC.head str == '-'
        then fail ("Invalid language string: '" ++ BC.unpack str ++ "'")
        else return str

test :: Parser a -> String -> IO a
test p str =
  do
    when (not $ BC.null rest)
      (putStr "Remainder: '" >> BC.putStr rest >> putStr "\n")
    case result of
      (Left err) -> putStr "ParseError: '" >> putStr err >> putStr "\n" >> error ""
      (Right a)  -> return a
  where (rest, result) = parse p (BC.pack str)

combine :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
combine f parser1 parser2 = 
  do p1 <- parser1
     p2 <- parser2
     return $ f p1 p2

lower :: Parser Char
lower = satisfy isLower

-- nt_empty is a line that isn't a comment or a triple. They appear in the
-- parsed output as Nothing, whereas a real triple appears as (Just triple).
nt_empty :: Parser (Maybe Triple)
nt_empty     = skipMany nt_space >> return Nothing

-- A subject is either a URI reference for a resource or a node id for a
-- blank node.
nt_subject :: Parser Node
nt_subject   =
  (nt_uriref >>= return . unode) <|>
  (nt_nodeID >>= return . bnode)

-- A predicate may only be a URI reference to a resource.
nt_predicate :: Parser Node
nt_predicate = nt_uriref >>= return . unode

-- An object may be either a resource (represented by a URI reference),
-- a blank node (represented by a node id), or an object literal.
nt_object :: Parser (Node)
nt_object =
  (nt_uriref >>=  return . unode) <|>
  (nt_nodeID >>=  return . bnode) <|>
  (nt_literal >>= return . LNode)

-- A URI reference is one or more nrab_character inside angle brackets.
nt_uriref :: Parser ByteString
nt_uriref = between_chars '<' '>' (takeWhile1 (\c -> c /= '>'))

-- A node id is "_:" followed by a name.
nt_nodeID :: Parser ByteString
nt_nodeID = char '_' >> char ':' >> nt_name >>= \n -> 
                return ('_' `BC.cons'` (':' `BC.cons'` n))

-- A name is a letter followed by any number of alpha-numeric characters.
nt_name :: Parser ByteString
nt_name =
  do init <- letter
     rest <- takeWhile isLetterOrDigit
     return (init `BC.cons'` rest)

isLetterOrDigit :: Char -> Bool
isLetterOrDigit c = isLetter c || isDigit c

alpha_num :: Parser Char
alpha_num = letter <|> digit

-- An nt_character is any character except a double quote character.
nt_character :: Parser Char
nt_character   =   satisfy is_nonquote_char

-- A nrab_character is a character that isn't a right angle bracket (this
-- is used where we are inside a URIref, where right angle brackets are
-- not allowed).
nrab_character :: Parser Char
nrab_character =   satisfy (\c -> c /= '>' && is_character c)

-- A character is any Unicode value from ASCII space to decimal 126 (tilde).
is_character :: Char -> Bool
is_character c =   c >= '\x0020' && c <= '\x007E'

-- A non-quote character is a character that isn't the double-quote character.
is_nonquote_char :: Char -> Bool
is_nonquote_char c = is_character c && c/= '"'

-- End-of-line consists of either lf or crlf.
-- We also test for eof and consider that to match as well.
nt_eoln :: Parser ()
nt_eoln = 
 do eof <|> (nt_cr >> nt_lf >> return ()) <|> (nt_lf >> return ())
-- try (nt_cr >> nt_lf >> return ()) <|> try (nt_lf >> return ()) <|> eof

-- Whitespace is either a space or tab character. We must avoid using the
-- built-in space combinator here, because it includes newline.
nt_space :: Parser Char
nt_space = char ' ' <|> nt_tab

-- Carriage return is \r.
nt_cr :: Parser Char
nt_cr          =   char '\r'

-- Line feed is \n.
nt_lf :: Parser Char
nt_lf          =   char '\n'

-- Tab is \t.
nt_tab :: Parser Char
nt_tab         =   char '\t'

-- An inner_string is a fragment of a string (this is used inside double
-- quotes), and consists of the non-quote characters allowed and the
-- standard escapes for a backslash (\\), a tab (\t), a carriage  return (\r),
-- a newline (\n), a double-quote (\"), a 4-digit Unicode escape (\uxxxx
-- where x is a hexadecimal digit), and an 8-digit Unicode escape
-- (\Uxxxxxxxx where x is a hexadecimaldigit).
inner_string :: Parser ByteString
inner_string =
  try (char '\\' >>
         ((char 't' >> return b_tab)  <|>
          (char 'r' >> return b_ret)  <|>
          (char 'n' >> return b_nl)  <|>
          (char '\\' >> return b_slash) <|>
          (char '"' >> return b_quote)   <|>
          (char 'u' >> count 4 hexDigit >>= \cs -> return $ BC.pack ('\\':'u':cs)) <|>
          (char 'U' >> count 8 hexDigit >>= \cs -> return $ BC.pack ('\\':'U':cs))))
  <|> (takeWhile (\c -> is_nonquote_char c && c /= '\\'))

b_tab = BC.singleton '\t'
b_ret = BC.singleton '\r'
b_nl  = BC.singleton '\n'
b_slash = BC.singleton '\\'
b_quote = BC.singleton '"'

hexDigit :: Parser Char
hexDigit = satisfy isHexDigit

digitParser :: Parser [Char]
digitParser = manyTill digit space

parseURL :: Graph gr => String -> IO (Either ParseFailure gr)
parseURL url = _parseURL parseString url

parseFile :: Graph gr => String -> IO (Either ParseFailure gr)
parseFile path = BC.readFile path >>= return . parse nt_ntripleDoc >>= return . handleParse mkGraph

parseString :: Graph gr =>
               ByteString -> 
               Either ParseFailure gr
parseString bs = handleParse mkGraph (parse nt_ntripleDoc bs)

handleParse :: Graph gr => (Triples -> Maybe BaseUrl -> PrefixMappings -> gr) ->
                           (ByteString, Either ParseError Payload) ->
                           (Either ParseFailure gr)
handleParse _mkGraph (rem, result)
  | BC.length rem /= 0 = error ("Leftover JUNK: " ++ BC.unpack rem ++ "\n")
  | otherwise          = 
      case result of
        Left err               -> Left  $ error ("Parse error: " ++ err)
        Right (ts, mbUrl, pms) -> Right $ _mkGraph (conv ts) mbUrl pms
  where
    conv []            = []
    conv (Nothing:ts)  = conv ts
    conv ((Just t):ts) = t : conv ts
