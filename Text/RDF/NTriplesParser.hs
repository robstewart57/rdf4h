{-

This module defines a parser for RDF in N-Triples format.

The most current description of N-Triples (as of Sept. 07) -- that is, the 
one used for creating this module -- is in the 'RDF Test Cases' candidate 
recommendation  <http://www.w3.org/TR/rdf-testcases/#ntriples>.

-}

module Text.RDF.NTriplesParser(
  parseFile, parseURL, parseString, ParseFailure
) 

where

import Text.RDF.Core
import Text.RDF.Namespace
import Text.RDF.ParserUtils

import qualified Data.Map as Map

import Text.ParserCombinators.Parsec

import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as B

import Control.Monad

{-

URIs are not validated syntactically, nor are datatype URIs checked in 
any way. All URIs are treated as opaque strings at present.

The following EBNF grammar for n-triples is taken from
<http://www.w3.org/TR/rdf-testcases/#ntriples>. I've doubled the pipe symbol
and the ampersand symbol in order to escape them for lhs2Tex.

ntripleDoc  	::=  	line*  	
line 	        ::= 	ws* ( comment | triple )? eoln 	
comment 	::= 	'#' ( character - ( cr | lf ) )* 	
triple 	        ::= 	subject ws+ predicate ws+ object ws* '.' ws* 	
subject 	::= 	uriref | nodeID 	
predicate 	::= 	uriref 	
object 	        ::= 	uriref | nodeID | literal 	
uriref 	        ::= 	'<' absoluteURI '>' 	
nodeID 	        ::= 	'_:' name 	
literal 	::= 	langString | datatypeString 	
langString 	::= 	'"' string '"' ( '@' language )?
datatypeString 	::= 	'"' string '"' '^^' uriref 	
language 	::= 	[a-z]+ ('-' [a-z0-9]+ )* /*encoding a language tag.*/	
ws 	        ::= 	space | tab 	
eoln 	        ::= 	cr | lf | cr lf 	
space 	        ::= 	#x20 /* US-ASCII space - decimal 32 */ 	
cr 	        ::= 	#xD /* US-ASCII carriage return - decimal 13 */ 	
lf 	        ::= 	#xA /* US-ASCII line feed - decimal 10 */ 	
tab 	        ::= 	#x9 /* US-ASCII horizontal tab - decimal 9 */ 	
string 	        ::= 	character* with escapes as defined in section Strings 	
name 	        ::= 	[A-Za-z][A-Za-z0-9]* 	
absoluteURI 	::= 	character+ with escapes as defined in section URI References 	
character 	::= 	[#x20-#x7E] /* US-ASCII space to decimal 126 */

-}


-- We define or redefine all here using same names as the spec, but with an
-- 'nt_' prefix in order to avoid name clashes (e.g., ntripleDoc becomes 
-- nt_ntripleDoc).

-- |nt_ntripleDoc is simply zero or more lines.
--nt_ntripleDoc = GenParser Char st (Maybe 
nt_ntripleDoc :: GenParser Char st ([Maybe Triple], Maybe BaseUrl, PrefixMappings)
nt_ntripleDoc = 
  many nt_line >>= \lines ->
  return (lines, Nothing, PrefixMappings Map.empty)

-- |nt_line is optional whitespace followed by either a comment, a triple, or 
-- empty. The 'empty' option is a simple deviation from the EBNF grammar
-- above. It encodes that the comment or triple are optional, as given in
-- the EBNF. Parsec did not like having optional stuff after the skipMany,
-- and this was a workaround.
nt_line :: GenParser Char st (Maybe Triple)
nt_line      = do skipMany nt_space
                  res <- (nt_comment <|>  nt_triple <|> nt_empty)
                  nt_eoln
                  return res

-- A comment consists of an initial # character, followed by any number of 
-- characters except cr or lf. The spec is redundant in specifying that
-- comment is hash followed by "character - (cr | lf)", since character
-- is already defined as the range #x0020-#x007E, so cr #x000D and 
-- lf #x000A are both already excluded. This returns Nothing as we are
-- ignoring comments for now.
nt_comment :: GenParser Char st (Maybe Triple)
nt_comment   = do char '#'; skipMany nt_character; return Nothing;

-- A triple consists of whitespace-delimited subject, predicate, and object,
-- followed by optional whitespace and a period, and possibly more 
-- whitespace.
nt_triple :: GenParser Char st (Maybe Triple)
nt_triple    = 
  do
    subj <- nt_subject
    many1 nt_space
    pred <- nt_predicate
    many1 nt_space
    obj <- nt_object
    many nt_space
    char '.'
    many nt_space
    return $ Just (triple subj pred obj)

-- nt_empty is a line that isn't a comment or a triple. They appear in the 
-- parsed output as Nothing, whereas a real triple appears as (Just triple).
nt_empty :: GenParser Char st (Maybe Triple)
nt_empty     = do skipMany nt_space; return Nothing

-- A subject is either a URI reference for a resource or a node id for a
-- blank node.
nt_subject :: GenParser Char st (Node)
nt_subject   = do {uri <- nt_uriref; return (unode uri);} <|> 
               do {nodeId <- nt_nodeID; return (bnode nodeId);}


-- A predicate may only be a URI reference to a resource.
nt_predicate :: GenParser Char st (Node)
nt_predicate = do uri <- nt_uriref; return (unode uri);

-- An object may be either a resource (represented by a URI reference),
-- a blank node (represented by a node id), or an object literal.
nt_object :: GenParser Char st (Node)
nt_object = do {uri    <- nt_uriref;  return (unode uri) } <|> 
            do {nodeId <- nt_nodeID;  return (bnode nodeId)} <|> 
            do {lit    <- nt_literal; return (LNode lit)}

-- A URI reference is an absolute URI inside angle brackets.
nt_uriref :: GenParser Char st (ByteString)
nt_uriref = do char '<'; uri <- nt_absoluteURI; char '>'; return uri

-- A node id is "_:" followed by a name.
nt_nodeID :: GenParser Char st (ByteString)
nt_nodeID = do string "_:"; n <- nt_name; return $ s2b "_:" `B.append` n

-- A literal is either a language literal (with optional language
-- specified) or a datatype literal (with required datatype
-- specified). The literal value is always enclosed in double
-- quotes. A language literal may have '@' after the closing quote,
-- followed by a language specifier. A datatype literal follows
-- the closing quote with ^^ followed by the URI of the datatype.
nt_literal :: GenParser Char st (LValue)
nt_literal    = 
  do 
    str <- between (char '"') (char '"') nt_string
    rt <- do {char '@'; lng <- nt_language; return (Just (Left lng))} <|> 
          do {string "^^"; uri <- nt_uriref; return (Just (Right uri))} <|>
          do {return Nothing}
    case rt of
      Nothing              -> return (plainL str)
      (Just (Left lng))    -> return (plainLL str lng)
      (Just (Right uri))   -> return (typedL str  (mkFastString uri))

-- A language specifier of a language literal is any number of lowercase
-- letters followed by any number of blocks consisting of a hyphen followed
-- by one or more lowercase letters or digits.
nt_language :: GenParser Char st (ByteString)
nt_language    =   
  do
    str1 <- many1 lower >>= return . s2b; 
    str2 <- many (do { char '-'; 
                       ld <- many (lower <|> digit); 
                       return (s2b $ '-':ld); 
                     })
    return (B.concat (str1:str2))

-- End-of-line consists of either lf or crlf. We left-factored the pattern
-- here to avoid using the try combinator.
nt_eoln :: GenParser Char st ()
nt_eoln        =   do  { nt_lf <|> nt_cr; optional nt_lf; }

-- Whitespace is either a space or tab character. We must avoid using the
-- built-in space combinator here, because it includes newline.
nt_space :: GenParser Char st Char
nt_space       =   char ' ' <|> nt_tab

-- Carriage return is \r, given here as a Unicode escape.
nt_cr :: GenParser Char st Char
nt_cr          =   char '\x000D'

-- Line feed is \n, given here as a Unicode escape.
nt_lf :: GenParser Char st Char
nt_lf          =   char '\x000A'

-- Tab is \t, given here as a Unicode escape.
nt_tab :: GenParser Char st Char
nt_tab         =   char '\x0009'

-- A name is a letter followed by any number of alpha-numeric characters.
nt_name :: GenParser Char st ByteString
nt_name        =   do ltr <- letter; str <- many alphaNum; return $! s2b (ltr:str)

-- An absolute URI is at least 1 nrab_character. We do not attempt to 
-- validate the URI at all, so anything that parses is accepted.
nt_absoluteURI :: GenParser Char st ByteString
nt_absoluteURI =   many1 nrab_character >>= return . s2b

-- An nt_character is any character except a double quote character.
nt_character :: GenParser Char st Char
nt_character   =   satisfy is_nonquote_char

-- A nrab_character is a character that isn't a right angle bracket (this
-- is used where we are inside a URIref, where right angle brackets are
-- not allowed).
nrab_character :: GenParser Char st Char
nrab_character =   satisfy (\c -> c /= '>' && is_character c)

-- A character is any Unicode value from ASCII space to decimal 126 (tilde).
is_character :: Char -> Bool
is_character c =   c >= '\x0020' && c <= '\x007E'

-- A non-quote character is a character that isn't the double-quote character.
is_nonquote_char :: Char -> Bool
is_nonquote_char c = is_character c && c/= '"'

-- The nt_string is simply a bunch of inner_string parts concatenated.
nt_string :: GenParser Char st ByteString
nt_string =  many inner_string >>= return . B.concat

-- An inner_string is a fragment of a string (this is used inside double 
-- quotes), and consists of the non-quote characters allowed and the 
-- standard escapes for a backslash (\\), a tab (\t), a carriage  return (\r),
-- a newline (\n), a double-quote (\"), a 4-digit Unicode escape (\uxxxx 
-- where x is a hexadecimal digit), and an 8-digit Unicode escape
-- (\Uxxxxxxxx where x is a hexadecimaldigit).
inner_string :: GenParser Char st ByteString
inner_string   = 
  try (do {
       char '\\'; 
       do {c <- oneOf ['t', 'r', 'n', '\\', '"']; return $ s2b ('\\':c:[])}  <|>
       do {char 'u'; chrs <- count 4 hexDigit; return $ s2b ('\\':'u':chrs)} <|>
       do {char 'U'; chrs <- count 8 hexDigit; return $ s2b ('\\':'U':chrs)}
  })  <|> do {c <- satisfy is_nonquote_char; return (B.singleton c)} 

-- ==========================================================
-- ==  END OF PARSER COMBINATORS AND SUPPORTING FUNCTIONS  ==
-- ==========================================================


-- |Parse the N-Triples document at the given filepath,
-- generating a graph containing the parsed triples.
parseFile :: Graph gr => String -> IO (Either ParseFailure gr)
parseFile path = parseFromFile nt_ntripleDoc path >>= return . handleParse mkGraph

-- |Parse the N-Triples document at the given URL, 
-- generating a graph containing the parsed triples.
parseURL :: Graph gr => String -> IO (Either ParseFailure gr)
parseURL url = _parseURL parseString url

-- |Parse the given string as an N-Triples document, 
-- generating a graph containing the parsed triples.
parseString :: Graph gr => String -> Either ParseFailure gr
parseString str = handleParse mkGraph (parse nt_ntripleDoc "" str)

handleParse :: Graph gr => (Triples -> Maybe BaseUrl -> PrefixMappings -> gr) ->
                           Either ParseError ([Maybe Triple], Maybe BaseUrl, PrefixMappings) ->
                           (Either ParseFailure gr)
handleParse _        (Left err) = Left $ ParseFailure $ show err
handleParse _mkGraph (Right (ts, baseUrl, prefixes)) = 
  Right $ _mkGraph (conv ts) baseUrl prefixes
  where
    conv [] = []
    conv (Nothing:ts)  = conv ts
    conv ((Just t):ts) = t : conv ts
