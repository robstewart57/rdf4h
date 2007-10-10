module TurtleParser where

import RDF
import Text.ParserCombinators.Parsec

import Control.Monad

{-

EBNF from <http://www.dajobe.org/2004/01/turtle/>
[1]	turtleDoc 	::= 	statement*
[2]	statement 	::= 	directive ws* '.' ws* | triples ws* '.' ws* | ws+
[3]	directive 	::= 	prefixID | base
[4]	prefixID 	::= 	'@prefix' ws+ prefixName? ':' ws+ uriref
[5]	base 	::= 	'@base' ws+ uriref
[6]	triples 	::= 	subject ws+ predicateObjectList
                                Provides RDF triples using the given subject and each pair from the predicateObjectList
[7]	predicateObjectList 	::= 	verb ws+ objectList ( ws* ';' ws* verb ws+ objectList )* (ws* ';')?
                                Provides a sequence of (verb, object) pairs for each object from the objectList
[8]	objectList 	::= 	object (ws* ',' ws* object)*
                                Provides a sequence of objects
[9]	verb 	::= 	predicate | 'a'
                                where 'a' is equivalent to the uriref <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>
[10]	comment 	::= 	'#' ( [^#xA#xD] )*
[11]	subject 	::= 	resource | blank
[12]	predicate 	::= 	resource
[13]	object 	::= 	resource | blank | literal
[14]	literal 	::= 	quotedString ( '@' language )? | datatypeString | integer | double | decimal | boolean
[15]	datatypeString 	::= 	quotedString '^^' resource
[16]	integer 	::= 	('-' | '+') ? [0-9]+
                                Interpreted as an xsd:integer and generates a datatyped literal with the datatype uriref http://www.w3.org/2001/XMLSchema#integer and canonical lexical representation of xsd:integer which includes allowing no leading zeros.
[17]	double 	::= 	('-' | '+') ? ( [0-9]+ '.' [0-9]* exponent | '.' ([0-9])+ exponent | ([0-9])+ exponent )
                                Interpreted as an xsd:double and generates a datatyped literal with the datatype uriref http://www.w3.org/2001/XMLSchema#double and any legal lexical representation of xsd:double.
[18]	decimal 	::= 	('-' | '+')? ( [0-9]+ '.' [0-9]* | '.' ([0-9])+ | ([0-9])+ )
                                Interpreted as an xsd:decimal and generates a datatyped literal with the datatype uriref http://www.w3.org/2001/XMLSchema#decimal and any legal lexical representation of xsd:decimal.
[19]	exponent 	::= 	[eE] ('-' | '+')? [0-9]+
[20]	boolean 	::= 	'true' | 'false'
                                Interpreted as an xsd:boolean and generates a datatyped literal with the datatype uriref http://www.w3.org/2001/XMLSchema#boolean and canonical lexical representation of xsd:boolean.
[21]	blank 	::= 	nodeID | '[]' | '[' ws* predicateObjectList ws* ']' | collection
                                Provides a blank node either from the given nodeID, a generated one, a generated one which is also used to provide the subject of RDF triples for each pair from the predicateObjectList or the root of the collection.
[22]	itemList 	::= 	object (ws+ object)*
                                Provides a sequence of objects (Note there are no commas between items unlike objectList)
[23]	collection 	::= 	'(' ws* itemList? ws* ')'
                                Provides a blank node at the start of an RDF collection of the objects in the itemList. See section Collections for the triples generated.
[24]	ws 	::= 	#x9 | #xA | #xD | #x20 | comment
[25]	resource 	::= 	uriref | qname
[26]	nodeID 	::= 	'_:' name
[27]	qname 	::= 	prefixName? ':' name?
                                See section QNames
[28]	uriref 	::= 	'<' relativeURI '>'
[29]	language 	::= 	[a-z]+ ('-' [a-z0-9]+ )*
                                encoding a language tag.
[30]	nameStartChar 	::= 	[A-Z] | "_" | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
[31]	nameChar 	::= 	nameStartChar | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
[32]	name 	::= 	nameStartChar nameChar*
[33]	prefixName 	::= 	( nameStartChar - '_' ) nameChar*
[34]	relativeURI 	::= 	ucharacter*
                                Used as a relative URI and resolved against the current base URI to give an absolute URI reference.
[35]	quotedString 	::= 	string | longString
[36]	string 	::= 	#x22 scharacter* #x22
[37]	longString 	::= 	#x22 #x22 #x22 lcharacter* #x22 #x22 #x22
[38]	character 	::= 	'\u' hex hex hex hex |
                                '\U' hex hex hex hex hex hex hex hex |
                                '\\' |
                                [#x20-#x5B] | [#x5D-#x10FFFF]
                                See String Escapes for full details.
[39]	echaracter 	::= 	character | '\t' | '\n' | '\r'
                                See String Escapes for full details.
[40]	hex 	::= 	[#x30-#x39] | [#x41-#x46]
                                hexadecimal digit (0-9, uppercase A-F)
[41]	ucharacter 	::= 	( character - #x3E ) | '\>'
[42]	scharacter 	::= 	( echaracter - #x22 ) | '\"'
[43]	lcharacter 	::= 	echaracter | '\"' | #x9 | #xA | #xD
-}

t_predicateObjectList = undefined
t_comment = undefined



t_object = t_resource <|> t_blank <|> t_literal
t_literal = undefined
t_datatypeString = 
  do s <- t_quotedString
     string "^^"
     r <- t_resource >>= \(UNode uri) -> return uri
     return (LNode $ TypedL s r)
t_integer = 
  do sign <- sign_parser;
     ds <- many1 digit;
     return (sign ++ ds)
t_double =
  do sign <- sign_parser
     rest <- do { ds <- many1 digit;  char '.'; ds' <- many digit; e <- t_exponent; return (ds ++ "." ++ ds' ++ e) }
             <|> do { char '.'; ds <- many1 digit; e <- t_exponent; return ('.':ds ++ e) }
             <|> do { ds <- many1 digit; e <- t_exponent; return (ds ++ e) }
     return (sign ++ rest)

sign_parser = option "" (oneOf "-+" >>= (\c -> return (c:[]))) 
digits1_parser = many1 digit

t_decimal = 
  do sign <- sign_parser
     rest <- try (do ds <- many digit; char '.'; ds' <- option "" (many digit); return (ds ++ ('.':ds')))
             <|> do { char '.'; ds <- many1 digit; return ('.':ds) }
             <|> many1 digit
     return (sign ++ rest)
     
t_exponent = do e <- oneOf "eE"; 
                s <- option "" (oneOf "-+" >>= \c -> return (c:[]))
                ds <- many1 digit; 
                return (e:(s++ds))
t_boolean = (string "true" >> return True) <|> (string "false" >> return False)
t_blank = try (do t_nodeID >>= return . BNode)
          <|> try (string "[]" >>= return . BNode)
          <|> (predObjList >> return (BNode ""))
          <|> (t_collection >> return (BNode ""))
  where
    predObjList = do { many t_ws; l <- t_predicateObjectList; many t_ws; return l}

t_itemList = 
  do obj1 <- t_object
     objs <- many (many1 t_ws >> t_object)
     return (obj1:objs)

t_collection :: GenParser Char st [Node]
t_collection = between (char '(') (char ')') g
  where
    g = do { many t_ws; l <- option [] t_itemList; many t_ws; return l }

t_ws = char '\x000A' <|> char '\x000D' <|> char '\x0020' <|> t_comment

t_resource = t_uriref <|> t_qname

t_nodeID = do { string "_:"; cs <- t_name; return ("_:" ++ cs) }

t_qname = 
  do pre <- option "" t_prefixName
     char ':'
     name <- option "" t_name
     return (UNode $ pre ++ (':' : name)) -- FIXME:

t_uriref = between (char '<') (char '>') t_relativeURI >>= return . UNode

t_language = 
  do init <- many1 lower;
     rest <- many (do {char '-'; cs <- many1 (lower <|> digit); return ('-':cs)})
     return (init ++ concat rest)


t_nameStartChar = char '_' <|> satisfy (flip in_range blocks)
  where blocks = [('A', 'Z'), ('a', 'z'), ('\x00C0', '\x00D6'), 
                  ('\x00D8', '\x00F6'), ('\x00F8', '\x02FF'),
                  ('\x0370', '\x037D'), ('\x037F', '\x1FFF'),
                  ('\x200C', '\x200D'), ('\x2070', '\x218F'),
                  ('\x2C00', '\x2FEF'), ('\x3001', '\xD7FF'),
                  ('\xF900', '\xFDCF'), ('\xFDF0', '\xFFFD'),
                  ('\x10000', '\xEFFFF')]

t_nameChar = t_nameStartChar <|> char '-' <|> char '\x00B7' <|> satisfy f
  where f = flip in_range [('0', '9'), ('\x0300', '\x036F'), ('\x203F', '\x2040')]
            
in_range :: Char -> [(Char, Char)] -> Bool
in_range c = any (\(c1, c2) -> c >= c1 && c <= c2)

t_name = do { c <- t_nameStartChar; cs <- many t_nameChar; return (c:cs) }

t_prefixName = do { c <- t_nameStartChar; cs <- many t_nameChar; return (c:cs) }

t_relativeURI = many t_ucharacter >>= return . concat

t_quotedString = t_longString <|> t_string

t_string = do { char '"'; schars <- many t_scharacter; char '"'; return (concat schars) }

t_longString = try (do {count 3 (char '"'); strs <- many t_lcharacter; count 3 (char '"'); return (concat strs)})

t_character = unicode_escape <|> non_ctrl_char_except ['\\']

t_echaracter = try (do char '\\'; c <- oneOf ['t', 'n', 'r']; return ('\\':c:[])) <|>
               t_character

t_hex = satisfy (\c -> (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F')) <?> "hexadecimal digit"

t_ucharacter = 
  do try (string "\\>" >>= return) <|> unicode_escape <|> non_ctrl_char_except ['\\', '>']
                  
t_scharacter =
  do try (string "\\\"" >>= return) 
     <|> try  (do char '\\'; c <- oneOf ['t', 'n', 'r']; return ('\\':c:[])) -- echaracter part 1
     <|> unicode_escape <|> non_ctrl_char_except ['\\', '"'] -- echaracter part 2 minus "

t_lcharacter = t_echaracter <|> (oneOf ['"', '\x0009', '\x000A', '\x000D'] >>= (\c -> return [c]))

unicode_escape = 
  do {
     char '\\';
     do { char '\\'; return "\\\\"; } <|>
     do { char 'u'; chrs <- count 4 t_hex; return ('\\':'u':chrs) } <|>
     do { char 'U'; chrs <- count 8 t_hex; return ('\\':'U':chrs) }
  }
       
non_ctrl_char_except cs = 
  do c <- satisfy (\c -> c <= '\x10FFFF' && (c >= '\x0020' && c `notElem` cs))
     return [c]

