module TurtleParser where

import RDF hiding (Object)
import Namespace
import ParserUtils
import Text.ParserCombinators.Parsec
import Data.Set.AVL (Set)
import qualified Data.Set.AVL as Set
import Data.Map.AVL (Map)
import qualified Data.Map.AVL as Map
import Data.Maybe(isJust, fromJust)
import Text.Printf

import AvlGraph -- FIXME: just for testing
import Foreign  -- FIXME: "
import Control.Monad
--import qualified Control.Monad.State as S

{-

EBNF from <http://www.dajobe.org/2004/01/turtle/>
[1]	turtleDoc 	::= 	statement*
[2]	statement 	::= 	directive ws* '.' ws* | triples ws* '.' ws* | ws+
[3]	directive 	::= 	prefixID | base
[4]	prefixID 	::= 	'@prefix' ws+ prefixName? ':' ws+ uriref
[5]	base            ::=     '@base' ws+ uriref
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
data Statement = S_Directive Directive
               | S_Triples T_Triples
  deriving Show
type Statements = [Statement]
data Directive = D_BaseUrl String
               | D_PrefixId (String, String)
  deriving Show
data Blank = B_BlankId String
           | B_BlankGen Int              -- int is genid used for []
           | B_POList Int POList         -- int is genid used for [ :p1 :o1; :p2 :o2 ]
           | B_Collection [(Int,Object)] -- int is genid used for ( :obj1 :obj2 )
  deriving Show
type POList = [(Resource, [Object])]
type Collection = [Object]
data Object = O_Resource Resource
            | O_Blank Blank
            | O_Literal Node
  deriving Show
data Resource = R_URIRef String
              | R_QName String String
              | R_Blank Blank
  deriving Show
type T_Triples     = (Resource, [(Resource, [Object])])
--type T_TriplesList = [T_Triples]
type ResourceWithId = (Maybe Int, Resource)

t_turtleDoc = many t_statement :: GenParser Char Int [Maybe Statement]

t_statement = (d >>= return . Just) <|> 
              (t >>= return . Just) <|> 
              (many1 t_ws >> (return Nothing))
  where d = do { dir <- t_directive; many t_ws >> (char '.' <?> "end-of-directive") >> many t_ws >> return dir}
        t = do { trp <- t_triples; many t_ws >> (char '.' <?> "end-of-triple") >> many t_ws >> return trp}
      
t_directive = (try t_prefixID <|> try t_base <?> "directive") >>= return . S_Directive

t_prefixID =
  do string "@prefix"
     many1 t_ws <?> "whitespace"
     pn <- option "" t_prefixName
     char ':'
     many1 t_ws
     (R_URIRef uri) <- t_uriref <?> "uriref"
     return $ D_PrefixId (pn, uri)

t_base =
  do string "@base"
     many1 t_ws        <?> "whitespace"
     (R_URIRef uri) <- t_uriref <?> "uriref"
     return $ D_BaseUrl uri

t_triples = 
  do subj <- t_subject <?> "subject"
     many1 t_ws        <?> "whitespace"
     poList <- t_predicateObjectList <?> "predicateObjectList"
     return $ S_Triples (subj, poList)

t_objectList =
  do obj1 <- t_object <?> "object"
     obj_rest <- many (try obj <?> "object")
     return (obj1:obj_rest)
  where obj = many t_ws >> char ',' >> many t_ws >> t_object

t_verb = 
  try t_predicate <|>
  (char 'a' >> return (R_URIRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
  <?> "verb"

t_predicateObjectList = 
  do v1 <- t_verb <?> "verb"
     many1 t_ws   <?> "whitespace"
     ol1 <- t_objectList <?> "objectList" -- cannot fail
     ol_rest <- many $
       try (do v' <- many t_ws >> char ';' >> many t_ws >> t_verb
               ol' <- many1 t_ws >> t_objectList
               return (v', ol'))
     return ((v1, ol1):ol_rest) <?> "predicateObjectList"

t_comment = 
  try (char '#') >> many (satisfy (\c -> c /= '\x000A' && c /= '\x000D')) >> return '#' -- FIXME

t_subject = (t_resource <?> "resource") <|> (t_blank >>= conv <?> "blank")
  where 
    conv (B_Collection _)  = error "Blank Collection not permitted as Subject"
    conv (B_POList i pol)      = return (R_Blank (B_POList i pol)) --error "Subject POList not implemented yet."
    conv blank               = return (R_Blank blank)

t_predicate = t_resource <?> "resource"

t_object = (((try t_resource) <?> "resource") >>= \r -> return (O_Resource r))
           <|> (((try t_blank) <?> "blank") >>= \r -> do i <- getState; updateState (+1); return (O_Blank r))
           <|> (((try t_literal) <?> "literal") >>= \l -> return (O_Literal l))

t_literal = 
  try str_literal <|>
  (do i <- try t_integer; return (LNode $ TypedL i (makeUri xsd "integer"))) <|>
  (do d <- try t_double; return (LNode $ TypedL d (makeUri xsd "double")))   <|>
  (do d <- try t_decimal; return (LNode $ TypedL d (makeUri xsd "decimal"))) <|>
  (do b <- try t_boolean; return (LNode $ TypedL b (makeUri xsd "boolean")))
     
str_literal =
  do 
    str <- try (t_quotedString <?> "quotedString")
    rt <-  rest           
    case rt of
      Nothing                        -> return (LNode $ PlainL str Nothing)
      (Just (Left lng))              -> return (LNode $ PlainL str (Just lng))
      (Just (Right (R_URIRef uri)))  -> return (LNode $ TypedL str uri)
      _                              -> error "t_literal"
  where
    rest = do { try (string "^^"); t_uriref >>= return . Just . Right} <|>
           do { try (char '@'); t_language >>= return . Just . Left} <|>
           return Nothing

t_integer = 
  do sign <- sign_parser <?> "+-"
     ds <- many1 digit   <?> "digit"
     notFollowedBy (char '.')
     return (sign ++ ds)

t_double =
  do sign <- sign_parser <?> "+-"
     rest <- try (do { ds <- many1 digit <?> "digit";  char '.'; ds' <- many digit <?> "digit"; e <- t_exponent <?> "exponent"; return (ds ++ "." ++ ds' ++ e) }) <|>
             try (do { char '.'; ds <- many1 digit <?> "digit"; e <- t_exponent <?> "exponent"; return ('.':ds ++ e) }) <|>
             try (do { ds <- many1 digit <?> "digit"; e <- t_exponent <?> "exponent"; return (ds ++ e) })
     return (sign ++ rest)

sign_parser = option "" (oneOf "-+" >>= (\c -> return (c:[]))) 
digits1_parser = many1 digit

t_decimal = 
  do sign <- sign_parser
     rest <- try (do ds <- many digit <?> "digit"; char '.'; ds' <- option "" (many digit); return (ds ++ ('.':ds')))
             <|> try (do { char '.'; ds <- many1 digit <?> "digit"; return ('.':ds) })
             <|> many1 digit <?> "digit"
     return (sign ++ rest)
     
t_exponent = do e <- oneOf "eE"
                s <- option "" (oneOf "-+" >>= \c -> return (c:[]))
                ds <- many1 digit; 
                return (e:(s++ds))

t_boolean = try (string "true" <|> string "false")

t_blank = (try t_nodeID >>= return . B_BlankId)
          <|> try (do string "[]"; n <- getState; updateState (+1); return (B_BlankGen n))
          <|> try (do char '['; poList <- predObjList; n <- getState; updateState (+1); char ']'; return (B_POList n poList))
          <|> (do coll <- t_collection;  ids <- getIds (length coll); return $ B_Collection (zip ids coll))
  where
    predObjList = do { many t_ws; l <- t_predicateObjectList; many t_ws; return l}
    getIds n = mapM (\_ -> do i <- getState; updateState (+1); return i) $ replicate n False

t_itemList = 
  do obj1 <- t_object <?> "object"
     objs <- many (try $ many1 t_ws >> try t_object) <?> "object"
     return (obj1:objs)

t_collection = try $ between (char '(') (char ')') g
  where
    g = do { many t_ws; l <- option [] t_itemList; many t_ws; return l }

t_ws = char '\x000A' <|> char '\x000D' <|> char '\x0020' <|> t_comment <?> "whitespace"

t_resource = t_uriref <|> t_qname

t_nodeID = do { string "_:"; cs <- t_name; return ("_:" ++ cs) }

t_qname = 
  do pre <- option "" t_prefixName
     char ':'
     name <- option "" t_name
     return (R_QName pre name)

t_uriref = between (char '<') (char '>') t_relativeURI >>= return . R_URIRef

t_language = 
  do init <- many1 lower;
     rest <- many (do {char '-'; cs <- many1 (lower <|> digit); return ('-':cs)})
     return (init ++ concat rest)


t_nameStartChar = try (char '_') <|> t_nameStartCharMinusUnderscore

t_nameStartCharMinusUnderscore = 
  try (satisfy (flip in_range blocks))
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

t_prefixName = do { c <- t_nameStartCharMinusUnderscore; cs <- many t_nameChar; return (c:cs) }

t_relativeURI = many t_ucharacter >>= return . concat

t_quotedString = try t_longString <|> try t_string

t_string = 
  do char '"'
     schars <- many t_scharacter
     char '"'
     return (concat schars)

t_longString = 
  do tripleQuote
     strs <- manyTill longString_char (try tripleQuote)
     -- the manyTill already read the closing quotes, so don't read again
     return (concat strs)
  where
    tripleQuote = count 3 (char '"')

longString_char  = 
  (try $ string "\\\"" >> return "\"") <|>
  (try $ oneOf ['\x0009', '\x000A', '\x000D'] >>= return . (:[]))         <|>  -- \r|\n|\t as single char
  (try $ char '\\' >> oneOf ['t', 'n', 'r', '\\'] >>= \c -> return ('\\':c:[])) <|>
  (try $ non_ctrl_char_except ['\\']) <|> 
  (try unicode_escape)

t_character =  try (non_ctrl_char_except ['\\']) <|> try (string "\\") <|> try unicode_escape

t_echaracter = try( do { (char '\\'); c <- oneOf ['t', 'n', 'r']; return ('\\':c:[]);}) <|>
               t_character
               

t_hex = satisfy (\c -> (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F')) <?> "hexadecimal digit"

t_ucharacter = 
  do try (string "\\>") <|> try unicode_escape <|> try (non_ctrl_char_except ['\\', '>'])
                  
t_scharacter =
  do try (string "\\\"") 
     <|> try  (do char '\\'; c <- oneOf ['t', 'n', 'r']; return ('\\':c:[])) -- echaracter part 1
     <|> unicode_escape <|> non_ctrl_char_except ['\\', '"']         -- echaracter part 2 minus "

-- characters used in long strings
t_lcharacter = 
  t_echaracter <|> 
  -- FIXME: doesn't work with 2 embedded quotes
  (char '"' >> notFollowedBy  (char '"') >>  (return "\"")) <|>
  (oneOf ['\x0009', '\x000A', '\x000D'] >>= \c -> return [c])

unicode_escape = 
  do {
     try $ char '\\';
     do { char '\\'; return "\\\\"; } <|>
     do { char 'u'; chrs <- count 4 t_hex; return ('\\':'u':chrs) } <|>
     do { char 'U'; chrs <- count 8 t_hex; return ('\\':'U':chrs) }
  }
       
non_ctrl_char_except cs = 
  satisfy (\c -> c <= '\x10FFFF' && (c >= '\x0020' && c `notElem` cs)) >>= 
    return . (:[])

--

post_process :: Maybe BaseUrl -> Either ParseError [Maybe Statement]
                -> Either ParseError (Triples, Maybe BaseUrl, PrefixMappings)
post_process _baseUrl _result = 
  case _result of
    (Left err)     ->  Left err
    (Right sts)    ->  Right $ post_process' _baseUrl $ map fromJust $ filter isJust sts

post_process' :: Maybe BaseUrl -> Statements -> (Triples, Maybe BaseUrl, PrefixMappings)
post_process' baseUrl stmts = f $ post_process'' ([], baseUrl, Map.empty) stmts
  where f (ts, lastBaseUrl, pms) = (ts, baseUrl, pms)


post_process'' :: (Triples, Maybe BaseUrl, PrefixMappings) -> Statements ->
                  (Triples, Maybe BaseUrl, PrefixMappings)
post_process'' tup [] = tup
post_process'' (ts, currBaseUrl, pms) (stmt:stmts) =
  case stmt of
    S_Directive sdir -> 
      case sdir of
        D_PrefixId (pre,url) -> post_process'' (ts, currBaseUrl, Map.insert pre url pms) stmts
        D_BaseUrl url        -> post_process'' (ts, (Just $ BaseUrl url), pms) stmts
    S_Triples strp    ->
      let (new_ts, new_baseUrl, newPms) = process_ts (ts, currBaseUrl, pms) strp
      in post_process'' (new_ts ++ ts, new_baseUrl, newPms) stmts

process_ts :: (Triples, Maybe BaseUrl, PrefixMappings) -> (Resource, [(Resource, [Object])]) ->
              (Triples, Maybe BaseUrl, PrefixMappings)
process_ts (ts, bUrl, pms) (subj, poLists) = (new_ts ++ ts, bUrl, pms)
  where
    new_ts = convertPOLists bUrl pms (subj, poLists)

-- When first encountering a POList, we convert each of the lists within the list and concatenate the result.
convertPOLists :: Maybe BaseUrl -> PrefixMappings -> (Resource, [(Resource, [Object])]) -> Triples
convertPOLists bUrl pms (subj, poLists) = concatMap (convertPOListItem bUrl pms subj) poLists

-- For a POList item within a POList, we convert the individual item to a list of triples.
convertPOListItem :: Maybe BaseUrl -> PrefixMappings -> Resource -> (Resource, [Object]) -> Triples
convertPOListItem bUrl pms subj (pred, objs) = concatMap (convertObjectListItem bUrl pms subj pred) objs

-- For a given object list item that is the object of supplied subject and predicate, convert it into a list
-- of triples.
convertObjectListItem :: Maybe BaseUrl -> PrefixMappings -> Resource -> Resource -> Object -> Triples
convertObjectListItem bUrl pms subj pred obj
  -- if we have a bnode po list as the subject, we create a set of triples with a bnode as subject
  -- and all the pred-obj pairs as pred and obj; and additionally create a set of triples representing
  -- the bnode as subj, the curr pred as pred, and each object (in possibly a collection) as object.
  | isBNodeListSubject subj = (\(R_Blank (B_POList i' polist')) ->  convertObjectListItem bUrl pms (R_Blank $ B_BlankGen i') pred obj
                                                                      ++ concatMap (convertPOListItem bUrl pms (R_Blank $ B_BlankGen i')) polist') 
                                subj
  | isOLiteral        obj   = log `seq` makeTriple' lnode
  | isOResource       obj   = log `seq` makeTriple' objNode
  | isOBlankBNode     obj   = log `seq` makeTriple' bIdNode
  | isOBlankBNodeGen  obj   = log `seq` makeTriple' bGenIdNode
  | isOBlankBNodeColl obj   = convertColl bUrl pms subj pred ocoll -- error "TurtleParser.convertObjectListItem.isOBlankBNodeColl"
  -- if object is a POList, then we create a bnode to use as the object in a triple and cons
  -- that triple onto the triples that represent the POList, each of which has the same bnode
  -- as the subject.
  | isOBlankPOList    obj   = makeTriple' bObjPolGenNode ++ convertPOLists bUrl pms (R_Blank opol, pol)
  | otherwise               = error $ "Unexpected case: " ++ show obj
  where 
    log                                   = False --unsafePerformIO $ putStrLn $ show subj ++ show pred ++ show obj
    makeTriple                            = triple subjNode predNode
    makeTriple'                           = (:[]) . makeTriple
    makeTriple'' s p o                    = triple s p o
    subjNode                              = uriRefQNameOrBlank bUrl pms subj
    predNode                              = uriRefQNameOrBlank bUrl pms pred
    objNode                               = uriRefQNameOrBlank bUrl pms res
    bIdNode                               = BNode bId
    bGenIdNode                            = BNodeGen bGenId
    bObjPolGenNode                        = BNodeGen poObjId
    (O_Literal lnode)                     = obj
    (O_Resource res)                      = obj
    (O_Blank (B_BlankId bId))             = obj
    (O_Blank (B_BlankGen bGenId))         = obj
    (O_Blank ocoll@(B_Collection _))      = obj
    (O_Blank opol@(B_POList poObjId pol)) = obj

{-
Collections are interpreted as follows:

( object1 object2 ) is short for:

[ rdf:first object1; rdf:rest [ rdf:first object2; rdf:rest rdf:nil ] ]

Which expands to the following triples:
:genid0 rdf:first object1 .
:genid0 rdf:rest :genid1 .
:genid1 rdf:first object2 .
:genid1 rdf:rest rdf:nil .
# plus the initial:
:subj :pred :genid0

( ) is short for the resource:
rdf:nil

The Notation3 spec has the following to say on lists:

1. All lists exist. The statement [rdf:first <a>; rdf:rest rdf:nil]. carries no
   information in that the list ( <a> ) exists and this expression carries no 
   new information about it.

2. A list has only one rdf:first. rdf:first is functional. If the same thing
   has rdf:first arcs from it, they must be to nodes which are RDF
   equivalent - are the same RDF node.

3. Lists are the same RDF node if they have the same the:first and the
   same rdf:rest.

-}
convertColl :: Maybe BaseUrl -> PrefixMappings -> Resource -> Resource -> Blank -> Triples
convertColl bUrl pms subj pred (B_Collection objs) =  
  if null objs
     then collTriples
     else initialTriple (head objs) : collTriples
  where initialTriple (initId, initObj) = triple subjNode predNode (BNodeGen initId)
        collTriples = convertColl' objs
        subjNode = uriRefQNameOrBlank  bUrl pms subj
        predNode = uriRefOrQName bUrl pms pred
        -- only called with empty list if collection was empty
        convertColl' :: [(Int, Object)] -> Triples
        convertColl' []                = [triple subjNode predNode rdfNil]
        -- if next elem is nil, then we're finished, and make rdf:next nil
        convertColl' ((i, obj):[])     = triple (BNodeGen i) rdfFirst (bObjToNode obj) :
                                           triple (BNodeGen i) rdfRest rdfNil : []
        convertColl' ((i1,obj1):o2@(i2,obj2):os) = triple (BNodeGen i1) rdfFirst (bObjToNode obj1) :
                                                  triple (BNodeGen i1) rdfRest (BNodeGen i2) : convertColl' (o2:os)
        bObjToNode (O_Resource res) = uriRefOrQName bUrl pms res
        bObjToNode (O_Literal lit)  = lit
        bObjToNode errObj           = error $ "TurtleParser.convertColl: not allowed in collection: " ++ show errObj

rdfNil   = UNode $ makeUri rdf "nil"
rdfFirst = UNode $ makeUri rdf "first"
rdfRest  = UNode $ makeUri rdf "rest"
rdfNext  = UNode $ makeUri rdf "next"

resourceToNode :: Maybe BaseUrl -> PrefixMappings -> Resource -> Node
resourceToNode baseUrl pms res =
  case (baseUrl, res) of
    (Nothing, (R_URIRef uri))  -> UNode $ resolveUri Nothing uri
    (bUrl,    (R_URIRef uri))  -> UNode $ resolveUri bUrl    uri
    (_,       (R_QName p u))   -> UNode $ resolveQName p pms ++ u
    (_,       (R_Blank b))     -> blankToNode b
  where 
    blankToNode (B_BlankId bId) = BNode bId
    blankToNode (B_BlankGen i)  = BNodeGen i
    blankToNode b               = error $ "Cannot convert blank '" ++ show b ++ "' to a single node."

resolveUri :: Maybe BaseUrl -> String -> String
resolveUri Nothing uri | isAbsoluteUri uri = uri
                       | otherwise         = error "No Base URL against which to resolve relative URI"
resolveUri (Just (BaseUrl bUrl)) uri | isAbsoluteUri uri = uri
                                     | otherwise         = bUrl ++ uri

{-
Need to determine if the uriref is really an absolute uri by checking to
see if it begins with a scheme, which would satisfy the regex:
scheme        = alpha *( alpha | digit | "+" | "-" | "." )

alpha is lowercase letters, but rfc says best to normalize uppercase to lower.
-}
isAbsoluteUri uriref = True

isBNodeListSubject (R_Blank (B_POList _ _))    = True
isBNodeListSubject _                         = False

isOResource (O_Resource _)                   = True
isOResource _                                = False
isOLiteral  (O_Literal _)                    = True
isOLiteral  _                                = False
isOBlankBNode    (O_Blank   (B_BlankId _))   = True
isOBlankBNode    _                           = False
isOBlankBNodeGen (O_Blank (B_BlankGen _))    = True
isOBlankBNodeGen _                           = False
isOBlankBNodeColl (O_Blank (B_Collection _)) = True
isOBlankBNodeColl _                          = False
isOBlankPOList (O_Blank (B_POList _ _))        = True
isOBlankPOList _                             = False

uriRefQNameOrBlank :: Maybe BaseUrl -> PrefixMappings -> Resource -> Node
uriRefQNameOrBlank  _        _     (R_URIRef url)            = UNode url
uriRefQNameOrBlank  _        pms   (R_QName pre local)       = UNode $ (resolveQName pre pms) ++ local
uriRefQNameOrBlank  _        _     (R_Blank (B_BlankId bId)) = BNode bId
uriRefQNameOrBlank  bUrl     _     (R_Blank (B_BlankGen genId))  = BNodeGen genId -- TODO: decide how to handle this
uriRefQNameOrBlank  _        _      res                        = error $ show ("TurtleParser.uriRefQNameOrBlank: " ++ show res)

uriRefOrQName :: Maybe BaseUrl -> PrefixMappings -> Resource -> Node
uriRefOrQName _      _     (R_URIRef url)            = UNode url
uriRefOrQName _      pms   (R_QName pre local)       = UNode $ (resolveQName pre pms) ++ local
uriRefOrQName _      _     _                         = error "TurtleParser.predicate"

parseString str = handleResult  $ post_process Nothing result
  where result = runParser t_turtleDoc 0 "" str

handleResult result =
  case result of
    (Left err)                      -> Left (ParseFailure $ show err)
    (Right (ts, baseUrl, prefixes)) -> Right $ mkGraph ts baseUrl prefixes

test :: Int -> IO ()
test testNum = readFile fpath >>= f
  where
    fpath = printf "data/ttl/conformance/test-%02d.ttl" testNum :: String
    f s = case parseString s of
            (Left err) -> putStrLn $ show err
            (Right gr) -> mapM_ (putStrLn . show) (triplesOf (gr::AvlGraph))

resolveQName :: String -> PrefixMappings -> String
resolveQName pre = Map.findWithDefault (error $ "Cannot resolve QName Prefix: " ++ pre) pre


-- Parse the N-Triples document at the given filepath,
-- generating a graph containing the parsed triples.
--parseFile :: Graph gr => String -> IO (Either ParseFailure gr)
--parseFile path = parseFromFile t_turtleDoc path >>= 
--                 return . handleParse mkGraph

-- Parse the N-Triples document at the given URL, 
-- generating a graph containing the parsed triples.
--parseURL :: Graph gr => String -> IO (Either ParseFailure gr)
--parseURL url = _parseURL parseString url

-- Parse the given string as an N-Triples document, 
-- generating a graph containing the parsed triples.
--parseString :: Graph gr => String -> Either ParseFailure gr
--parseString str = handleParse mkGraph $ parse t_turtleDoc "" str 

{-
### Error in:   4:1
Blank POList not permitted as Subject
### Error in:   5:1
user error (HUnit:   input06:ParseFailure "(line 3, column 5):\nunexpected \" \"\nexpecting \"_\", \"-\", \"\\183\" or \":\"")
### Error in:   6:1
user error (HUnit:   input07:ParseFailure "(line 2, column 7):\nunexpected \"(\"\nexpecting whitespace or objectList")
### Error in:   8:1
user error (HUnit:   input09:ParseFailure "(line 6, column 14):\nunexpected \" \"\nexpecting \"_\", \"-\", \"\\183\" or \":\"")
### Error in:   9:1
user error (HUnit:   input10:ParseFailure "(line 1, column 54):\nunexpected \"0\"\nexpecting whitespace or objectList")
### Error in:   16:1
user error (HUnit:   input17:ParseFailure "(line 3, column 7):\nunexpected \"\\\"\"\nexpecting whitespace or objectList")
### Error in:   17:1
user error (HUnit:   input18:ParseFailure "(line 3, column 7):\nunexpected \"\\\"\"\nexpecting whitespace or objectList")
### Error in:   18:1
user error (HUnit:   input19:ParseFailure "(line 3, column 8):\nunexpected \"1\"\nexpecting whitespace or objectList")
### Error in:   19:1
user error (HUnit:   input20:ParseFailure "(line 5, column 7):\nunexpected \"\\\"\"\nexpecting whitespace or objectList")
### Error in:   20:1
user error (HUnit:   input21:ParseFailure "(line 2, column 7):\nunexpected \"1\"\nexpecting whitespace or objectList")
### Error in:   21:1
user error (HUnit:   input22:ParseFailure "(line 2, column 7):\nunexpected \"-\"\nexpecting whitespace or objectList")
### Error in:   22:1
user error (HUnit:   input23:ParseFailure "(line 3, column 7):\nunexpected \"\\\"\"\nexpecting whitespace or objectList")
### Error in:   23:1
user error (HUnit:   input24:ParseFailure "(line 2, column 7):\nunexpected \"t\"\nexpecting whitespace or objectList")
### Error in:   28:1
user error (HUnit:   input29:ParseFailure "(line 1, column 53):\nunexpected \"<\"\nexpecting whitespace or objectList")
Cases: 60  Tried: 60  Errors: 14  Failures: 0
(Counts {cases = 60, tried = 60, errors = 14, failures = 0},0)
-}
