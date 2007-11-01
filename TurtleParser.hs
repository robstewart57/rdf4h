module TurtleParser where

import RDF hiding (Object)
import Utils
import Namespace
import ParserUtils
import Text.ParserCombinators.Parsec
import qualified Data.Map.AVL as Map
import Data.Maybe(isJust, fromJust)
import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as B

import Control.Monad


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
data Directive = D_BaseUrl ByteString
               | D_PrefixId (ByteString, ByteString)
  deriving Show
data Blank = B_BlankId ByteString
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
data Resource = R_URIRef ByteString
              | R_QName ByteString ByteString
              | R_Blank Blank
  deriving Show
type T_Triples     = (Resource, [(Resource, [Object])])

type ParseState = (Maybe BaseUrl, Int)

t_turtleDoc :: GenParser Char ParseState (IO [Maybe Statement])
t_turtleDoc =  many t_statement >>= \ts -> eof >> (return $ mapM id ts)

     
t_statement :: GenParser Char ParseState (IO (Maybe Statement))
t_statement = (d >>= return . liftM Just) <|> 
              (t >>= return . liftM Just) <|> 
              (many1 t_ws >> (return $ return Nothing))
  where d = do { dir <- t_directive; many t_ws >> (char '.' <?> "end-of-directive") >> many t_ws >> return dir}
        t = do { trp <- t_triples; many t_ws >> (char '.' <?> "end-of-triple") >> many t_ws >> return trp}

t_directive :: GenParser Char ParseState (IO Statement)
t_directive = 
  do d <- (try t_prefixID <|> try t_base <?> "directive")
     return $ d >>=  return . S_Directive

t_prefixID :: GenParser Char ParseState (IO Directive)
t_prefixID =
  do string "@prefix"
     many1 t_ws <?> "whitespace"
     pn <- option (s2b "") t_prefixName
     char ':'
     many1 t_ws
     uriref <- t_uriref
     return $ uriref >>= \(R_URIRef uri) -> return (D_PrefixId (pn, uri))

t_base :: GenParser Char ParseState (IO Directive)
t_base =
  do string "@base"
     many1 t_ws        <?> "whitespace"
     uriref <- t_uriref
     return $ uriref >>= \(R_URIRef uri) -> return (D_BaseUrl uri)

t_triples :: GenParser Char ParseState (IO Statement)
t_triples = 
  do subj <- t_subject <?> "subject"
     many1 t_ws        <?> "whitespace"
     poList <- t_predicateObjectList <?> "predicateObjectList"
     return (subj >>= \s -> poList >>= \pol -> (return (S_Triples (s, pol))))

t_objectList :: GenParser Char ParseState (IO [Object])
t_objectList =
  do obj1 <- t_object <?> "object"
     obj_rest <- many (try obj <?> "object")
     return $ mapM id (obj1:obj_rest)
  where obj = many t_ws >> char ',' >> many t_ws >> t_object

t_verb :: GenParser Char ParseState (IO Resource)
t_verb = 
  try t_predicate <|>
  (char 'a' >> return (return (R_URIRef $ s2b "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")))
  <?> "verb"

t_predicateObjectList :: GenParser Char ParseState (IO [(Resource, [Object])])
t_predicateObjectList = 
  do v1 <- t_verb <?> "verb"
     many1 t_ws   <?> "whitespace"
     ol1 <- t_objectList <?> "objectList" -- cannot fail
     ol_rest <- many $
       try (do v' <- many t_ws >> char ';' >> many t_ws >> t_verb
               ol' <- many1 t_ws >> t_objectList
               return (v', ol'))
     return (mapM (\(res, objs) -> res >>= \r -> objs >>= \os -> return (r,os)) (((v1, ol1):ol_rest))) <?> "predicateObjectList"

t_comment = 
  try (char '#') >> many (satisfy (\c -> c /= '\x000A' && c /= '\x000D')) >> return '#' -- FIXME

t_subject :: GenParser Char ParseState (IO Resource)
t_subject = 
  (t_resource <?> "resource") <|> 
  (t_blank >>= return . (\b -> b >>= return . conv) <?> "blank")
  where 
    conv :: Blank -> Resource
    conv (B_Collection _)  = error "Blank Collection not permitted as Subject"
    conv (B_POList i pol)  = R_Blank (B_POList i pol)
    conv blank             = R_Blank blank

t_predicate :: GenParser Char ParseState (IO Resource)
t_predicate = t_resource <?> "resource"

t_object :: GenParser Char ParseState (IO Object)
t_object = (((try t_resource) <?> "resource") >>= return . liftM O_Resource) <|>
           (((try t_blank) <?> "blank") >>= \r ->
               do (b,i) <- getState; 
                  setState (b,i+1); 
                  return (r >>= return . O_Blank))
           <|> (((try t_literal) <?> "literal") >>= \l -> return (l >>= return . O_Literal))


t_literal :: GenParser Char ParseState (IO Node)
t_literal = 
  try str_literal <|>
  (do i <- try t_integer; return (lnode $ TypedL (s2b i) (makeUri xsd $ s2b "integer"))) <|>
  (do d <- try t_double; return (lnode $ TypedL (s2b d) (makeUri xsd $ s2b "double")))   <|>
  (do d <- try t_decimal; return (lnode $ TypedL (s2b d) (makeUri xsd $ s2b "decimal"))) <|>
  (do b <- try t_boolean; return (lnode $ TypedL (s2b b) (makeUri xsd $ s2b "boolean")))

str_literal :: GenParser Char ParseState (IO Node)
str_literal =
  do 
    str <- try (t_quotedString <?> "quotedString")
    rt <-  rest
    case rt of
      Nothing                        -> return (lnode $ PlainL str Nothing)
      (Just (Left lng))              -> return (lnode $ PlainL str (Just $ s2b lng))
      (Just (Right urires))          -> return (urires >>= \(R_URIRef uri) -> lnode $ TypedL str uri)
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

t_blank :: GenParser Char ParseState (IO Blank)
t_blank = 
  (try t_nodeID >>= return . return . B_BlankId) <|>
  try (do string "[]"; (b,n) <- getState; setState (b,n+1); return $ return (B_BlankGen n)) <|>
  try (do char '['; poList <- predObjList; (b,n) <- getState; setState (b,n+1); char ']'; return (poList >>= return . B_POList n )) <|>
  do objs <- t_collection
     let len = length objs
     ids <- getIds len
     return $ z objs ids >>= return . B_Collection
  where
    z :: [IO Object] -> [Int] -> IO [(Int, Object)]
    z cs ids = mapM id (zipWith f cs ids)
    f obj id' = obj >>= \o -> return (id', o)
    predObjList = do { many t_ws; l <- t_predicateObjectList; many t_ws; return l}
    getIds :: Int -> GenParser Char ParseState [Int]
    getIds n = mapM (\_ -> do (b,i) <- getState; setState (b,i+1); return i) (replicate n False)

t_itemList = 
  do obj1 <- t_object <?> "object"
     objs <- many (try $ many1 t_ws >> try t_object) <?> "object"
     return (obj1:objs)

t_collection :: GenParser Char ParseState ([IO Object])
t_collection = try $ between (char '(') (char ')') g
  where
    g = do { many t_ws; l <- option [] t_itemList; many t_ws; return l }

t_ws = char '\x000A' <|> char '\x000D' <|> char '\x0020' <|> t_comment <?> "whitespace"

t_resource :: GenParser Char ParseState (IO Resource)
t_resource = t_uriref <|> t_qname

t_nodeID = do { string "_:"; cs <- t_name; return $ s2b "_:" `B.append` cs }

t_qname :: GenParser Char ParseState (IO Resource)
t_qname = 
  do pre <- option (s2b "") t_prefixName
     char ':'
     name <- option (s2b "") t_name
     return $ return (R_QName pre name)

t_uriref :: GenParser Char ParseState (IO Resource)
t_uriref = between (char '<') (char '>') t_relativeURI >>= return . return . R_URIRef

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

t_name = do { c <- t_nameStartChar; cs <- many t_nameChar; return $ s2b (c:cs) }

t_prefixName = do { c <- t_nameStartCharMinusUnderscore; cs <- many t_nameChar; return $ s2b (c:cs) }

t_relativeURI = many t_ucharacter >>= return . s2b . concat

t_quotedString = try t_longString <|> try t_string

t_string = 
  do char '"'
     schars <- many t_scharacter
     char '"'
     return (s2b $ concat schars)

t_longString = 
  do tripleQuote
     strs <- manyTill longString_char (try tripleQuote)
     -- the manyTill already read the closing quotes, so don't read again
     return (s2b $ concat strs)
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
  do try unicode_escape <|> try (string "\\>") <|> try (non_ctrl_char_except ['>'])

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

post_process :: Maybe BaseUrl -> ByteString -> [Maybe Statement]
                -> IO (Triples, Maybe BaseUrl, PrefixMappings)
post_process _baseUrl _docUrl sts = 
  (post_process' _baseUrl _docUrl $ map fromJust $ filter isJust sts) >>= return

post_process' :: Maybe BaseUrl -> ByteString -> Statements -> IO (Triples, Maybe BaseUrl, PrefixMappings)
post_process' baseUrl docUrl stmts = post_process'' ([], baseUrl, docUrl, Map.empty) stmts >>= return . f
  where f (ts, _, _, pms) = (ts, baseUrl, pms)


post_process'' :: (Triples, Maybe BaseUrl, ByteString, PrefixMappings) -> Statements ->
                  IO (Triples, Maybe BaseUrl, ByteString, PrefixMappings)
post_process'' tup [] = return tup
post_process'' (ts, currBaseUrl, docUrl, pms) (stmt:stmts) =
  case stmt of
    S_Directive sdir -> 
      case sdir of
        D_PrefixId (pre,url) -> post_process'' (ts, currBaseUrl, docUrl, Map.insert pre (resolveUrl currBaseUrl url) pms) stmts
        D_BaseUrl url        -> post_process'' (ts, Just (newBaseUrl currBaseUrl url), docUrl, pms) stmts
    S_Triples strp    ->
      do (new_ts, new_baseUrl, _, newPms) <- process_ts (ts, currBaseUrl, docUrl, pms) strp
         post_process'' (new_ts ++ ts, new_baseUrl, docUrl, newPms) stmts
  where
    resolveUrl Nothing               url = url
    resolveUrl (Just (BaseUrl bUrl)) url = if isAbsoluteUri url then url else (if url == hash then docUrl `B.append` hash else bUrl `B.append` url)
    newBaseUrl Nothing               url = BaseUrl url
    newBaseUrl (Just (BaseUrl bUrl)) url = BaseUrl $ if isAbsoluteUri url then url else bUrl `B.append` url
    hash = s2b "#"

process_ts :: (Triples, Maybe BaseUrl, ByteString, PrefixMappings) -> (Resource, [(Resource, [Object])]) ->
              IO (Triples, Maybe BaseUrl, ByteString, PrefixMappings)
process_ts (ts, bUrl, docUrl, pms) (subj, poLists) = new_ts >>= \ts' -> return (ts' ++ ts, bUrl, docUrl, pms)
  where
    new_ts = convertPOLists bUrl docUrl pms (subj, poLists)

-- When first encountering a POList, we convert each of the lists within the list and concatenate the result.
convertPOLists :: Maybe BaseUrl -> ByteString -> PrefixMappings -> (Resource, [(Resource, [Object])]) -> IO Triples
convertPOLists bUrl docUrl pms (subj, poLists) = mapM (convertPOListItem bUrl docUrl pms subj) poLists >>= return . concat

-- For a POList item within a POList, we convert the individual item to a list of triples.
convertPOListItem :: Maybe BaseUrl -> ByteString -> PrefixMappings -> Resource -> (Resource, [Object]) -> IO Triples
convertPOListItem bUrl docUrl pms subj (pred, objs) = mapM (convertObjectListItem bUrl docUrl pms subj pred) objs >>= return . concat

-- For a given object list item that is the object of supplied subject and predicate, convert it into a list
-- of triples.
convertObjectListItem :: Maybe BaseUrl -> ByteString -> PrefixMappings -> Resource -> Resource -> Object -> IO Triples
convertObjectListItem bUrl docUrl pms subj pred obj
  -- if we have a bnode po list as the subject, we create a set of triples with a bnode as subject
  -- and all the pred-obj pairs as pred and obj; and additionally create a set of triples representing
  -- the bnode as subj, the curr pred as pred, and each object (in possibly a collection) as object.
  | isBNodeListSubject subj =
        (\(R_Blank (B_POList i' polist')) ->  
           convertObjectListItem bUrl docUrl pms (R_Blank $ B_BlankGen i') pred obj >>= \i1s ->
             mapM (convertPOListItem bUrl docUrl pms (R_Blank $ B_BlankGen i')) polist' >>= return . concat >>= \i2s -> return (i1s ++ i2s))
          subj
  | isOLiteral        obj   = makeTriple' lnode
  | isOResource       obj   = objNode >>= makeTriple'
  | isOBlankBNode     obj   = bIdNode >>= makeTriple'
  | isOBlankBNodeGen  obj   = bGenIdNode >>= makeTriple'
  | isOBlankBNodeColl obj   = convertColl bUrl pms subj pred ocoll
  -- if object is a POList, then we create a bnode to use as the object in a triple and cons
  -- that triple onto the triples that represent the POList, each of which has the same bnode
  -- as the subject.
  | isOBlankPOList    obj   = convertPOLists bUrl docUrl pms (R_Blank opol, pol) >>= \ts ->  (bObjPolGenNode >>= makeTriple' >>= \ts' -> return (ts' ++ ts))
  | otherwise               = error $ "Unexpected case: " ++ show obj
  where 
    makeTriple :: Node -> IO Triple
    makeTriple                            = \o -> subjNode >>= \s -> predNode >>= \p -> return (triple s p o)
    makeTriple'                           = \t -> makeTriple t >>= return . (:[])
    subjNode                              = uriRefQNameOrBlank bUrl pms subj
    predNode                              = uriRefQNameOrBlank bUrl pms pred
    objNode                               = uriRefQNameOrBlank bUrl pms res
    bIdNode                               = mkFastString bId >>= return . BNode
    bGenIdNode                            = return (BNodeGen bGenId)
    bObjPolGenNode                        = return (BNodeGen poObjId)
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
convertColl :: Maybe BaseUrl -> PrefixMappings -> Resource -> Resource -> Blank -> IO Triples
convertColl bUrl pms subj pred (B_Collection objs) =  
  do ts <- convertColl' objs
     if null objs
        then return ts
        else initialTriple (head objs) >>= return . (:ts)
  where
    initialTriple :: (Int, Object) -> IO Triple
    initialTriple (initId, _) = subjNode >>= \s -> predNode >>= \p -> return (triple s p (BNodeGen initId))
    subjNode = uriRefQNameOrBlank  bUrl pms subj
    predNode = uriRefOrQName bUrl pms pred
    -- only called with empty list if collection was empty
    convertColl' :: [(Int, Object)] -> IO Triples
    convertColl' []                = do {s <- subjNode; p <- predNode; o <- rdfNil; return [triple s p o]}
    -- if next elem is nil, then we're finished, and make rdf:rest nil
    convertColl' ((i, obj):[])     = do { f <- rdfFirst;r <- rdfRest;n <- rdfNil;o <- bObjToNode obj;return $ triple (BNodeGen i) f o : triple (BNodeGen i) r n : [] }
    convertColl' ((i1,obj1):o2@(i2,_):os) = do {f <- rdfFirst;r <- rdfRest;o <- bObjToNode obj1; convertColl' (o2:os) >>= \ts -> return $ triple (BNodeGen i1) f o : triple (BNodeGen i1) r (BNodeGen i2) : ts}
    bObjToNode :: Object -> IO Node
    bObjToNode (O_Resource res) = uriRefOrQName bUrl pms res
    bObjToNode (O_Literal lit)  = return lit
    bObjToNode errObj           = error $ "TurtleParser.convertColl: not allowed in collection: " ++ show errObj
    rdfNil   = rdfNode "nil"
    rdfFirst = rdfNode "first"
    rdfRest  = rdfNode "rest"
    rdfNode localName = mkFastString (makeUri rdf (s2b localName)) >>= return . UNode
convertColl _ _ _ _ coll = error $ "TurtleParser.convertColl. Unexpected blank: " ++ show coll
{-
resourceToNode :: Maybe BaseUrl -> PrefixMappings -> Resource -> Node
resourceToNode baseUrl pms res =
  case (baseUrl, res) of
    (bUrl,    (R_URIRef uri))  -> UNode $ resolveUri bUrl    uri
    (_,       (R_QName p u))   -> UNode $ resolveQName p pms ++ u
    (_,       (R_Blank b))     -> blankToNode b
  where 
    blankToNode (B_BlankId bId) = BNode bId
    blankToNode (B_BlankGen i)  = BNodeGen i
    blankToNode b               = error $ "Cannot convert blank '" ++ show b ++ "' to a single node."
-}

{-
resolveUri :: Maybe BaseUrl -> String -> String
resolveUri Nothing uri | isAbsoluteUri uri = uri
                       | otherwise         = error "No Base URL against which to resolve relative URI"
resolveUri (Just (BaseUrl bUrl)) uri | isAbsoluteUri uri = uri
                                     | otherwise         = bUrl ++ uri
-}

{-
Need to determine if the uriref is really an absolute uri by checking to
see if it begins with a scheme, which would satisfy the regex:
scheme        = alpha *( alpha | digit | "+" | "-" | "." )

alpha is lowercase letters, but rfc says best to normalize uppercase to lower.
-}
isAbsoluteUri = B.elem ':'

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

uriRefQNameOrBlank :: Maybe BaseUrl -> PrefixMappings -> Resource -> IO Node
uriRefQNameOrBlank  Nothing  _     (R_URIRef url)                = mkFastString url >>= return . UNode
uriRefQNameOrBlank (Just (BaseUrl u)) _ (R_URIRef url)           = 
  if isAbsoluteUri url 
     then mkFastString url >>= return . UNode
     else mkFastString (u `B.append` url) >>= return . UNode
uriRefQNameOrBlank  bUrl     pms   (R_QName pre local)           = return ((resolveQName bUrl pre pms) `B.append` local) >>= mkFastString >>= return . UNode
uriRefQNameOrBlank  _        _     (R_Blank (B_BlankId bId))     = mkFastString bId >>= return . BNode
uriRefQNameOrBlank  _        _     (R_Blank (B_BlankGen genId))  = return $ BNodeGen genId
uriRefQNameOrBlank  _        _      res                          = error $ show ("TurtleParser.uriRefQNameOrBlank: " ++ show res)

uriRefOrQName :: Maybe BaseUrl -> PrefixMappings -> Resource -> IO Node
uriRefOrQName Nothing _     (R_URIRef url)            = mkFastString url >>= return . UNode
uriRefOrQName (Just (BaseUrl u)) _ (R_URIRef url)     = let url' = if isAbsoluteUri url then url else u `B.append` url
                                                        in mkFastString url' >>= return . UNode
uriRefOrQName bUrl    pms   (R_QName pre local)       = let u = (resolveQName bUrl pre pms) `B.append` local
                                                        in mkFastString u >>= return . UNode
uriRefOrQName _       _     _                         = error "TurtleParser.predicate"

resolveQName :: Maybe BaseUrl -> ByteString -> PrefixMappings -> ByteString
resolveQName Nothing            pre   _ 
  | pre == B.empty =  error "Cannot resolve empty QName prefix to a base URL."
resolveQName (Just (BaseUrl u)) pre   pms 
  | pre == B.empty =  Map.findWithDefault u B.empty pms
resolveQName _                  pre   pms = 
  Map.findWithDefault (error $ "Cannot resolve QName Prefix: " ++ B.unpack pre) pre pms


parseString :: Graph gr => Maybe BaseUrl -> String -> String -> IO (Either ParseFailure gr)
parseString bUrl docUrl ttlStr = handleResult bUrl docUrl (runParser t_turtleDoc (bUrl, 0) "" ttlStr)

handleResult :: Graph gr => Maybe BaseUrl -> String -> Either ParseError (IO [Maybe Statement]) -> IO (Either ParseFailure gr)
handleResult bUrl docUrl result =
  case result of
     l@(Left err) -> return $ Left (ParseFailure $ show err)
     (Right res)  -> do stmts <- res
                        (ts, mbUrl, pms) <- post_process bUrl (s2b docUrl) stmts
                        g <- mkGraph ts bUrl pms
                        return (Right g)

parseFile :: Graph gr => Maybe BaseUrl -> String -> String -> IO (Either ParseFailure gr)
parseFile bUrl docUrl fpath = 
  readFile fpath >>= \str -> handleResult bUrl docUrl (runParser t_turtleDoc (bUrl, 0) docUrl str)

-- Parse the N-Triples document at the given URL, 
-- generating a graph containing the parsed triples.
--parseURL :: Graph gr => String -> IO (Either ParseFailure gr)
--parseURL url = _parseURL parseString url
parseURL :: Graph gr => Maybe BaseUrl -> String -> String -> IO (Either ParseFailure gr)
parseURL bUrl docUrl locUrl = _parseURL (parseString bUrl docUrl) locUrl
