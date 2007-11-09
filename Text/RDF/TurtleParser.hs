{-

This module defines a parser for RDF in Turtle format.

The version of the Turtle specification to which this parser conforms (with 
the exception noted below) is that of September 11, 2004, retrieved from
<http://www.dajobe.org/2004/01/turtle/>.

-}

module Text.RDF.TurtleParser where

import Text.RDF.Core hiding (Object)
import Text.RDF.Namespace
import Text.RDF.ParserUtils()

import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
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

-- Declarations of datatypes that are used only within this module for parsing.

type Statements = [Statement]

data Statement = 
    S_Directive  {-# UNPACK #-} !Directive
  | S_Triples    {-# UNPACK #-} !T_Triples
  deriving Show

data Directive = 
    D_BaseUrl    {-# UNPACK #-} !ByteString
  | D_PrefixId   {-# UNPACK #-} !(ByteString, ByteString)
  deriving Show

data Blank = 
    B_BlankId    {-# UNPACK #-} !ByteString
  | B_BlankGen   {-# UNPACK #-} !Int            -- int is genid used for []
  | B_POList Int {-# UNPACK #-} !POList         -- int is genid used for [ :p1 :o1; :p2 :o2 ]
  | B_Collection {-# UNPACK #-} ![(Int,Object)] -- int is genid used for ( :obj1 :obj2 )
  deriving Show

type POList = [(Resource, [Object])]

type Collection = [Object]

data Object = 
    O_Resource {-# UNPACK #-} !Resource
  | O_Blank    {-# UNPACK #-} !Blank
  | O_Literal  {-# UNPACK #-} !Node
  deriving Show

data Resource = 
    R_URIRef {-# UNPACK #-} !ByteString
  | R_QName  {-# UNPACK #-} !ByteString  {-# UNPACK #-}  !ByteString
  | R_Blank  {-# UNPACK #-} !Blank
  deriving Show

type T_Triples  = (Resource, [(Resource, [Object])])

-- The state that we maintain while parsing consists of the following:
--
-- Maybe BaseUrl:    the base URL of the graph, which may or may not be
--                   set initially, and may or may not be updated in the 
--                   course of parsing the document.
--
-- Maybe ByteString: an optional document URL that is used for resolving
--                   references to 'this document'. Not updated while parsing.
-- 
-- Int:              the value of the next identifier to be used for
--                   creating generated bnode identifiers. Starts at 1,
--                   and is incremented by 1 after it is assigned.
--
-- PrefixMappings:   an initially empty map of prefix-URI associations
--                   that is updated as no prefixes are encountered and
--                   used for resolving qnames while parsing.
--
-- Maybe Node:       the most recently encountered subject node.
--
-- Maybe Node:       the most recently encountered predicate node.
--
-- [Triple]:         triples to be integrated into the triple-stream when 
--                   possible; this is for cases like the object position of
--                   a triple where a single node may be returned or else
--                   a node plus a bunch of triples (e.g., when [] contains 
--                   triples). Instead of complicating types by having such
--                   combinators use more complex ADTs, like Either, we just
--                   put them into the parse state and handle them whenever
--                   it is convenient to do so.
type ParseState = (Maybe BaseUrl, Maybe ByteString, Int, PrefixMappings, Maybe Node, Maybe Node, Triples)

----------------------------------------------
-- Parsing functions and helpers begin here --
----------------------------------------------


-- The combinators generally appear in the order that the corresponding EBNF rules are defined
-- in the spec, and apart from a couple of deviations for performance reasons, a combinator
-- generally implements one rule.

t_turtleDoc :: GenParser Char ParseState (Triples, PrefixMappings)
t_turtleDoc =  
  do ioStmts <- many t_statement >>= \ts -> eof >> return ts :: GenParser Char ParseState ([Maybe Statement])
     (_bUrl,_dUrl,_i,_pms,_s,_p,_ts) <- getState
     let (ts,_,pms) = convertStatements _bUrl _dUrl ioStmts 
     return (ts, pms)
     
t_statement :: GenParser Char ParseState (Maybe Statement)
t_statement = (d >>= return .  Just) <|> 
              (t >>= return .  Just) <|> 
              (many1 t_ws >> (return $! Nothing))
  where 
    d = do { dir <- t_directive; many t_ws >> (char '.' <?> "end-of-directive") >> many t_ws >> return dir}
    t = do { trp <- t_triples; many t_ws >> (char '.' <?> "end-of-triple") >> many t_ws >> return trp}

-- Converts a list of 'Maybe Statement' values into equivalent triples, along with the prefix
-- mappings encountered while converting and the last given BaseUrl, or the original or Nothing
-- if none were present in the parsed document.
convertStatements :: Maybe BaseUrl -> Maybe ByteString -> [Maybe Statement] -> (Triples, Maybe BaseUrl, PrefixMappings)
convertStatements !baseUrl !docUrl  =  f ([], baseUrl, PrefixMappings Map.empty)
  where 
    f :: (Triples, Maybe BaseUrl, PrefixMappings) -> [Maybe Statement] ->  (Triples, Maybe BaseUrl, PrefixMappings)
    f  tup                   []                  = tup
    f  tup                   (Nothing:stmts)     = f tup stmts
    f (ts, currBaseUrl, pms@(PrefixMappings pms')) ((Just stmt):stmts) =
      case stmt of
        (S_Directive (D_PrefixId (pre, frag))) -> f (ts, currBaseUrl, PrefixMappings (Map.insert pre (absolutizeUrl currBaseUrl docUrl frag) pms')) stmts
        (S_Directive (D_BaseUrl url))          -> f (ts, Just (newBaseUrl currBaseUrl url),  pms) stmts
        (S_Triples strips)                     -> do let (new_ts, new_baseUrl, newPms) = process_ts (currBaseUrl, pms) strips
                                                     f (new_ts ++ ts, new_baseUrl, newPms) stmts
    process_ts :: (Maybe BaseUrl, PrefixMappings) -> (Resource, [(Resource, [Object])])  ->  (Triples, Maybe BaseUrl, PrefixMappings)
    process_ts (bUrl, pms) (subj, poLists) = (ts, bUrl, pms)
      where ts = convertPOLists bUrl docUrl pms (subj, poLists)

-- Resolve a URL fragment found on the right side of a prefix mapping by converting it to an absolute URL if possible.
absolutizeUrl :: Maybe BaseUrl -> Maybe ByteString -> ByteString -> ByteString
absolutizeUrl mbUrl mdUrl urlFrag =
  case isAbsoluteUri urlFrag of
    True   ->  urlFrag
    False  ->  
      case (mbUrl, mdUrl) of
        (Nothing,             Nothing    )  -> urlFrag
        (Just (BaseUrl bUrl), Nothing    )  -> bUrl `B.append` urlFrag
        (Nothing,             (Just dUrl))  -> if fragIsHash then dUrl `B.append` urlFrag else urlFrag
        (Just (BaseUrl bUrl), (Just dUrl))  -> if fragIsHash then dUrl `B.append` urlFrag else bUrl `B.append` urlFrag
  where
    fragIsHash     = B.length urlFrag == 1 && B.head urlFrag == '#'

t_directive :: GenParser Char ParseState (Statement)
t_directive = (try t_prefixID <|> try t_base <?> "directive") >>= return . S_Directive

t_prefixID :: GenParser Char ParseState (Directive)
t_prefixID =
  do string "@prefix"
     many1 t_ws <?> "whitespace"
     pn <- option (s2b "") t_prefixName
     char ':'
     many1 t_ws
     (R_URIRef uri)  <- t_uriref
     return $ D_PrefixId (pn, uri)

t_base :: GenParser Char ParseState (Directive)
t_base =
  do string "@base"
     many1 t_ws        <?> "whitespace"
     (R_URIRef uri) <- t_uriref
     return $ (D_BaseUrl uri)

t_triples :: GenParser Char ParseState (Statement)
t_triples = 
  do subj <- t_subject <?> "subject"
     many1 t_ws        <?> "whitespace"
     poList <- t_predicateObjectList <?> "predicateObjectList"
     return $! S_Triples (subj, poList)

t_objectList :: GenParser Char ParseState ([Object])
t_objectList =
  do obj1 <- t_object <?> "object"
     obj_rest <- many (try obj <?> "object")
     return $! (obj1:obj_rest)
  where 
    obj = many t_ws >> char ',' >> many t_ws >> t_object

t_verb :: GenParser Char ParseState (Resource)
t_verb = 
  try t_predicate <|>
  (char 'a' >> return (R_URIRef $! rdfTypeBs))
  <?> "verb"

-- When first encountering a POList, we convert each of the lists within the list and concatenate the result.
convertPOLists :: Maybe BaseUrl -> Maybe ByteString -> PrefixMappings -> (Resource, [(Resource, [Object])]) -> Triples
convertPOLists !bUrl !docUrl !pms !(!subj, !poLists) = concatMap (convertPOListItem bUrl docUrl pms subj) poLists

-- For a POList item within a POList, we convert the individual item to a list of triples.

convertPOListItem :: Maybe BaseUrl -> Maybe ByteString -> PrefixMappings -> Resource -> (Resource, [Object]) -> Triples
convertPOListItem !bUrl !docUrl !pms !subj !(!pred, !objs) = concatMap (convertObjectListItem bUrl docUrl pms subj pred) objs

-- For a given object list item that is the object of supplied subject and predicate, convert it into a list
-- of triples.
convertObjectListItem :: Maybe BaseUrl -> Maybe ByteString -> PrefixMappings -> Resource -> Resource -> Object -> Triples
convertObjectListItem !bUrl !docUrl !pms !subj !pred !obj
  -- if we have a bnode po list as the subject, we create a set of triples with a bnode as subject
  -- and all the pred-obj pairs as pred and obj; and additionally create a set of triples representing
  -- the bnode as subj, the curr pred as pred, and each object (in possibly a collection) as object.
  | isBNodeListSubject subj = let (R_Blank (B_POList i' polist')) = subj
                                  i1s = convertObjectListItem bUrl docUrl pms (R_Blank (B_BlankGen i')) pred obj
                                  i2s = map (convertPOListItem bUrl docUrl pms (R_Blank (B_BlankGen i'))) polist'
                              in (i1s ++ concat i2s)
  | isOLiteral        obj   = [makeTriple lnode]
  | isOResource       obj   = [makeTriple objNode]
  | isOBlankBNode     obj   = [makeTriple bIdNode]
  | isOBlankBNodeGen  obj   = [makeTriple bGenIdNode]
  | isOBlankBNodeColl obj   = convertColl bUrl pms subj pred ocoll
  -- if object is a POList, then we create a bnode to use as the object in a triple and cons
  -- that triple onto the triples that represent the POList, each of which has the same bnode
  -- as the subject.
  | isOBlankPOList    obj   = let ts = convertPOLists bUrl docUrl pms (R_Blank opol, pol)
                                  ts' = makeTriple bObjPolGenNode
                              in (ts' : ts)
  | otherwise                = error $ "Unexpected case: " ++ show obj
  where 
    makeTriple :: Node -> Triple
    makeTriple   o                        = triple subjNode predNode o
    subjNode                              = uriRefQNameOrBlank bUrl pms subj
    predNode                              = uriRefOrQName bUrl pms pred
    objNode                               = uriRefQNameOrBlank bUrl pms res
    bIdNode                               = BNode (mkFastString bId)
    bGenIdNode                            = BNodeGen bGenId
    bObjPolGenNode                        = BNodeGen poObjId
    (O_Literal lnode)                     = obj
    (O_Resource res)                      = obj
    (O_Blank (B_BlankId bId))             = obj
    (O_Blank (B_BlankGen bGenId))         = obj
    (O_Blank ocoll@(B_Collection _))      = obj
    (O_Blank opol@(B_POList poObjId pol)) = obj


t_predicateObjectList :: GenParser Char ParseState ([(Resource, [Object])])
t_predicateObjectList = 
  do v1 <- t_verb <?> "verb"
     many1 t_ws   <?> "whitespace"
     ol1 <- t_objectList <?> "objectList" -- cannot fail
     ol_rest <- many $
       try (do v' <- many t_ws >> char ';' >> many t_ws >> t_verb
               ol' <- many1 t_ws >> t_objectList
               return (v', ol'))
     return ((v1,ol1):ol_rest) <?> "predicateObjectList"

t_comment :: GenParser Char ParseState Char
t_comment = 
  try (char '#') >> many (satisfy (\c -> c /= '\x000A' && c /= '\x000D')) >> return '#' -- FIXME

t_subject :: GenParser Char ParseState (Resource)
t_subject = 
  (t_resource <?> "resource") <|> 
  (t_blank >>=  return . conv) <?> "blank"
  where 
    conv :: Blank -> Resource
    conv (B_Collection _)  = error "Blank Collection not permitted as Subject"
    conv (B_POList i pol)  = (R_Blank $! (B_POList i pol))
    conv blank             = (R_Blank $! blank)

t_predicate :: GenParser Char ParseState (Resource)
t_predicate = t_resource <?> "resource"

t_object :: GenParser Char ParseState (Object)
t_object = (((try t_resource) <?> "resource") >>=  return . O_Resource) <|>
           (((try t_blank) <?> "blank") >>= \r ->
               do (bUrl,dUrl,i,pms,_s,_p,_ts) <- getState; 
                  setState (bUrl,dUrl,i+1,pms,_s,_p,_ts); 
                  return (O_Blank r))
           <|> ((try t_literal >>= return . O_Literal) <?> "literal")

t_literal :: GenParser Char ParseState (Node)
t_literal = 
  try str_literal <|>
  (do i <- try t_integer; return $! (mkLNode i xsdIntUri))     <|>
  (do d <- try t_double;  return $! (mkLNode d xsdDoubleUri))  <|>
  (do d <- try t_decimal; return $! (mkLNode d xsdDecimalUri)) <|>
  (do b <- try t_boolean; return $! (mkLNode b xsdBooleanUri))
  where
    mkLNode :: ByteString -> FastString -> Node
    mkLNode bs fs =  LNode $ typedL bs fs

str_literal :: GenParser Char ParseState (Node)
str_literal =
  do 
    str <- try (t_quotedString <?> "quotedString")
    rt <-  rest
    case rt of
      Nothing                          -> return $! (lnode $! (plainL str))
      (Just (Left lng))                -> return $! (lnode $! (plainLL str lng))
      (Just (Right (R_URIRef urires))) -> return $! LNode (typedL str (mkFastString urires))
      (Just (Right _))                 -> error $! "TurtleParser.str_literal"
  where
    rest = (try (string "^^") >> t_uriref >>= return . Just . Right) <|>
           (try (char '@') >> t_language >>= return . Just . Left) <|>
           return Nothing

t_integer :: GenParser Char ParseState ByteString
t_integer = 
  do sign <- sign_parser <?> "+-"
     ds <- many1 digit   <?> "digit"
     notFollowedBy (char '.')
     -- integer must be in canonical format, with no leading plus sign or leading zero
     return $! (s2b sign `B.append` s2b ds)

t_double :: GenParser Char ParseState ByteString
t_double =
  do sign <- sign_parser <?> "+-"
     rest <- try (do { ds <- many1 digit <?> "digit";  char '.'; ds' <- many digit <?> "digit"; e <- t_exponent <?> "exponent"; return (s2b ds `B.snoc` '.' `B.append` s2b ds' `B.append` e) }) <|>
             try (do { char '.'; ds <- many1 digit <?> "digit"; e <- t_exponent <?> "exponent"; return ('.' `B.cons` s2b ds `B.append` e) }) <|>
             try (do { ds <- many1 digit <?> "digit"; e <- t_exponent <?> "exponent"; return (s2b ds `B.append` e) })
     return $! s2b sign `B.append` rest

sign_parser :: GenParser Char ParseState String
sign_parser = option "" (oneOf "-+" >>= (\c -> return $! (c:[]))) 

digits1_parser :: GenParser Char ParseState String
digits1_parser = many1 digit

t_decimal :: GenParser Char ParseState ByteString
t_decimal = 
  do sign <- sign_parser
     rest <- try (do ds <- many digit <?> "digit"; char '.'; ds' <- option "" (many digit); return (ds ++ ('.':ds')))
             <|> try (do { char '.'; ds <- many1 digit <?> "digit"; return ('.':ds) })
             <|> many1 digit <?> "digit"
     return $ s2b sign `B.append` s2b rest
     
t_exponent :: GenParser Char ParseState ByteString
t_exponent = do e <- oneOf "eE"
                s <- option "" (oneOf "-+" >>= \c -> return (c:[]))
                ds <- many1 digit; 
                return $! (e `B.cons` (s2b s `B.append` s2b ds))

t_boolean :: GenParser Char ParseState ByteString
t_boolean = try ((string "true" >> return trueBs) <|> (string "false" >> return falseBs))

t_blank :: GenParser Char ParseState (Blank)
t_blank = 
  (try t_nodeID >>= return . B_BlankId)  <|>
  try (do string "[]"; (bUrl,dUrl,n,pms,_s,_p,_ts) <- getState; setState (bUrl,dUrl,n+1,pms,_s,_p,_ts); return (B_BlankGen n)) <|>
  try (do char '['; poList <- predObjList; (bUrl,dUrl,n,pms,_s,_p,_ts) <- getState; setState (bUrl,dUrl,n+1,pms,_s,_p,_ts); char ']'; return (B_POList n poList)) <|>
  do objs <- t_collection
     let len = length objs
     ids <- getIds len
     return $ B_Collection (z objs ids)
  where
    z :: [Object] -> [Int] -> [(Int, Object)]
    z cs ids  = zipWith f cs ids
    f obj id' = (id', obj)
    predObjList = do { many t_ws; l <- t_predicateObjectList; many t_ws; return l}
    getIds :: Int -> GenParser Char ParseState [Int]
    getIds !n = mapM (\_ -> do (bUrl,dUrl,i,pms,_s,_p,_ts) <- getState; setState (bUrl,dUrl,i+1,pms,_s,_p,_ts); return i) (replicate n False)

t_itemList :: GenParser Char ParseState [Object]
t_itemList = 
  do obj1 <- t_object <?> "object"
     objs <- many (try $ many1 t_ws >> try t_object) <?> "object"
     return $! (obj1:objs)

{-
Collections are interpreted as follows:

( object1 object2 ) is short for:

[ rdf:first object1; rdf:rest [ rdf:first object2; rdf:rest rdf:nil ] ]

Which expands to the following triples:
:genid0 rdf:first object1 .
:genid0 rdf:rest :genid1 .
:genid1 rdf:first object2 .
:genid1 rdf:rest rdf:nil .
-- plus the initial:
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
convertColl !bUrl !pms !subj !pred (B_Collection !objs) =  
  let ts = convertColl' objs
  in  case null objs of
         True   ->  ts
         False  ->  initialTriple (head objs) : ts
  where
    initialTriple :: (Int, Object) -> Triple
    initialTriple (initId, _) = triple subjNode predNode (BNodeGen initId)
    subjNode = uriRefQNameOrBlank  bUrl pms subj
    predNode = uriRefOrQName bUrl pms pred
    -- only called with empty list if collection was empty
    convertColl' :: [(Int, Object)] -> Triples
    convertColl' []                = [triple subjNode predNode rdfNilNode]
    -- if next elem is nil, then we're finished, and make rdf:rest nil
    convertColl' ((i, obj):[])     = triple (BNodeGen i) rdfFirstNode (bObjToNode obj) : triple (BNodeGen i) rdfRestNode rdfNilNode : []
    convertColl' ((i1,obj1):o2@(i2,_):os) = triple (BNodeGen i1) rdfFirstNode (bObjToNode obj1) : triple (BNodeGen i1) rdfRestNode (BNodeGen i2) : convertColl' (o2:os) -- do {f <- rdfFirstNode;r <- rdfRestNode;o <- bObjToNode obj1; convertColl' (o2:os) >>= \ts -> return $! triple (BNodeGen i1) f o : triple (BNodeGen i1) r (BNodeGen i2) : ts}
    bObjToNode :: Object -> Node
    bObjToNode (O_Resource res) = uriRefOrQName bUrl pms res
    bObjToNode (O_Literal lit)  = lit
    bObjToNode errObj            = error $ "TurtleParser.convertColl: not allowed in collection: " ++ show errObj
convertColl _ _ _ _ coll = error $ "TurtleParser.convertColl. Unexpected blank: " ++ show coll

t_collection :: GenParser Char ParseState ([Object])
t_collection = try $! between (char '(') (char ')') g
  where
    g =  many t_ws >> option [] t_itemList >>= \l -> many t_ws >> return l

t_ws  :: GenParser Char ParseState Char
t_ws = char '\x000A' <|> char '\x000D' <|> char '\x0020' <|> t_comment <?> "whitespace"

t_resource :: GenParser Char ParseState (Resource)
t_resource = t_uriref <|> t_qname

t_nodeID  :: GenParser Char ParseState ByteString
t_nodeID = do { string "_:"; cs <- t_name; return $! s2b "_:" `B.append` cs }


uriRefQNameOrBlank :: Maybe BaseUrl -> PrefixMappings -> Resource -> Node
uriRefQNameOrBlank (Just (BaseUrl u)) _ (R_URIRef url)           = UNode $! mkFastString (mkAbsoluteUrl u url)
uriRefQNameOrBlank  Nothing           _ (R_URIRef url)           = UNode $! mkFastString url
uriRefQNameOrBlank  bUrl     pms   (R_QName pre local)           = UNode $! mkFastString ((resolveQName bUrl pre pms) `B.append` local)
uriRefQNameOrBlank  _        _     (R_Blank (B_BlankId bId))     = BNode $! mkFastString bId
uriRefQNameOrBlank  _        _     (R_Blank (B_BlankGen genId))  = BNodeGen genId
uriRefQNameOrBlank  _        _     !res                          = error  $  show ("TurtleParser.uriRefQNameOrBlank: " ++ show res)

uriRefOrQName :: Maybe BaseUrl -> PrefixMappings -> Resource -> Node
uriRefOrQName Nothing                _    (R_URIRef !url)     = UNode $! mkFastString url
uriRefOrQName (Just (BaseUrl base))  _    (R_URIRef url)      = UNode $! mkFastString (mkAbsoluteUrl base url)
uriRefOrQName bUrl                   pms  (R_QName pre local) = UNode $! mkFastString (resolveQName bUrl pre pms `B.append` local)
uriRefOrQName _                      _    _                   = error "TurtleParser.predicate"

t_qname :: GenParser Char ParseState (Resource)
t_qname = 
  do pre <- option (s2b "") t_prefixName
     char ':'
     name <- option (s2b "") t_name
     return (R_QName pre name)

t_uriref :: GenParser Char ParseState (Resource)
t_uriref = between (char '<') (char '>') t_relativeURI >>= \uri -> return (R_URIRef uri)

t_language  :: GenParser Char ParseState ByteString
t_language = 
  do init <- many1 lower;
     rest <- many (do {char '-'; cs <- many1 (lower <|> digit); return (s2b ('-':cs))})
     return $! (s2b init `B.append` (B.concat rest))

t_nameStartChar  :: GenParser Char ParseState Char
t_nameStartChar = try (char '_') <|> t_nameStartCharMinusUnderscore

t_nameStartCharMinusUnderscore  :: GenParser Char ParseState Char
t_nameStartCharMinusUnderscore = 
  try (satisfy (flip in_range blocks))
  where blocks = [('A', 'Z'), ('a', 'z'), ('\x00C0', '\x00D6'), 
                  ('\x00D8', '\x00F6'), ('\x00F8', '\x02FF'),
                  ('\x0370', '\x037D'), ('\x037F', '\x1FFF'),
                  ('\x200C', '\x200D'), ('\x2070', '\x218F'),
                  ('\x2C00', '\x2FEF'), ('\x3001', '\xD7FF'),
                  ('\xF900', '\xFDCF'), ('\xFDF0', '\xFFFD'),
                  ('\x10000', '\xEFFFF')]

t_nameChar :: GenParser Char ParseState Char
t_nameChar = t_nameStartChar <|> char '-' <|> char '\x00B7' <|> satisfy f
  where
    f = flip in_range [('0', '9'), ('\x0300', '\x036F'), ('\x203F', '\x2040')]
            
in_range :: Char -> [(Char, Char)] -> Bool
in_range !c = any $! (\(c1, c2) -> c >= c1 && c <= c2)

t_name :: GenParser Char ParseState ByteString
t_name = do { c <- t_nameStartChar; cs <- many t_nameChar; return $! s2b $! (c:cs) }

t_prefixName :: GenParser Char ParseState ByteString
t_prefixName = do { c <- t_nameStartCharMinusUnderscore; cs <- many t_nameChar; return $! s2b $! (c:cs) }

t_relativeURI  :: GenParser Char ParseState ByteString
t_relativeURI = many t_ucharacter >>= return .  B.concat

t_quotedString  :: GenParser Char ParseState ByteString
t_quotedString = try t_longString <|> try t_string

-- a non-long string: any number of scharacters (echaracter without ") inside doublequotes.
t_string  :: GenParser Char ParseState ByteString
t_string = between (char '"') (char '"') (many t_scharacter) >>= return . B.concat

t_longString  :: GenParser Char ParseState ByteString
t_longString = 
  do tripleQuote
     strs <- manyTill longString_char (try tripleQuote)
     -- the manyTill already read the closing quotes, so don't read again
     return $! (B.concat $! strs)
  where
    tripleQuote = count 3 (char '"')

longString_char  :: GenParser Char ParseState ByteString
longString_char  = 
  (try $! oneOf ['\x0009', '\x000A', '\x000D'] >>= \c -> return $! B.singleton c)         <|>  -- \r|\n|\t as single char
  (try $! char '\\' >> oneOf ['t', 'n', 'r', '\\'] >>= \c -> return $! s2b ('\\':c:[])) <|>
  (try $! string "\\\"" >> return (s2b "\\\"")) <|>
  (try $! non_ctrl_char_except ['\\'] >>= \s -> return $! s) <|> 
  (try unicode_escape)

t_character  :: GenParser Char ParseState ByteString
t_character =  
  try (non_ctrl_char_except ['\\'] >>= \c -> return c) <|> 
  try (string "\\" >> return (B.singleton '\\')) <|> 
  try unicode_escape

t_echaracter  :: GenParser Char ParseState ByteString
t_echaracter = try( do { (char '\\'); c <- oneOf ['t', 'n', 'r']; return $! s2b ('\\':c:[]);}) <|>
               t_character
               
t_hex  :: GenParser Char ParseState Char
t_hex = (satisfy $! (\c -> (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F'))) <?> "hexadecimal digit"

t_ucharacter  :: GenParser Char ParseState ByteString
t_ucharacter = 
  do try unicode_escape <|> 
       try (string "\\>" >> return escapedGtBs) <|> 
         try (non_ctrl_char_except ['>'] >>= \s -> return $! s)

-- characters used in (non-long) strings; any echaracters except ", or an escaped \"
-- echaracter - #x22 ) | '\"'
t_scharacter  :: GenParser Char ParseState ByteString
t_scharacter =
  do (try (string "\\\"") >> return (s2b ('\\':'"':[]))) 
     <|> try (do char '\\'; c <- oneOf ['t', 'n', 'r']; return $! s2b ('\\':c:[])) -- echaracter part 1
     <|> unicode_escape 
     <|> (non_ctrl_char_except ['\\', '"'] >>= \s -> return $! s)       -- echaracter part 2 minus "

-- characters used in long strings
t_lcharacter  :: GenParser Char ParseState ByteString
t_lcharacter = 
  t_echaracter <|> 
  (oneOf ['\x0009', '\x000A', '\x000D'] >>= \c -> return $! B.singleton c) <|>
  (try (string "\"\"") >> notFollowedBy  (char '"') >>  (return $! s2b "\"\"")) <|> -- FIXME: doesn't work with 2 embedded quotes
  (char '"' >> notFollowedBy (char '"') >> return (B.singleton '"'))

unicode_escape  :: GenParser Char ParseState ByteString
unicode_escape = 
  do {
     try (char '\\' >> return (B.singleton '\\'));
     do { char '\\'; return $! s2b "\\\\"; } <|>
     do { char 'u'; chrs <- count 4 t_hex; return $! s2b "\\u" `B.append` s2b chrs } <|>
     do { char 'U'; chrs <- count 8 t_hex; return $! s2b "\\U" `B.append` s2b chrs }
  }
       
non_ctrl_char_except  :: String -> GenParser Char ParseState ByteString
non_ctrl_char_except !cs = 
  satisfy (\c -> c <= '\x10FFFF' && (c >= '\x0020' && c `notElem` cs)) >>=  return . B.singleton

-----------------------------------------------------------
-- Some standard values that are used in various places. --
-----------------------------------------------------------

-- TODO: move some of these elsewhere if they're useful elsewhere.

trueBs, falseBs, escapedGtBs :: ByteString
trueBs      = s2b "true"
falseBs     = s2b "false"
escapedGtBs = s2b "\\>"

rdfTypeBs :: ByteString
rdfTypeBs = s2b $! "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

xsdIntUri, xsdDoubleUri, xsdDecimalUri, xsdBooleanUri :: FastString
xsdIntUri     =  mkFastString $! makeUri xsd $! s2b "integer"
xsdDoubleUri  =  mkFastString $! makeUri xsd $! s2b "double"
xsdDecimalUri =  mkFastString $! makeUri xsd $! s2b "decimal"
xsdBooleanUri =  mkFastString $! makeUri xsd $! s2b "boolean"

rdfNilNode, rdfFirstNode, rdfRestNode :: Node
rdfNilNode   = mkRdfNode $! "nil"
rdfFirstNode = mkRdfNode $! "first"
rdfRestNode  = mkRdfNode $! "rest"

mkRdfNode :: String -> Node
mkRdfNode localName = UNode $! mkFastString (makeUri rdf (s2b localName))

-- End of parsing and related functions.

---------------------------------------------------------
--  Supporting functions for conversion process below  --
---------------------------------------------------------

-- Make an absolute URL by returning as is if already an absolute URL and otherwise
-- appending the URL to the given base URL.
{-# INLINE mkAbsoluteUrl #-}
mkAbsoluteUrl :: ByteString -> ByteString -> ByteString
mkAbsoluteUrl base url = 
  case isAbsoluteUri url of
    True  ->  url 
    False ->  base `B.append` url

-- Resolve a prefix using the given prefix mappings and base URL. If the prefix is
-- empty, then the base URL will be used if there is a base URL and if the map
-- does not contain an entry for the empty prefix.
resolveQName :: Maybe BaseUrl -> ByteString -> PrefixMappings -> ByteString
resolveQName mbaseUrl prefix (PrefixMappings pms') =
  case (mbaseUrl, B.null prefix) of
    (Just (BaseUrl base), True)  ->  Map.findWithDefault base B.empty pms'
    (Nothing,             True)  ->  err1
    (_,                   _   )  ->  Map.findWithDefault err2 prefix pms'
  where 
    err1 = error  "Cannot resolve empty QName prefix to a Base URL."
    err2 = error ("Cannot resolve QName prefix: " ++ B.unpack prefix)

newBaseUrl :: Maybe BaseUrl -> ByteString -> BaseUrl
newBaseUrl Nothing                url = BaseUrl url
newBaseUrl (Just (BaseUrl !bUrl)) url = BaseUrl $! mkAbsoluteUrl bUrl url

{-# INLINE isAbsoluteUri #-}
isAbsoluteUri :: ByteString -> Bool
isAbsoluteUri b = B.elem ':' b

{-# INLINE isBNodeListSubject #-}
isBNodeListSubject :: Resource -> Bool
isBNodeListSubject (R_Blank (B_POList _ _))   = True
isBNodeListSubject _                          = False

{-# INLINE isOResource #-}
isOResource :: Object -> Bool
isOResource (O_Resource _)                    = True
isOResource _                                 = False

{-# INLINE isOLiteral #-}
isOLiteral :: Object -> Bool
isOLiteral  (O_Literal _)                     = True
isOLiteral  _                                 = False

{-# INLINE isOBlankBNode #-}
isOBlankBNode :: Object -> Bool
isOBlankBNode    (O_Blank   (B_BlankId _))    = True
isOBlankBNode    _                            = False

{-# INLINE isOBlankBNodeGen #-}
isOBlankBNodeGen :: Object -> Bool
isOBlankBNodeGen (O_Blank (B_BlankGen _))     = True
isOBlankBNodeGen _                            = False

{-# INLINE isOBlankBNodeColl #-}
isOBlankBNodeColl :: Object -> Bool
isOBlankBNodeColl (O_Blank (B_Collection _))  = True
isOBlankBNodeColl _                           = False

{-# INLINE isOBlankPOList #-}
isOBlankPOList :: Object -> Bool
isOBlankPOList (O_Blank (B_POList _ _))       = True
isOBlankPOList _                              = False


------------------------------------------------------------------
-- The various parse methods that the module exposes externally. --
------------------------------------------------------------------

-- |Parse the given string as a Turtle document, using the given '(Maybe BaseUrl)' as the base URL,
-- if present, and using the given document URL as the URL of the Turtle document.
-- 
-- Returns either a 'ParseFailure' or a new graph containing the parsed triples.
parseString :: Graph gr => Maybe BaseUrl -> String -> String -> (Either ParseFailure gr)
parseString !bUrl !docUrl !ttlStr = handleResult bUrl (runParser t_turtleDoc initialState "" ttlStr)
  where initialState = (bUrl, Just (s2b docUrl), 1, PrefixMappings Map.empty, Nothing, Nothing, [])

-- |Parse the given file as a Turtle document, using the given '(Maybe BaseUrl)' as the base URL,
-- if present, and using the given document URL as the URL of the Turtle document. 
-- 
-- Returns either a 'ParseFailure' or a new graph containing the parsed triples.
parseFile :: Graph gr => Maybe BaseUrl -> String -> String -> IO (Either ParseFailure gr)
parseFile bUrl docUrl fpath = 
  readFile fpath >>= \str -> return $ handleResult bUrl (runParser t_turtleDoc initialState docUrl str)
  where initialState = (bUrl, Just (s2b docUrl), 1, PrefixMappings Map.empty, Nothing, Nothing, [])

{- Broken until dev-haskell/http and dev-haskell/http-simple are updated.
-- |Parse the document at the given location URL as a Turtle document, using the given '(Maybe BaseUrl)' 
-- as the base URL, if present, and using the given document URL as the URL of the Turtle document. The
-- document URL is for the purpose of resolving references to 'this document' within the document, and
-- may be different than the actual location URL from which the document is retrieved.
-- 
-- Returns either a 'ParseFailure' or a new graph containing the parsed triples.
parseURL :: Graph gr => Maybe BaseUrl -> String -> String -> (Either ParseFailure gr)
parseURL bUrl docUrl locUrl = _parseURL (parseString bUrl docUrl) locUrl
-}

handleResult :: Graph gr => Maybe BaseUrl -> Either ParseError (Triples, PrefixMappings) -> Either ParseFailure gr
handleResult !bUrl !result =
  case result of
    (Left err)         -> Left (ParseFailure $ show err)
    (Right (ts, pms))  -> Right $! mkGraph ts bUrl pms

