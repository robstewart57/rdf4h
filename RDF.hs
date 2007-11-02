module RDF (Graph(empty, mkGraph, triplesOf, select, query, baseUrl),
            BaseUrl(BaseUrl),
            Triple, triple, Triples,sortTriples,
            Node(UNode, BNode, BNodeGen, LNode),
            LValue(PlainL, TypedL),
            NodeSelector, isUNode, isBNode, isLNode,
            subjectOf, predicateOf, objectOf,
            Subject, Predicate, Object,
            ParseFailure(ParseFailure),
            FastString(uniq,len,value),mkFastString,mkFastStringR,
            s2b,b2s,unode,bnode,lnode,plainL,typedL,
            printT, printTs, printN, printNs)
where

import Namespace
import Utils
import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List

import Text.Printf

-- |A convenience function for converting from a string to a bytestring.
b2s :: ByteString -> String
b2s !s = B.unpack $! s

-- |A convenience function for converting from a bytestring to a string.
s2b :: String -> ByteString
s2b !s = B.pack $! s

-- |An alias for 'Node', defined for convenience and readability purposes.
type Subject = Node

-- |An alias for 'Node', defined for convenience and readability purposes.
type Predicate = Node

-- |An alias for 'Node', defined for convenience and readability purposes.
type Object = Node

-- |An RDF graph is a set of (unique) RDF triples, together with the 
-- operations defined upon the graph.
-- 
-- For information about the efficiency of the functions, see the
-- documentation for the particular graph instance.
-- 
-- For more information about the concept of an RDF graph, see
-- the following: <http://www.w3.org/TR/rdf-concepts/#section-rdf-graph>.
class Graph gr where
  -- |Answer the base URL of this graph, if any.
  baseUrl :: gr -> Maybe BaseUrl
  -- |Answer an empty graph.
  empty  :: gr
  -- |Answer a graph containing all the given triples. Duplicate triples
  -- are permitted in the input, but the resultant graph will contains only 
  -- unique triples.
  mkGraph :: Triples -> Maybe BaseUrl -> PrefixMappings -> IO gr
  -- |Answer a list of all triples in the graph.
  triplesOf :: gr -> Triples
  -- |Answer the triples in the graph that match the given selectors.
  -- 
  -- The three NodeSelector parameters are optional functions that match
  -- the respective subject, predicate, and object of a triple. The triples
  -- returned are those in the given graph for which the first selector 
  -- returns true when called on the subject, the second selector returns
  -- true when called on the predicate, and the third selector returns true
  -- when called on the ojbect. A 'Nothing' parameter is equivalent to a 
  -- function that always returns true for the appropriate node; but 
  -- implementations may be able to much more efficiently answer a select
  -- that involves a 'Nothing' parameter rather than an @(id True)@ parameter.
  -- 
  -- The following call illustrates the use of select, and would result in
  -- the selection of all and only the triples that have a blank node 
  -- as subject and a literal node as object:
  -- 
  -- > select gr (Just isBNode) Nothing (Just isLNode)
  --
  -- Note: this function may be very slow; see the documentation for the 
  -- particular graph implementation for more information.
  select    :: gr -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
  -- |Answer the triples in the graph that match the given pattern, where
  -- the pattern (3 Maybe Node parameters) is interpreted as a triple pattern.
  -- 
  -- The @Maybe Node@ params are interpreted as the subject, predicate, and
  -- object of a triple, respectively. @Just n@ is true iff the triple has
  -- a node equal to @n@ in the appropriate location; @Nothing@ is always
  -- true, regardless of the node in the appropriate location.
  -- 
  -- For example, @ query gr (Just n1) Nothing (Just n2) @ would return all
  -- and only the triples that have @n1@ as subject and @n2@ as object, 
  -- regardless of the predicate of the triple.
  query         :: gr -> Maybe Node -> Maybe Node -> Maybe Node -> Triples

-- |An RDF node, which may be either a URIRef node ('UNode'), a blank 
-- node ('BNode'), or a literal node ('LNode').
data Node =  
  -- |An RDF URI reference. See
  -- <http://www.w3.org/TR/rdf-concepts/#section-Graph-URIref> for more
  -- information.
  UNode {-# UNPACK #-} !FastString
  -- |An RDF blank node. See
  -- <http://www.w3.org/TR/rdf-concepts/#section-blank-nodes> for more
  -- information.
  | BNode {-# UNPACK #-} !FastString
  -- |An RDF blank node with an auto-generated identifier, as used in 
  -- Turtle.
  | BNodeGen {-# UNPACK #-} !Int
  -- |An RDF literal. See
  -- <http://www.w3.org/TR/rdf-concepts/#section-Graph-Literal> for more
  -- information.
  | LNode {-# UNPACK #-} !LValue

instance Eq Node where
  (==) !n1 !n2 = equalNode n1 n2

equalNode :: Node -> Node -> Bool
equalNode !(UNode !u1)     !(UNode !u2)    = u1 == u2
equalNode !(UNode !_)      !_              = False
equalNode !(BNode !s1)     !(BNode !s2)    = s1 == s2
equalNode !(BNode !_)      !_              = False
equalNode !(BNodeGen !i1)  !(BNodeGen !i2) = i1 == i2
equalNode !(BNodeGen !_)   !_              = False
equalNode !(LNode !lv1)    !(LNode !lv2)   = lv1 == lv2
equalNode !(LNode !_)      !_              = False

instance Ord Node where
  compare !n1 !n2 = compareNode n1 n2

compareNode :: Node -> Node -> Ordering
compareNode !(UNode !u1) !(UNode !u2)       = 
  case uniq u1 == uniq u2 of
    True  -> EQ
    False -> compare (value $! u1) (value $! u2)
compareNode !(UNode !_)  !_                   = LT
compareNode !(BNode !s1) !(BNode !s2)         = 
  case uniq s1 == uniq s2 of
    True  -> EQ
    False -> compare (value $! s1) (value $! s2)
compareNode !(BNode !_)  !(UNode !_)          = GT
compareNode !(BNode !_)  !_                   = LT
compareNode !(BNodeGen !i1) !(BNodeGen !i2)   = compare i1 i2
compareNode !(BNodeGen !_)  !(LNode !_)       = GT
compareNode !(BNodeGen !_)  !_                = LT
compareNode !(LNode !l1) !(LNode !l2)         = compareLValue l1 l2
compareNode !(LNode !_) !_                    = GT


-- |An RDF triple is a statement consisting of a subject, predicate,
-- and object, respectively.
--
-- See <http://www.w3.org/TR/rdf-concepts/#section-triples> for 
-- more information.
newtype Triple = Triple (Node, Node, Node)

subjectOf :: Triple -> Node
subjectOf !(Triple (!s, !_, !_)) = s

predicateOf :: Triple -> Node
predicateOf !(Triple (!_,!p, !_)) = p

objectOf :: Triple -> Node
objectOf !(Triple (!_, !_, !o))   = o

instance Eq Triple where
  (==) !t1 !t2 = equalTriple t1 t2

equalTriple :: Triple -> Triple -> Bool
equalTriple !t1 !t2 = 
  (objectOf $! t1)     `equalNode`  (objectOf $! t2)    &&
  (predicateOf $! t1)  `equalNode`  (predicateOf $! t2) &&
  (subjectOf $! t1)    `equalNode`  (subjectOf $! t2)

instance Ord Triple where
  compare !t1 !t2 = compareTriple t1 t2

compareTriple :: Triple -> Triple -> Ordering
compareTriple !t1 !t2 = 
  case compS of
    EQ -> case compP of
            EQ -> compO
            sp -> sp
    sc  -> sc
  where
    compS = compareNode (subjectOf $! t1) (subjectOf $! t2)
    compP = compareNode (predicateOf t1) (predicateOf t2)
    compO = compareNode (objectOf t1) (objectOf t2)

-- |A list of triples. This is defined for convenience and readability.
type Triples = [Triple]

-- TODO: check spec for equality and comparison of literals
-- |The actual value of an RDF literal, represented as the 'LValue'
-- parameter of an 'LNode'.
data LValue = 
  -- |A plain literal value, with an optional language specifier.
  PlainL {-# UNPACK #-} !ByteString {-# UNPACK #-} !(Maybe ByteString)
  -- |A typed literal value consisting of the literal value and
  -- the URI of the datatype of the value, respectively.
  | TypedL {-# UNPACK #-} !ByteString {-# UNPACK #-} !ByteString

instance Eq LValue where
  l1 == l2 = l1 `equalLValue` l2

equalLValue :: LValue -> LValue -> Bool
equalLValue  !(PlainL _ _) !(TypedL _ _) = False
equalLValue  !(TypedL _ _) !(PlainL _ _) = False
equalLValue  !(PlainL _ Nothing) !(PlainL _ (Just _)) = False
equalLValue  !(PlainL _ (Just _)) !(PlainL _ Nothing) = False
equalLValue  !(PlainL !l1 (Just l1')) !(PlainL !l2 (Just !l2')) = l1' == l2' && l1 == l2
equalLValue  !(PlainL !l1 Nothing) !(PlainL !l2 Nothing) = l1 == l2
equalLValue  !(TypedL !l1 !t1) !(TypedL !l2 !t2) = l1 == l2 && t1 == t2

instance Ord LValue where
  compare !l1 !l2 = compareLValue l1 l2

compareLValue :: LValue -> LValue -> Ordering
compareLValue !(PlainL _ _) !(TypedL _ _) = LT
compareLValue !(TypedL _ _) !(PlainL _ _) = GT
compareLValue !(PlainL _ !Nothing) !(PlainL _ !(Just _)) = LT
compareLValue !(PlainL _ !(Just _)) !(PlainL _ !Nothing) = GT
compareLValue !(PlainL !l1 !Nothing) !(PlainL !l2 !Nothing) = compare l1 l2
compareLValue !(PlainL !l1 !(Just l1')) !(PlainL !l2 !(Just l2')) =
  case compare l1' l2' of
    EQ -> compare l1 l2
    c  -> c
compareLValue (TypedL !l1 !t1) (TypedL !l2 !t2) =
  case compare l1 l2 of
    EQ -> compare t1 t2
    c  -> c

-- |The base URL of a graph.
newtype BaseUrl = BaseUrl ByteString

-- |Represents a failure in parsing an N-Triples document, including
-- an error message with information about the cause for the failure.
newtype ParseFailure = ParseFailure String
  deriving (Eq, Show)


-- |Create a 'Triple' with the given subject, predicate, and object.
triple :: Subject -> Predicate -> Object -> Triple
-- subject must be U or B
triple !(LNode !_) !_         !_    = error "subject cannot be LNode"
-- predicate must be U
triple !_         !(LNode !_) !_    = error "predicate cannot be LNode"
triple !_         !(BNode !_) !_    = error "predicate cannot be BNode"
-- no other constraints
triple !subj      !pred       !obj  = Triple $! (subj, pred, obj)


-- String representations

instance Show Triple where
  show (Triple (subj, pred, obj)) = 
    printf "%s %s %s ." (show subj) (show pred) (show obj)

instance Show Node where
  show (UNode uri)                      = printf "<%s>" (show uri)
  show (BNode  id)                      = show id
  show (BNodeGen genId)                 = "_:genid" ++ show genId
  show (LNode (PlainL lit Nothing))     = printf "\"%s\"" (show lit)
  show (LNode (PlainL lit (Just lang))) = printf "\"%s\"@\"%s\"" (show lit) (show lang)
  show (LNode (TypedL lit uri))         = printf "\"%s\"^^<%s>" (show lit) (show uri)

instance Show LValue where
  show (PlainL lit Nothing)     = printf "\"%s\"" (show lit)
  show (PlainL lit (Just lang)) = printf "\"%s\"@%s" (show lit) (show lang)
  show (TypedL lit dtype)       = printf "\"%s\"^^%s" (show lit) (show dtype)

sortTriples :: Triples -> Triples
sortTriples !ts = sort ts
--  sortBy (compareByNode subjectOf)    $!
--  sortBy (compareByNode objectOf)     $!
--  sortBy (compareByNode predicateOf)  ts

compareByNode :: (Triple -> Node) -> Triple -> Triple -> Ordering
compareByNode !fn !t1 !t2 = compareNode (fn $! t1) (fn $! t2)


-- Functions for testing type of a node --

-- |Answer if given node is a URI Ref node.
isUNode :: Node -> Bool
isUNode !(UNode _) = True
isUNode !_         = False
-- |Answer if given node is a blank node.
isBNode :: Node -> Bool
isBNode !(BNode _)    = True
isBNode !(BNodeGen _) = True
isBNode !_            = False
-- |Answer if given node is a literal node.
isLNode :: Node -> Bool
isLNode !(LNode _) = True
isLNode !_         = False

-- |A 'NodeSelector' is either a function that returns 'True'
--  or 'False' for a node, or Nothing, which indicates that all
-- nodes would return 'True'.
-- 
-- The selector It is said to select, or match, the nodes for
-- which it returns 'True'.
--
-- When used in conjunction with the 'select' method of 'Graph', three
-- node selectors are used to match a triple.
type NodeSelector = Maybe (Node -> Bool)

-- |A convenience function for creating a UNode for the given String URI.
unode :: ByteString -> IO Node
unode !str = mkFastStringR str >>= return . UNode

-- |A convenience function for creating a BNode for the given string id. 
bnode :: ByteString -> IO Node
bnode !str = mkFastStringR str >>= return . BNode

-- |A convenience function for creating an LNode for the given LValue.
lnode :: LValue -> IO Node
lnode !str = return $ LNode str

-- |A convenience function for creating a PlainL LValue for the given
-- string value, and optionally, language identifier.
plainL :: String -> Maybe String -> LValue
plainL !str Nothing    = PlainL (s2b str) Nothing
plainL !str (Just !lng) = PlainL (s2b str) (Just $ s2b lng)

-- |A convenience function for creating a TypedL LValue for the given
-- string value and datatype URI, respectively.
typedL :: String -> String -> LValue
typedL !str !uri        = TypedL (s2b str) (s2b uri)

-- Utility functions for interactive experimentation
-- |Utility function to print a triple to stdout.
printT :: Triple -> IO ()
printT  = putStrLn . show
-- |Utility function to print a list of triples to stdout.
printTs :: [Triple] -> IO ()
printTs = mapM_ printT
-- |Utility function to print a node to stdout.
printN :: Node -> IO ()
printN  = putStrLn . show :: Node   -> IO ()
-- |Utility function to print a list of nodes to stdout.
printNs :: [Node] -> IO ()
printNs = mapM_ printN
