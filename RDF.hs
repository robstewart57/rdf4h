module RDF (Graph(empty, mkGraph, triplesOf, select, query, baseUrl),
            BaseUrl(BaseUrl),
            Triple, triple, Triples,sortTriples,
            Node(UNode, BNode, BNodeGen, LNode),
            LValue(PlainL, PlainLL, TypedL),
            NodeSelector, isUNode, isBNode, isLNode,
            subjectOf, predicateOf, objectOf,
            Subject, Predicate, Object,
            ParseFailure(ParseFailure),
            FastString(uniq,value),mkFastString,
            s2b,b2s,unode,bnode,lnode,plainL,plainLL,typedL,
            printT, printTs, printN, printNs)
where

import Namespace
import Utils
import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Control.Monad

import Text.Printf

-- |A convenience function for converting from a string to a bytestring.
{-# INLINE b2s #-}
b2s :: ByteString -> String
b2s = B.unpack

-- |A convenience function for converting from a bytestring to a string.
{-# INLINE s2b #-}
s2b :: String -> ByteString
s2b = B.pack

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
  | BNodeGen  {-# UNPACK #-} !Int
  -- |An RDF literal. See
  -- <http://www.w3.org/TR/rdf-concepts/#section-Graph-Literal> for more
  -- information.
  | LNode {-# UNPACK #-} !LValue

{-
I'm trying to fix a space leak and have a question about GHC and the UNPACK pragma and strict constructor fields. What are the conditions that would cause this not to be an optimization? If I read the GHC doc correctly, pattern matching on the constructor is okay and will not cause reboxing, but being passed to a function that takes the value as a non-strict function WILL cause reboxing. Is this correct? Are there other ways that reboxing can be forced?

Cale: passing to a polymorphic function will cause reboxing.
-}
instance Eq Node where
  (UNode fs1)    ==  (UNode fs2)     =  uniq fs1 == uniq fs2
  (BNode fs1)    ==  (BNode fs2)     =  uniq fs1 == uniq fs2
  (BNodeGen i1)  ==  (BNodeGen i2)   =  i1 == i2
  (LNode l1)     ==  (LNode l2)      =  l1 == l2
  _              ==  _               =  False

instance Ord Node where
  compare n1 n2 = compareNode n1 n2

{-# INLINE compareNode #-}
compareNode :: Node -> Node -> Ordering
compareNode (UNode fs1)                      (UNode fs2)                      = compareFS fs1 fs2
compareNode (UNode _)                        _                                = LT
compareNode (BNode fs1)                      (BNode fs2)                      = compareFS fs1 fs2
compareNode (BNode _)                        (UNode _)                        = GT
compareNode (BNode _)                        _                                = LT
compareNode (BNodeGen i1)                    (BNodeGen i2)                    = compare i1 i2
compareNode (BNodeGen _)                     (LNode _)                        = LT
compareNode (BNodeGen _)                     _                                = GT
compareNode (LNode (PlainL bs1))             (LNode (PlainL bs2))             = compare bs1 bs2
compareNode (LNode (PlainL _))               (LNode _)                        = LT
compareNode (LNode (PlainLL bs1 bs1'))       (LNode (PlainLL bs2 bs2'))       =
  case compare bs1' bs2' of
    EQ -> compare bs1 bs2
    LT -> LT
    GT -> GT
compareNode (LNode (PlainLL _ _))            (LNode (PlainL _))               = GT
compareNode (LNode (PlainLL _ _))            (LNode _)                        = LT
compareNode (LNode (TypedL bs1 fs1))         (LNode (TypedL bs2 fs2))         =
  case compare fs1 fs2 of
    EQ -> compare bs1 bs2
    LT -> LT
    GT -> GT
compareNode (LNode (TypedL _ _))             (LNode _)                        = GT
compareNode (LNode _)                        _                                = GT

-- |An RDF triple is a statement consisting of a subject, predicate,
-- and object, respectively.
--
-- See <http://www.w3.org/TR/rdf-concepts/#section-triples> for 
-- more information.
data Triple = Triple {-# UNPACK #-} !Node {-# UNPACK #-} !Node {-# UNPACK #-} !Node

instance Eq Triple where
  (Triple s1 p1 o1) == (Triple s2 p2 o2) = s1 == s2 && p1 == p2 && o1 == o2

instance Ord Triple where
  (Triple s1 p1 o1) `compare` (Triple s2 p2 o2) =
    case compareNode s1 s2 of
      EQ -> case compareNode p1 p2 of
              EQ -> compareNode o1 o2
              LT -> LT
              GT -> GT
      GT -> GT
      LT -> LT

sortTriples :: Triples -> Triples
sortTriples = sort

-- |A list of triples. This is defined for convenience and readability.
type Triples = [Triple]

-- TODO: check spec for equality and comparison of literals
-- |The actual value of an RDF literal, represented as the 'LValue'
-- parameter of an 'LNode'.
data LValue = 
  -- |A plain (untyped) literal value in an unspecified language.
  PlainL {-# UNPACK #-} !ByteString
  -- |A plain (untyped) literal value with a language specifier.
  | PlainLL {-# UNPACK #-} !ByteString {-# UNPACK #-} !ByteString 
  -- |A typed literal value consisting of the literal value and
  -- the URI of the datatype of the value, respectively.
  | TypedL {-# UNPACK #-} !ByteString {-# UNPACK #-} !FastString

instance Eq LValue where
  (PlainL bs1)        ==  (PlainL bs2)        =  bs1 == bs2
  (PlainLL bs1 bs1')  ==  (PlainLL bs2 bs2')  =  bs1' == bs2'    &&  bs1 == bs2
  (TypedL bs1 fs1)    ==  (TypedL bs2 fs2)    =  equalFS fs1 fs2 &&  bs1 == bs2
  _                   ==  _                   =  False

instance Ord LValue where
  compare l1 l2 = compareLValue l1 l2

{-# INLINE compareLValue #-}
compareLValue :: LValue -> LValue -> Ordering
compareLValue (PlainL bs1)       (PlainL bs2)       = compare bs1 bs2
compareLValue (PlainL _)         _                  = LT
compareLValue _                  (PlainL _)         = GT
compareLValue (PlainLL bs1 bs1') (PlainLL bs2 bs2') = 
  case compare bs1' bs2' of
    EQ -> compare bs1 bs2
    GT -> GT
    LT -> LT
compareLValue (PlainLL _ _)       _                 = LT
compareLValue _                   (PlainLL _ _)     = GT
compareLValue (TypedL l1 t1) (TypedL l2 t2) =
  case compareFS t1 t2 of
    EQ -> compare l1 l2
    GT -> GT
    LT -> LT

{-# INLINE subjectOf #-}
subjectOf :: Triple -> Node
subjectOf (Triple s _ _) = s

{-# INLINE predicateOf #-}
predicateOf :: Triple -> Node
predicateOf (Triple _ p _) = p

{-# INLINE objectOf #-}
objectOf :: Triple -> Node
objectOf (Triple _ _ o)   = o

-- |The base URL of a graph.
newtype BaseUrl = BaseUrl ByteString

-- |Represents a failure in parsing an N-Triples document, including
-- an error message with information about the cause for the failure.
newtype ParseFailure = ParseFailure String
  deriving (Eq, Show)


-- |Create a 'Triple' with the given subject, predicate, and object.
{-# INLINE triple #-}
triple :: Subject -> Predicate -> Object -> Triple
-- subject must be U or B
triple (LNode _) _         _    = error "subject cannot be LNode"
-- predicate must be U
triple _         (LNode _) _    = error "predicate cannot be LNode"
triple _         (BNode _) _    = error "predicate cannot be BNode"
-- no other constraints
triple subj      pred      obj  = Triple subj pred obj

-- String representations

instance Show Triple where
  show (Triple subj pred obj) = 
    printf "%s %s %s ." (show subj) (show pred) (show obj)

instance Show Node where
  show (UNode uri)                = printf "<%s>" (show uri)
  show (BNode  id)                = show id
  show (BNodeGen genId)           = "_:genid" ++ show genId
  show (LNode (PlainL lit))       = printf "\"%s\"" (show lit)
  show (LNode (PlainLL lit lang)) = printf "\"%s\"@\"%s\"" (show lit) (show lang)
  show (LNode (TypedL lit uri))   = printf "\"%s\"^^<%s>" (show lit) (show uri)

instance Show LValue where
  show (PlainL lit)               = printf "\"%s\"" (show lit)
  show (PlainLL lit lang)         = printf "\"%s\"@%s" (show lit) (show lang)
  show (TypedL lit dtype)         = printf "\"%s\"^^%s" (show lit) (show dtype)

-- Functions for testing type of a node --

-- |Answer if given node is a URI Ref node.
{-# INLINE isUNode #-}
isUNode :: Node -> Bool
isUNode (UNode _) = True
isUNode _         = False

-- |Answer if given node is a blank node.
{-# INLINE isBNode #-}
isBNode :: Node -> Bool
isBNode (BNode _)    = True
isBNode (BNodeGen _) = True
isBNode _            = False

-- |Answer if given node is a literal node.
{-# INLINE isLNode #-}
isLNode :: Node -> Bool
isLNode (LNode _) = True
isLNode _         = False

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
{-# INLINE unode #-}
unode :: ByteString -> IO Node
unode = liftM UNode . mkFastString

-- |A convenience function for creating a BNode for the given string id.
{-# INLINE bnode #-}
bnode :: ByteString -> IO Node
bnode = liftM BNode . mkFastString

-- |A convenience function for creating an LNode for the given LValue.
{-# INLINE lnode #-}
lnode :: LValue -> IO Node
lnode = return . LNode

-- |A convenience function for creating a PlainL LValue for the given
-- string value, and optionally, language identifier.
{-# INLINE plainL #-}
plainL :: String -> IO LValue
plainL = return . PlainL . s2b

plainLL :: String -> String -> IO LValue
plainLL val lang = return $ PlainLL (s2b val) (s2b lang)

-- |A convenience function for creating a TypedL LValue for the given
-- string value and datatype URI, respectively.
{-# INLINE typedL #-}
typedL :: String -> String -> IO LValue
typedL val dtype = liftM (TypedL $ s2b val) . mkFastString . s2b $ dtype

-- Utility functions for interactive experimentation
-- |Utility function to print a triple to stdout.
printT :: Triple -> IO ()
printT  = putStrLn . show

-- |Utility function to print a list of triples to stdout.
printTs :: [Triple] -> IO ()
printTs = mapM_ printT

-- |Utility function to print a node to stdout.
printN :: Node -> IO ()
printN  = putStrLn . show

-- |Utility function to print a list of nodes to stdout.
printNs :: [Node] -> IO ()
printNs = mapM_ printN
