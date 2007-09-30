module RDF (Graph(empty, mkGraph, triplesOf, select, querySubj, querySubjPred),
            Triple, triple,
            Node(UNode, BNode, LNode),
            LValue(PlainL, TypedL),
            Selector, isUNode, isBNode, isLNode,
              hasSubject, hasPredicate, hasObject,
            subjectOf, predicateOf, objectOf, distinct,
            printT, printTs, printN, printNs)
where

import Namespace()

import qualified Data.Set as Set
import Text.Printf

class Graph gr where
  -- |Answer an empty graph.
  empty  :: gr
  -- |Answer a graph containing all the given triples.
  mkGraph :: [Triple] -> gr
  -- |Answer a list of all triples in the graph.
  triplesOf :: gr -> [Triple]
  -- |Select the triples in graph that match the selector.
  -- This function is a convenience function for interactive
  -- querying of smallish (<= 10,000 triples) graphs, as it
  -- must check all triples in the graph.
  select :: Selector -> gr -> [Triple]
  querySubj :: gr -> Node -> [Triple]
  querySubjPred :: gr -> Node -> Node -> [Triple]

-- |An RDF node, which may be either a URIRef node (UNode), a blank 
-- node (BNode), or a literal node (LNode).
data Node =  UNode String               -- a uri ref node
           | BNode String               -- a blank node
           | LNode LValue               -- a literal node
  deriving (Eq, Ord)


-- |An RDF statement is a Triple consisting of Subject, Predicate,
--  and Object nodes.
data Triple = Triple Node Node Node
  deriving (Eq)

-- TODO: check spec for equality and comparison of literals
-- |A literal value, which may be a plain literal, optionally with
-- a language specifier, or a typed literal consisting of the value
-- and the URI of the datatype, respectively.
data LValue = PlainL String (Maybe String) -- a plain literal w/ opt lang
            | TypedL String String         -- a typed literal w/ type uri
  deriving (Eq, Ord)

-- |Create a triple with the given subject, predicate, and object.
triple :: Node -> Node -> Node -> Triple
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
  show (UNode uri)                      = printf "<%s>" uri
  show (BNode  id)                      = show id
  show (LNode (PlainL lit Nothing))     = printf "\"%s\"" lit
  show (LNode (PlainL lit (Just lang))) = printf "\"%s\"@\"%s\"" lit lang
  show (LNode (TypedL lit uri))         = printf "\"%s\"^^<%s>" lit uri

-- Functions for testing type of a node --

-- |Answer if given node is a URI Ref node.
isUNode :: Node -> Bool
isUNode (UNode _) = True
isUNode _         = False
-- |Answer if given node is a blank node.
isBNode :: Node -> Bool
isBNode (BNode _) = True
isBNode _         = False
-- |Answer if given node is a literal node.
isLNode :: Node -> Bool
isLNode (LNode _) = True
isLNode _         = False

-- Functions for extracting a node from a triple --
-- |Extract the subject of a triple.
subjectOf     ::  Triple -> Node
subjectOf    (Triple s _ _) = s
-- |Extract the predicate of a triple.
predicateOf   ::  Triple -> Node
predicateOf  (Triple _ p _) = p
-- |Extract the object of a triple.
objectOf      ::  Triple -> Node
objectOf     (Triple _ _ o) = o

-- |Eliminate duplicates from a list.
distinct :: Ord a => [a] -> [a]
distinct = Set.toList . Set.fromList

-- Simplest possible selector: is passed each triple and says yay or nay
-- |A function that returns True or False when given three nodes
-- representing the subject, predicate, and object, respectively.
type Selector = Node -> Node -> Node -> Bool

-- Convenience functions for user with select.

-- |Create a selector that matches triples with the given node as Subject.
hasSubject     :: Node -> Selector
hasSubject     u@(UNode _)  s _ _   =   u == s
hasSubject     b@(BNode _)  s _ _   =   b == s
hasSubject       (LNode _)  _ _ _   =   False
-- |Create a selector that matches triples with the given node 
-- as predicate.
hasPredicate   :: Node -> Selector
hasPredicate   u@(UNode _)  _ p _   =   u == p
hasPredicate     _          _ _ _   =   False
-- |Create a selector that matches triples with the given node as object.
hasObject      :: Node -> Selector
hasObject      u            _ _ o   =   u == o

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