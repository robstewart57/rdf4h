module RDF (Graph(Graph), triplesOf,
            Triple, triple,
            Node(UNode, BNode, LNode),
            LValue(PlainL, TypedL),
            Selector, select, isUNode, isBNode, isLNode,
              hasSubject, hasPredicate, hasObject,
            subjectOf, predicateOf, objectOf, distinct,
            printT, printTs, printN, printNs)
where

import Namespace

import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf

newtype Graph = Graph [Triple]

data Node =  UNode String               -- a uri ref node
           | BNode String               -- a blank node
           | LNode LValue               -- a literal node
  deriving (Eq, Ord)

triplesOf :: Graph -> [Triple]
triplesOf (Graph ts) = ts

data Triple = Triple Node Node Node
  deriving (Eq)

-- TODO: check spec for equality and comparison of literals
data LValue = PlainL String (Maybe String) -- a plain literal w/ opt lang
            | TypedL String String         -- a typed literal w/ type uri
  deriving (Eq, Ord)

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

-- Functions for testing type of a node
isUNode, isBNode, isLNode :: Node -> Bool
isUNode (UNode _) = True
isUNode _         = False
isBNode (BNode _) = True
isBNode _         = False
isLNode (LNode _) = True
isLNode _         = False

-- Functions for extracting a node from a triple
subjectOf, predicateOf, objectOf :: Triple -> Node
subjectOf    (Triple s _ _) = s
predicateOf  (Triple _ p _) = p
objectOf     (Triple _ _ o) = o

-- Eliminates duplicates from a list.
distinct :: Ord a => [a] -> [a]
distinct = Set.toList . Set.fromList

-- Simplest possible selector: is passed each triple and says yay or nay
type Selector = Node -> Node -> Node -> Bool
select :: Selector -> Graph -> [Triple]
select sl (Graph ts) = filter (\(Triple s p o) -> sl s p o) ts

-- Convenience functions for user with select.
hasSubject, hasPredicate, hasObject :: Node -> Node -> Node -> Node -> Bool
hasSubject     u@(UNode _)  s _ _   =   u == s
hasSubject     b@(BNode _)  s _ _   =   b == s
hasSubject       (LNode _)  _ _ _   =   False
hasPredicate   u@(UNode _)  _ p _   =   u == p
hasPrecicate     _          _ _ _   =   False
hasObject      u            _ _ o   =   u == o

-- Utility functions for interactive experimentation
printT  = putStrLn . show :: Triple -> IO ()
printTs = mapM_ printT
printN  = putStrLn . show :: Node   -> IO ()
printNs = mapM_ printN