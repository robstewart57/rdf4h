-- |A simple graph instance represented as a list of triples.

module TriplesGraph(TriplesGraph) where

import RDF

-- |The simplest possible representation of a Graph.
newtype TriplesGraph = TriplesGraph [Triple]

instance Graph TriplesGraph where
  empty                                 = TriplesGraph []
  mkGraph                               = TriplesGraph
  triplesOf     (TriplesGraph ts)       = ts
  select sl     (TriplesGraph ts)       = filter (matchSelector sl) ts
  query         (TriplesGraph ts) s p o = filter (matchPattern s p o) ts

matchSelector :: Selector -> Triple -> Bool
matchSelector sl t = sl (subjectOf t) (predicateOf t) (objectOf t)

matchPattern :: Maybe Subject -> Maybe Predicate -> Maybe Object 
                              -> Triple -> Bool
matchPattern s p o t = smatch t && pmatch t && omatch t
  where
    smatch t = matchNode s (subjectOf t)
    pmatch t = matchNode p (predicateOf t)
    omatch t = matchNode o (objectOf t)

matchNode :: Maybe Node -> Node -> Bool
matchNode Nothing   _  = True
matchNode (Just n1) n2 = n1 == n2