-- |A simple graph instance represented as a list of triples.

module TriplesGraph(TriplesGraph) where

import Data.Set.AVL(toList,fromList)
import RDF

-- |The simplest possible representation of a Graph.
newtype TriplesGraph = TriplesGraph [Triple]

instance Graph TriplesGraph where
  empty                                 = TriplesGraph []
  mkGraph                               = TriplesGraph . toList . fromList
  triplesOf     (TriplesGraph ts)       = ts
  select sl     (TriplesGraph ts)       = filter (matchSelector sl) ts
  query         (TriplesGraph ts) s p o = filter (matchPattern s p o) ts

matchSelector :: Selector -> Triple -> Bool
matchSelector sl t = sl (subjectOf t) (predicateOf t) (objectOf t)

matchPattern :: Maybe Subject -> Maybe Predicate -> Maybe Object 
                              -> Triple -> Bool
matchPattern subj pred obj t = smatch t && pmatch t && omatch t
  where
    smatch trp = matchNode subj (subjectOf trp)
    pmatch trp = matchNode pred (predicateOf trp)
    omatch trp = matchNode obj (objectOf trp)

matchNode :: Maybe Node -> Node -> Bool
matchNode Nothing   _  = True
matchNode (Just n1) n2 = n1 == n2
