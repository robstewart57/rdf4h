-- |"TriplesGraph" contains a list-backed graph implementation suitable
-- for smallish graphs.
module TriplesGraph(TriplesGraph, 
                    empty, mkGraph, triplesOf, select, query)

where

import Data.Set.AVL(toList,fromList)
import RDF

-- |A simple implementation of the 'Graph' type class that represents
-- the graph internally as a list of triples. 
--
-- Note that this type of graph is fine for interactive
-- experimentation and querying of smallish (<10,000 triples) graphs,
-- but there are better options for larger graphs or graphs that you
-- will do many queries against (e.g., 'AvlGraph' is faster for queries).
-- 
-- The time complexity of the functions (where n == num_triples) are:
--
--  * 'empty'    : O(1)
--
--  * 'mkGraph'  : O(n)
--
--  * 'triplesOf': O(1)
--
--  * 'select'   : O(n)
--
--  * 'query'    : O(n)
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
