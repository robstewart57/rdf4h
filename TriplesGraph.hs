-- |"TriplesGraph" contains a list-backed graph implementation suitable
-- for smallish graphs.
module TriplesGraph(TriplesGraph, empty, mkGraph, triplesOf, select, query)

where

import qualified Data.Map as Map
--import qualified Data.Set.AVL as Set

import Data.List

import RDF
import Namespace

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
newtype TriplesGraph = TriplesGraph (Triples, Maybe BaseUrl, PrefixMappings)

instance Graph TriplesGraph where
  baseUrl         = baseUrl'
  prefixMappings  = prefixMappings'
  empty           = empty'
  mkGraph         = mkGraph'
  triplesOf       = triplesOf'
  select          = select'
  query           = query'

prefixMappings' :: TriplesGraph -> PrefixMappings
prefixMappings' (TriplesGraph (_, _, pms)) = pms

baseUrl' :: TriplesGraph -> Maybe BaseUrl
baseUrl' (TriplesGraph (_, baseUrl, _)) = baseUrl

{-# NOINLINE empty' #-}
empty' :: TriplesGraph
empty' = TriplesGraph ([], Nothing, PrefixMappings Map.empty)

mkGraph' :: Triples -> Maybe BaseUrl -> PrefixMappings -> TriplesGraph
mkGraph' ts baseUrl pms = TriplesGraph $! (dupeFreeTs, baseUrl, pms)
  where dupeFreeTs = nub . sort $! ts

triplesOf' :: TriplesGraph -> Triples
triplesOf' (TriplesGraph (ts, _, _)) = ts

select' :: TriplesGraph -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' (TriplesGraph (ts, _, _)) s p o = filter (matchSelect s p o) ts

query' :: TriplesGraph -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
query' (TriplesGraph (ts, _, _)) s p o = filter (matchPattern s p o) ts

matchSelect :: NodeSelector -> NodeSelector -> NodeSelector -> Triple -> Bool
matchSelect s p o t = 
  match s (subjectOf t) && match p (predicateOf t) && match o (objectOf t)
  where 
    match Nothing   _ = True
    match (Just fn) n = fn n

matchPattern :: Maybe Subject -> Maybe Predicate -> Maybe Object -> Triple -> Bool
matchPattern subj pred obj t = smatch t && pmatch t && omatch t
  where
    smatch trp = matchNode subj (subjectOf trp)
    pmatch trp = matchNode pred (predicateOf trp)
    omatch trp = matchNode obj (objectOf trp)

matchNode :: Maybe Node -> Node -> Bool
matchNode Nothing   _  = True
matchNode (Just n1) n2 = n1 == n2
