-- |"TriplesGraph" contains a list-backed graph implementation suitable
-- for smallish graphs or for temporary graphs that will not be queried.
-- It maintains the triples in the order that they are given in, and is
-- especially useful for holding N-Triples, where it is often desirable
-- to preserve the order of the triples when they were originally parsed.
-- Duplicate triples are not filtered. If you might have duplicate triples,
-- use @MGraph@ instead, which is also more efficient. However, the query
-- functions of this graph (select, query) remove duplicates from their
-- result triples (but triplesOf does not) since it is usually cheap
-- to do so.
module Text.RDF.TriplesGraph(TriplesGraph, empty, mkGraph, triplesOf, select, query)

where

import Text.RDF.Core
import Text.RDF.Namespace

import qualified Data.Map as Map

import Data.List

-- |A simple implementation of the 'Graph' type class that represents
-- the graph internally as a list of triples.
--
-- Note that this type of graph is fine for interactive
-- experimentation and querying of smallish (<10,000 triples) graphs,
-- but there are better options for larger graphs or graphs that you
-- will do many queries against (e.g., @MGraph@ is faster for queries).
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

empty' :: TriplesGraph
empty' = TriplesGraph ([], Nothing, PrefixMappings Map.empty)

-- We no longer remove duplicates here, as it is very time consuming and is often not
-- necessary (raptor does not seem to remove dupes either). Instead, we remove dupes
-- from the results of the select' and query' functions, since it is cheap to do
-- there in most cases, but not when triplesOf' is called.
mkGraph' :: Triples -> Maybe BaseUrl -> PrefixMappings -> TriplesGraph
mkGraph' ts baseUrl pms = TriplesGraph (ts, baseUrl, pms)

triplesOf' :: TriplesGraph -> Triples
triplesOf' (TriplesGraph (ts, _, _)) = ts

select' :: TriplesGraph -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' (TriplesGraph (ts, _, _)) s p o = removeDupes $ filter (matchSelect s p o) ts

query' :: TriplesGraph -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
query' (TriplesGraph (ts, _, _)) s p o = removeDupes $ filter (matchPattern s p o) ts

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
