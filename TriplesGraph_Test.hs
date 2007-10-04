import RDF
import TriplesGraph
import Namespace

import GraphTestUtils

import Data.Char
import Data.List
import Data.Set.AVL(fromList, toList)
import Control.Monad
import System.Random
import Test.QuickCheck

-- Utility methods unique to triples graph

instance Arbitrary TriplesGraph where
  arbitrary = liftM mkGraph arbitraryTs
  coarbitrary = undefined

instance Show TriplesGraph where
  --show gr = "Graph(n=" ++ show (length $ triplesOf gr) ++ ")"
  show gr = concatMap (\t -> show t ++ "\n")  (triplesOf gr)

graph :: Triples -> TriplesGraph
graph = mkGraph

_empty :: TriplesGraph
_empty = empty

_mkGraph :: Triples -> TriplesGraph
_mkGraph = mkGraph

_triplesOf :: TriplesGraph -> Triples
_triplesOf = triplesOf

-- The generic tests that apply to all graph implementations  --

prop_empty :: Bool
prop_empty = p_empty _triplesOf _empty

prop_mkGraph_triplesOf :: Triples -> Bool
prop_mkGraph_triplesOf = p_mkGraph_triplesOf _triplesOf _mkGraph

prop_mkGraph_no_dupes :: Triples -> Bool
prop_mkGraph_no_dupes = p_mkGraph_no_dupes _triplesOf _mkGraph

prop_query_all_wildcard :: Triples -> Bool
prop_query_all_wildcard = p_query_all_wildcard _mkGraph

prop_query_matched_no_wildcards :: TriplesGraph -> Property
prop_query_matched_no_wildcards = p_query_matched_no_wildcards _triplesOf

prop_query_unmatched_no_wildcards :: TriplesGraph -> Triple -> Property
prop_query_unmatched_no_wildcards = p_query_unmatched_no_wildcards _triplesOf

prop_query_matched_po_wildcards :: TriplesGraph -> Property
prop_query_matched_po_wildcards = p_query_matched_po_wildcards _triplesOf