import RDF
import Namespace

import Control.Monad
import GraphTestUtils
import Test.QuickCheck

import TriplesGraph

----------------------------------------------------
-- instances and graph functions for TriplesGraph --
----------------------------------------------------

instance Arbitrary TriplesGraph where
  arbitrary = liftM mkGraph arbitraryTs
  coarbitrary = undefined

instance Show TriplesGraph where
  --show gr = "Graph(n=" ++ show (length $ triplesOf gr) ++ ")"
  show gr = concatMap (\t -> show t ++ "\n")  (triplesOf gr)

_empty :: TriplesGraph
_empty = empty

_mkGraph :: Triples -> TriplesGraph
_mkGraph = mkGraph

_triplesOf :: TriplesGraph -> Triples
_triplesOf = triplesOf

----------------------------------------------------
--  generic tests parameterized for TriplesGraph  --
----------------------------------------------------

prop_empty :: Bool
prop_empty = p_empty _triplesOf _empty

prop_mkGraph_triplesOf :: Triples -> Bool
prop_mkGraph_triplesOf = p_mkGraph_triplesOf _triplesOf _mkGraph

prop_mkGraph_no_dupes :: Triples -> Bool
prop_mkGraph_no_dupes = p_mkGraph_no_dupes _triplesOf _mkGraph

prop_query_all_wildcard :: Triples -> Bool
prop_query_all_wildcard = p_query_all_wildcard _mkGraph

prop_query_matched_spo :: TriplesGraph -> Property
prop_query_matched_spo = p_query_matched_spo _triplesOf

prop_query_unmatched_spo :: TriplesGraph -> Triple -> Property
prop_query_unmatched_spo = p_query_unmatched_spo _triplesOf

prop_query_match_s :: TriplesGraph -> Property
prop_query_match_s = p_query_match_s _triplesOf

prop_query_match_p :: TriplesGraph -> Property
prop_query_match_p = p_query_match_p _triplesOf

prop_query_match_o :: TriplesGraph -> Property
prop_query_match_o = p_query_match_o _triplesOf

prop_query_match_sp :: TriplesGraph -> Property
prop_query_match_sp = p_query_match_sp _triplesOf

prop_query_match_so :: TriplesGraph -> Property
prop_query_match_so = p_query_match_so _triplesOf

prop_query_match_po :: TriplesGraph -> Property
prop_query_match_po = p_query_match_po _triplesOf