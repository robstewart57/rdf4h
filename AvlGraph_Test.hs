import RDF
import Namespace

import Control.Monad
import GraphTestUtils
import Test.QuickCheck

import AvlGraph

----------------------------------------------------
--   instances and graph functions for AvlGraph   --
----------------------------------------------------

instance Arbitrary AvlGraph where
  arbitrary = liftM mkGraph arbitraryTs
  coarbitrary = undefined

instance Show AvlGraph where
  --show gr = "Graph(n=" ++ show (length $ triplesOf gr) ++ ")"
  show gr = concatMap (\t -> show t ++ "\n")  (triplesOf gr)

_empty :: AvlGraph
_empty = empty

_mkGraph :: Triples -> AvlGraph
_mkGraph = mkGraph

_triplesOf :: AvlGraph -> Triples
_triplesOf = triplesOf

----------------------------------------------------
--    generic tests parameterized for AvlGraph    --
----------------------------------------------------

prop_empty :: Bool
prop_empty = p_empty _triplesOf _empty

prop_mkGraph_triplesOf :: Triples -> Bool
prop_mkGraph_triplesOf = p_mkGraph_triplesOf _triplesOf _mkGraph

prop_mkGraph_no_dupes :: Triples -> Bool
prop_mkGraph_no_dupes = p_mkGraph_no_dupes _triplesOf _mkGraph

prop_query_match_none :: Triples -> Bool
prop_query_match_none = p_query_match_none _mkGraph

prop_query_matched_spo :: AvlGraph -> Property
prop_query_matched_spo = p_query_matched_spo _triplesOf

prop_query_unmatched_spo :: AvlGraph -> Triple -> Property
prop_query_unmatched_spo = p_query_unmatched_spo _triplesOf

prop_query_match_s :: AvlGraph -> Property
prop_query_match_s = p_query_match_s _triplesOf

prop_query_match_p :: AvlGraph -> Property
prop_query_match_p = p_query_match_p _triplesOf

prop_query_match_o :: AvlGraph -> Property
prop_query_match_o = p_query_match_o _triplesOf

prop_query_match_sp :: AvlGraph -> Property
prop_query_match_sp = p_query_match_sp _triplesOf

prop_query_match_so :: AvlGraph -> Property
prop_query_match_so = p_query_match_so _triplesOf

prop_query_match_po :: AvlGraph -> Property
prop_query_match_po = p_query_match_po _triplesOf

prop_select_match_none :: AvlGraph -> Bool
prop_select_match_none = p_select_match_none

prop_select_match_s :: AvlGraph -> Property
prop_select_match_s = p_select_match_s _triplesOf

prop_select_match_p :: AvlGraph -> Property
prop_select_match_p = p_select_match_p _triplesOf

prop_select_match_o :: AvlGraph -> Property
prop_select_match_o = p_select_match_o _triplesOf

prop_select_match_sp :: AvlGraph -> Property
prop_select_match_sp = p_select_match_sp _triplesOf

prop_select_match_so :: AvlGraph -> Property
prop_select_match_so = p_select_match_so _triplesOf

prop_select_match_po :: AvlGraph -> Property
prop_select_match_po = p_select_match_po _triplesOf

prop_select_match_spo :: AvlGraph -> Property
prop_select_match_spo = p_select_match_spo _triplesOf