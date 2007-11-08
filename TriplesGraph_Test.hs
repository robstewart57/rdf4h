import RDF
import Namespace

import Control.Monad
--import Data.Map(Map)
import qualified Data.Map as Map
import GraphTestUtils
import Test.QuickCheck

import TriplesGraph

----------------------------------------------------
-- instances and graph functions for TriplesGraph --
----------------------------------------------------

instance Arbitrary TriplesGraph where
  arbitrary = liftM3 mkGraph arbitraryTs (return Nothing) (return $ PrefixMappings Map.empty)
  coarbitrary = undefined

instance Show TriplesGraph where
  show gr = concatMap (\t -> show t ++ "\n")  (triplesOf gr)

_empty :: TriplesGraph
_empty = empty

_mkGraph :: Triples -> Maybe BaseUrl -> PrefixMappings -> TriplesGraph
_mkGraph = mkGraph

_triplesOf :: TriplesGraph -> Triples
_triplesOf = triplesOf

----------------------------------------------------
--  generic tests parameterized for TriplesGraph  --
----------------------------------------------------

prop_tg_empty :: Bool
prop_tg_empty = p_empty _triplesOf _empty

prop_tg_mkGraph_triplesOf :: Triples -> Maybe BaseUrl -> PrefixMappings -> Bool
prop_tg_mkGraph_triplesOf = p_mkGraph_triplesOf _triplesOf _mkGraph

prop_tg_mkGraph_no_dupes :: Triples -> Maybe BaseUrl -> PrefixMappings -> Bool
prop_tg_mkGraph_no_dupes = p_mkGraph_no_dupes _triplesOf _mkGraph

prop_tg_query_match_none :: Triples -> Maybe BaseUrl -> PrefixMappings -> Bool
prop_tg_query_match_none = p_query_match_none _mkGraph

prop_tg_query_matched_spo :: TriplesGraph -> Property
prop_tg_query_matched_spo = p_query_matched_spo _triplesOf

prop_tg_query_unmatched_spo :: TriplesGraph -> Triple -> Property
prop_tg_query_unmatched_spo = p_query_unmatched_spo _triplesOf

prop_tg_query_match_s :: TriplesGraph -> Property
prop_tg_query_match_s = p_query_match_s _triplesOf

prop_tg_query_match_p :: TriplesGraph -> Property
prop_tg_query_match_p = p_query_match_p _triplesOf

prop_tg_query_match_o :: TriplesGraph -> Property
prop_tg_query_match_o = p_query_match_o _triplesOf

prop_tg_query_match_sp :: TriplesGraph -> Property
prop_tg_query_match_sp = p_query_match_sp _triplesOf

prop_tg_query_match_so :: TriplesGraph -> Property
prop_tg_query_match_so = p_query_match_so _triplesOf

prop_tg_query_match_po :: TriplesGraph -> Property
prop_tg_query_match_po = p_query_match_po _triplesOf

prop_tg_select_match_none :: TriplesGraph -> Bool
prop_tg_select_match_none = p_select_match_none

prop_tg_select_match_s :: TriplesGraph -> Property
prop_tg_select_match_s = p_select_match_s _triplesOf

prop_tg_select_match_p :: TriplesGraph -> Property
prop_tg_select_match_p = p_select_match_p _triplesOf

prop_tg_select_match_o :: TriplesGraph -> Property
prop_tg_select_match_o = p_select_match_o _triplesOf

prop_tg_select_match_sp :: TriplesGraph -> Property
prop_tg_select_match_sp = p_select_match_sp _triplesOf

prop_tg_select_match_so :: TriplesGraph -> Property
prop_tg_select_match_so = p_select_match_so _triplesOf

prop_tg_select_match_po :: TriplesGraph -> Property
prop_tg_select_match_po = p_select_match_po _triplesOf

prop_tg_select_match_spo :: TriplesGraph -> Property
prop_tg_select_match_spo = p_select_match_spo _triplesOf
