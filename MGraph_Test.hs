import RDF
import Namespace

--import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad
import GraphTestUtils
import Test.QuickCheck

import MGraph

----------------------------------------------------
--   instances and graph functions for AvlGraph   --
----------------------------------------------------

instance Arbitrary MGraph where
  arbitrary = liftM3 mkGraph arbitraryTs (return Nothing) (return $ PrefixMappings Map.empty)
  coarbitrary = undefined

instance Show MGraph where
  show gr = concatMap (\t -> show t ++ "\n")  (triplesOf gr)

_empty :: MGraph
_empty = empty

_mkGraph :: Triples -> Maybe BaseUrl -> PrefixMappings -> MGraph
_mkGraph = mkGraph

_triplesOf :: MGraph -> Triples
_triplesOf = triplesOf

----------------------------------------------------
--    generic tests parameterized for MGraph    --
----------------------------------------------------

prop_empty :: Bool
prop_empty = p_empty _triplesOf _empty

prop_mkGraph_triplesOf :: Triples -> Maybe BaseUrl -> PrefixMappings -> Bool
prop_mkGraph_triplesOf = p_mkGraph_triplesOf _triplesOf _mkGraph

prop_mkGraph_no_dupes :: Triples -> Maybe BaseUrl -> PrefixMappings -> Bool
prop_mkGraph_no_dupes = p_mkGraph_no_dupes _triplesOf _mkGraph

prop_mg_query_match_none :: Triples -> Maybe BaseUrl -> PrefixMappings -> Bool
prop_mg_query_match_none = p_query_match_none _mkGraph

prop_mg_query_matched_spo :: MGraph -> Property
prop_mg_query_matched_spo = p_query_matched_spo _triplesOf

prop_mg_query_unmatched_spo :: MGraph -> Triple -> Property
prop_mg_query_unmatched_spo = p_query_unmatched_spo _triplesOf

prop_mg_query_match_s :: MGraph -> Property
prop_mg_query_match_s = p_query_match_s _triplesOf

prop_mg_query_match_p :: MGraph -> Property
prop_mg_query_match_p = p_query_match_p _triplesOf

prop_mg_query_match_o :: MGraph -> Property
prop_mg_query_match_o = p_query_match_o _triplesOf

prop_mg_query_match_sp :: MGraph -> Property
prop_mg_query_match_sp = p_query_match_sp _triplesOf

prop_mg_query_match_so :: MGraph -> Property
prop_mg_query_match_so = p_query_match_so _triplesOf

prop_mg_query_match_po :: MGraph -> Property
prop_mg_query_match_po = p_query_match_po _triplesOf

prop_mg_select_match_none :: MGraph -> Bool
prop_mg_select_match_none = p_select_match_none

prop_mg_select_match_s :: MGraph -> Property
prop_mg_select_match_s = p_select_match_s _triplesOf

prop_mg_select_match_p :: MGraph -> Property
prop_mg_select_match_p = p_select_match_p _triplesOf

prop_mg_select_match_o :: MGraph -> Property
prop_mg_select_match_o = p_select_match_o _triplesOf

prop_mg_select_match_sp :: MGraph -> Property
prop_mg_select_match_sp = p_select_match_sp _triplesOf

prop_mg_select_match_so :: MGraph -> Property
prop_mg_select_match_so = p_select_match_so _triplesOf

prop_mg_select_match_po :: MGraph -> Property
prop_mg_select_match_po = p_select_match_po _triplesOf

prop_mg_select_match_spo :: MGraph -> Property
prop_mg_select_match_spo = p_select_match_spo _triplesOf
