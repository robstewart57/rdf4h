module Data.RDF.TriplesGraph_Test where

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Data.RDF
import Data.RDF.TriplesGraph
import Data.RDF.GraphTestUtils
import qualified Data.Map as Map
import Control.Monad

tests = [ testGroup "TriplesGraph"
            [ testProperty "empty"              prop_tg_empty
            , testProperty "mkRdf_triplesOf"    prop_tg_mkRdf_triplesOf
            , testProperty "query_match_none"   prop_tg_query_match_none
            , testProperty "query_matched_spo"  prop_tg_query_matched_spo
            , testProperty "query_matched_spo_no_dupes" prop_tg_query_matched_spo_no_dupes
            , testProperty "query_unmatched_spo" prop_tg_query_unmatched_spo
            , testProperty "query_match_s"      prop_tg_query_match_s
            , testProperty "query_match_p"      prop_tg_query_match_p
            , testProperty "query_match_o"      prop_tg_query_match_o
            , testProperty "query_match_sp"     prop_tg_query_match_sp
            , testProperty "query_match_so"     prop_tg_query_match_so
            , testProperty "query_match_po"     prop_tg_query_match_po
            , testProperty "select_match_none"  prop_tg_select_match_none
            , testProperty "select_match_s"     prop_tg_select_match_s
            , testProperty "select_match_p"     prop_tg_select_match_p
            , testProperty "select_match_o"     prop_tg_select_match_o
            , testProperty "select_match_sp"    prop_tg_select_match_sp
            , testProperty "select_match_so"    prop_tg_select_match_so
            , testProperty "select_match_po"    prop_tg_select_match_po
            , testProperty "select_match_spo"   prop_tg_select_match_spo
            ]
        ]

----------------------------------------------------
-- instances and graph functions for TriplesGraph --
----------------------------------------------------

instance Arbitrary TriplesGraph where
  arbitrary = liftM3 mkRdf arbitraryTs (return Nothing) (return $ PrefixMappings Map.empty)
  --coarbitrary = undefined

_empty :: TriplesGraph
_empty = empty

_mkRdf :: Triples -> Maybe BaseUrl -> PrefixMappings -> TriplesGraph
_mkRdf = mkRdf

_triplesOf :: TriplesGraph -> Triples
_triplesOf = triplesOf

----------------------------------------------------
--  generic tests parameterized for TriplesGraph  --
----------------------------------------------------

prop_tg_empty :: Bool
prop_tg_empty = p_empty _triplesOf _empty

prop_tg_mkRdf_triplesOf :: Triples -> Maybe BaseUrl -> PrefixMappings -> Bool
prop_tg_mkRdf_triplesOf = p_mkRdf_triplesOf _triplesOf _mkRdf

prop_tg_query_match_none :: Triples -> Maybe BaseUrl -> PrefixMappings -> Bool
prop_tg_query_match_none = p_query_match_none _mkRdf

prop_tg_query_matched_spo :: TriplesGraph -> Property
prop_tg_query_matched_spo = p_query_matched_spo _triplesOf

prop_tg_query_matched_spo_no_dupes :: TriplesGraph -> Property
prop_tg_query_matched_spo_no_dupes = p_query_matched_spo_no_dupes _triplesOf _mkRdf

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
