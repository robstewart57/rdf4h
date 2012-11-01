module Data.RDF.MGraph_Test where

import Data.RDF.Types
import Data.RDF.MGraph
import Data.RDF.GraphTestUtils
import qualified Data.Map as Map
import Control.Monad

-- Testing imports
import Test.QuickCheck
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

tests = [ testGroup "MGraph"
            [ testProperty "empty"                      (p_empty _triplesOf _empty)
            , testProperty "mkRdf_triplesOf"            (p_mkRdf_triplesOf _triplesOf _mkRdf)
            , testProperty "mkRdf_no_dupes"             (p_mkRdf_no_dupes _triplesOf _mkRdf)
            , testProperty "query_match_none"           (p_query_match_none _mkRdf)
            , testProperty "query_matched_spo"          (p_query_matched_spo _triplesOf)
            , testProperty "query_matched_spo_no_dupes" (p_query_matched_spo_no_dupes _triplesOf _mkRdf)
            , testProperty "query_unmatched_spo"        (p_query_unmatched_spo _triplesOf)
            , testProperty "query_match_s"              (p_query_match_s _triplesOf)
            , testProperty "query_match_p"              (p_query_match_p _triplesOf)
            , testProperty "query_match_o"              (p_query_match_o _triplesOf)
            , testProperty "query_match_sp"             (p_query_match_sp _triplesOf)
            , testProperty "query_match_so"             (p_query_match_so _triplesOf)
            , testProperty "query_match_po"             (p_query_match_po _triplesOf)
            , testProperty "match_none"                 (p_select_match_none :: MGraph -> Bool)
            , testProperty "select_match_s"             (p_select_match_s _triplesOf)
            , testProperty "select_match_p"             (p_select_match_p _triplesOf)
            , testProperty "select_match_o"             (p_select_match_o _triplesOf)
            , testProperty "select_match_sp"            (p_select_match_sp _triplesOf)
            , testProperty "select_match_so"            (p_select_match_so _triplesOf)
            , testProperty "select_match_po"            (p_select_match_po _triplesOf)
            , testProperty "select_match_spo"           (p_select_match_spo _triplesOf)
            ]
        ]

----------------------------------------------------
--   instances and graph functions for MGraph   --
----------------------------------------------------

instance Arbitrary MGraph where
  arbitrary = liftM3 mkRdf arbitraryTs (return Nothing) (return $ PrefixMappings Map.empty)
  --coarbitrary = undefined

instance Show MGraph where
  show gr = concatMap (\t -> show t ++ "\n")  (triplesOf gr)

_empty :: MGraph
_empty = empty

_mkRdf :: Triples -> Maybe BaseUrl -> PrefixMappings -> MGraph
_mkRdf = mkRdf

_triplesOf :: MGraph -> Triples
_triplesOf = triplesOf

----------------------------------------------------
--    generic tests parameterized for MGraph    --
----------------------------------------------------

prop_mg_empty :: Bool
prop_mg_empty = p_empty _triplesOf _empty

prop_mg_mkRdf_triplesOf :: Triples -> Maybe BaseUrl -> PrefixMappings -> Bool
prop_mg_mkRdf_triplesOf = p_mkRdf_triplesOf _triplesOf _mkRdf

prop_mg_mkRdf_no_dupes :: Triples -> Maybe BaseUrl -> PrefixMappings -> Bool
prop_mg_mkRdf_no_dupes = p_mkRdf_no_dupes _triplesOf _mkRdf

prop_mg_query_match_none :: Triples -> Maybe BaseUrl -> PrefixMappings -> Bool
prop_mg_query_match_none = p_query_match_none _mkRdf

prop_mg_query_matched_spo :: MGraph -> Property
prop_mg_query_matched_spo = p_query_matched_spo _triplesOf

prop_mg_query_matched_spo_no_dupes :: MGraph -> Property
prop_mg_query_matched_spo_no_dupes = p_query_matched_spo_no_dupes _triplesOf _mkRdf

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
