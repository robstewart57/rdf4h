{-# LANGUAGE FlexibleInstances #-}

module Data.RDF.Graph.HashMapSP_Test (triplesOf',uniqTriplesOf',empty',mkRdf') where

import Data.RDF.Types
import Data.RDF.Graph.HashMapSP (HashMapSP)
import Data.RDF.GraphTestUtils
import qualified Data.Map as Map
import Control.Monad

import Test.QuickCheck

instance Arbitrary HashMapSP

instance Arbitrary (RDF HashMapSP) where
  arbitrary = liftM3 mkRdf arbitraryTs (return Nothing) (return $ PrefixMappings Map.empty)

empty' :: RDF HashMapSP
empty' = empty

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF HashMapSP
mkRdf' = mkRdf

triplesOf' :: RDF HashMapSP -> Triples
triplesOf' = triplesOf

uniqTriplesOf' :: RDF HashMapSP -> Triples
uniqTriplesOf' = uniqTriplesOf
