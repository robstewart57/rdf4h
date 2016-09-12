{-# LANGUAGE FlexibleInstances #-}

module Data.RDF.Graph.HashMapS_Test (triplesOf',uniqTriplesOf',empty',mkRdf') where

import Data.RDF.Types
import Data.RDF.Graph.HashMapS (HashMapS)
import Data.RDF.GraphTestUtils
import qualified Data.Map as Map
import Control.Monad

import Test.QuickCheck

instance Arbitrary HashMapS

instance Arbitrary (RDF HashMapS) where
  arbitrary = liftM3 mkRdf arbitraryTs (return Nothing) (return $ PrefixMappings Map.empty)
  --coarbitrary = undefined

empty' :: RDF HashMapS
empty' = empty

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF HashMapS
mkRdf' = mkRdf

triplesOf' :: RDF HashMapS -> Triples
triplesOf' = triplesOf

uniqTriplesOf' :: RDF HashMapS -> Triples
uniqTriplesOf' = uniqTriplesOf
