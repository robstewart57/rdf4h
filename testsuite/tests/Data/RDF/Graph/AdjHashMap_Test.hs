{-# LANGUAGE FlexibleInstances #-}

module Data.RDF.Graph.AdjHashMap_Test (triplesOf',uniqTriplesOf',empty',mkRdf',addTriple',removeTriple') where

import Data.RDF.Types
import Data.RDF.Graph.AdjHashMap (AdjHashMap)
import Data.RDF.GraphTestUtils
import qualified Data.Map as Map
import Control.Monad

import Test.QuickCheck

instance Arbitrary (RDF AdjHashMap) where
  arbitrary = liftM3 mkRdf arbitraryTs (return Nothing) (return $ PrefixMappings Map.empty)
  --coarbitrary = undefined

empty' :: RDF AdjHashMap
empty' = empty

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF AdjHashMap
mkRdf' = mkRdf

triplesOf' :: RDF AdjHashMap -> Triples
triplesOf' = triplesOf

uniqTriplesOf' :: RDF AdjHashMap -> Triples
uniqTriplesOf' = uniqTriplesOf

addTriple' :: RDF AdjHashMap -> Triple -> RDF AdjHashMap
addTriple' = addTriple

removeTriple' :: RDF AdjHashMap -> Triple -> RDF AdjHashMap
removeTriple' = removeTriple
