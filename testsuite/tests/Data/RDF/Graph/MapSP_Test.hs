{-# LANGUAGE FlexibleInstances #-}

module Data.RDF.Graph.MapSP_Test (triplesOf',uniqTriplesOf',empty',mkRdf') where

import Data.RDF.Types
import Data.RDF.Graph.MapSP (MapSP)
import Data.RDF.GraphTestUtils
import qualified Data.Map as Map
import Control.Monad

import Test.QuickCheck

instance Arbitrary MapSP

instance Arbitrary (RDF MapSP) where
  arbitrary = liftM3 mkRdf arbitraryTs (return Nothing) (return $ PrefixMappings Map.empty)
  --coarbitrary = undefined

empty' :: RDF MapSP
empty' = empty

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF MapSP
mkRdf' = mkRdf

triplesOf' :: RDF MapSP -> Triples
triplesOf' = triplesOf

uniqTriplesOf' :: RDF MapSP -> Triples
uniqTriplesOf' = uniqTriplesOf
