{-# LANGUAGE FlexibleInstances #-}

module Data.RDF.Graph.HashSP_Test (triplesOf',uniqTriplesOf',empty',mkRdf') where

import Data.RDF.Types
import Data.RDF.Graph.HashMapSP (HashSP)
import Data.RDF.GraphTestUtils
import qualified Data.Map as Map
import Control.Monad

import Test.QuickCheck

instance Arbitrary (RDF HashSP) where
  arbitrary = liftM3 mkRdf arbitraryTs (return Nothing) (return $ PrefixMappings Map.empty)

empty' :: RDF HashSP
empty' = empty

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF HashSP
mkRdf' = mkRdf

triplesOf' :: RDF HashSP -> Triples
triplesOf' = triplesOf

uniqTriplesOf' :: RDF HashSP -> Triples
uniqTriplesOf' = uniqTriplesOf
