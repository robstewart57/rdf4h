{-# LANGUAGE FlexibleInstances #-}

module Data.RDF.Graph.SP_Test (triplesOf',uniqTriplesOf',empty',mkRdf') where

import Data.RDF.Types
import Data.RDF.Graph.MapSP (SP)
import Data.RDF.GraphTestUtils
import qualified Data.Map as Map
import Control.Monad

import Test.QuickCheck

instance Arbitrary SP

instance Arbitrary (RDF SP) where
  arbitrary = liftM3 mkRdf arbitraryTs (return Nothing) (return $ PrefixMappings Map.empty)
  --coarbitrary = undefined

empty' :: RDF SP
empty' = empty

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF SP
mkRdf' = mkRdf

triplesOf' :: RDF SP -> Triples
triplesOf' = triplesOf

uniqTriplesOf' :: RDF SP -> Triples
uniqTriplesOf' = uniqTriplesOf
