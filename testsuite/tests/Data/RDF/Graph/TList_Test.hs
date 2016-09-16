{-# LANGUAGE FlexibleInstances #-}

module Data.RDF.Graph.TList_Test (empty',triplesOf',uniqTriplesOf',mkRdf') where

import Control.Monad
import qualified Data.Map as Map
import Data.RDF.GraphTestUtils
import Data.RDF.Namespace
import Data.RDF.Graph.TList
import Data.RDF.Types
import Test.QuickCheck.Arbitrary

instance Arbitrary (RDF TList) where
  arbitrary = liftM3 mkRdf arbitraryTs (return Nothing) (return $ PrefixMappings Map.empty)
  --coarbitrary = undefined

empty' :: RDF TList
empty' = empty

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF TList
mkRdf' = mkRdf

triplesOf' :: RDF TList -> Triples
triplesOf' = triplesOf

uniqTriplesOf' :: RDF TList -> Triples
uniqTriplesOf' = uniqTriplesOf
