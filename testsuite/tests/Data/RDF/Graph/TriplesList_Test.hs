{-# LANGUAGE FlexibleInstances #-}

module Data.RDF.Graph.TriplesList_Test (empty',triplesOf',uniqTriplesOf',mkRdf') where

import Control.Monad
import qualified Data.Map as Map
import Data.RDF.GraphTestUtils
import Data.RDF.Namespace
import Data.RDF.Graph.TriplesList
import Data.RDF.Types
import Test.QuickCheck.Arbitrary

instance Arbitrary TriplesList

instance Arbitrary (RDF TriplesList) where
  arbitrary = liftM3 mkRdf arbitraryTs (return Nothing) (return $ PrefixMappings Map.empty)
  --coarbitrary = undefined

empty' :: RDF TriplesList
empty' = empty

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF TriplesList
mkRdf' = mkRdf

triplesOf' :: RDF TriplesList -> Triples
triplesOf' = triplesOf

uniqTriplesOf' :: RDF TriplesList -> Triples
uniqTriplesOf' = uniqTriplesOf
