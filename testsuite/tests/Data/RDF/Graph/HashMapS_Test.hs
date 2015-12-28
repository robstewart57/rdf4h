module Data.RDF.Graph.HashMapS_Test (triplesOf',uniqTriplesOf',empty',mkRdf') where

import Data.RDF.Types
import Data.RDF.Graph.HashMapS (HashMapS)
import Data.RDF.GraphTestUtils
import qualified Data.Map as Map
import Control.Monad

import Test.QuickCheck

instance Arbitrary HashMapS where
  arbitrary = liftM3 mkRdf arbitraryTs (return Nothing) (return $ PrefixMappings Map.empty)
  --coarbitrary = undefined

empty' :: HashMapS
empty' = empty

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> HashMapS
mkRdf' = mkRdf

triplesOf' :: HashMapS -> Triples
triplesOf' = triplesOf

uniqTriplesOf' :: HashMapS -> Triples
uniqTriplesOf' = uniqTriplesOf
