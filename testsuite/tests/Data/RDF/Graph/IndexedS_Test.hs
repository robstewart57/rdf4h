module Data.RDF.Graph.IndexedS_Test (triplesOf',uniqTriplesOf',empty',mkRdf') where

import Data.RDF.Types
import Data.RDF.Graph.IndexedS (IndexedS)
import Data.RDF.GraphTestUtils
import qualified Data.Map as Map
import Control.Monad

import Test.QuickCheck

instance Arbitrary IndexedS where
  arbitrary = liftM3 mkRdf arbitraryTs (return Nothing) (return $ PrefixMappings Map.empty)
  --coarbitrary = undefined

empty' :: IndexedS
empty' = empty

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> IndexedS
mkRdf' = mkRdf

triplesOf' :: IndexedS -> Triples
triplesOf' = triplesOf

uniqTriplesOf' :: IndexedS -> Triples
uniqTriplesOf' = uniqTriplesOf
