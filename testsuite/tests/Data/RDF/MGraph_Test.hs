module Data.RDF.MGraph_Test (triplesOf',uniqTriplesOf',empty',mkRdf') where

import Data.RDF.Types
import Data.RDF.MGraph (MGraph)
import Data.RDF.GraphTestUtils
import qualified Data.Map as Map
import Control.Monad

import Test.QuickCheck

instance Arbitrary MGraph where
  arbitrary = liftM3 mkRdf arbitraryTs (return Nothing) (return $ PrefixMappings Map.empty)
  --coarbitrary = undefined

empty' :: MGraph
empty' = empty

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> MGraph
mkRdf' = mkRdf

triplesOf' :: MGraph -> Triples
triplesOf' = triplesOf

uniqTriplesOf' :: MGraph -> Triples
uniqTriplesOf' = uniqTriplesOf
