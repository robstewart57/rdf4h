module Data.RDF.Graph.MapSP_Test (triplesOf',uniqTriplesOf',empty',mkRdf') where

import Data.RDF.Types
import Data.RDF.Graph.MapSP (MapSP)
import Data.RDF.GraphTestUtils
import qualified Data.Map as Map
import Control.Monad

import Test.QuickCheck

instance Arbitrary MapSP where
  arbitrary = liftM3 mkRdf arbitraryTs (return Nothing) (return $ PrefixMappings Map.empty)
  --coarbitrary = undefined

empty' :: MapSP
empty' = empty

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> MapSP
mkRdf' = mkRdf

triplesOf' :: MapSP -> Triples
triplesOf' = triplesOf

uniqTriplesOf' :: MapSP -> Triples
uniqTriplesOf' = uniqTriplesOf
