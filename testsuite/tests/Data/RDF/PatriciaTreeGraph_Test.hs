module Data.RDF.PatriciaTreeGraph_Test (triplesOf',uniqTriplesOf',empty',mkRdf') where

import Data.RDF.Types
import Data.RDF.PatriciaTreeGraph (PatriciaTreeGraph)
import Data.RDF.GraphTestUtils
import qualified Data.Map as Map
import Control.Monad

import Test.QuickCheck

instance Arbitrary PatriciaTreeGraph where
  arbitrary = liftM3 mkRdf arbitraryTs (return Nothing) (return $ PrefixMappings Map.empty)
  --coarbitrary = undefined

empty' :: PatriciaTreeGraph
empty' = empty

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> PatriciaTreeGraph
mkRdf' = mkRdf

triplesOf' :: PatriciaTreeGraph -> Triples
triplesOf' = triplesOf

uniqTriplesOf' :: PatriciaTreeGraph -> Triples
uniqTriplesOf' = uniqTriplesOf
