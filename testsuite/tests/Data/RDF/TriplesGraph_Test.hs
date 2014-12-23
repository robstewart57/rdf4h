module Data.RDF.TriplesGraph_Test (empty',triplesOf',uniqTriplesOf',mkRdf') where

import Control.Monad
import qualified Data.Map as Map
import Data.RDF.GraphTestUtils
import Data.RDF.Namespace
import Data.RDF.TriplesGraph
import Data.RDF.Types
import Test.QuickCheck.Arbitrary

instance Arbitrary TriplesGraph where
  arbitrary = liftM3 mkRdf arbitraryTs (return Nothing) (return $ PrefixMappings Map.empty)
  --coarbitrary = undefined

empty' :: TriplesGraph
empty' = empty

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> TriplesGraph
mkRdf' = mkRdf

triplesOf' :: TriplesGraph -> Triples
triplesOf' = triplesOf

uniqTriplesOf' :: TriplesGraph -> Triples
uniqTriplesOf' = uniqTriplesOf
