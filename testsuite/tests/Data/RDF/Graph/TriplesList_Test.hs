module Data.RDF.Graph.TriplesList_Test (empty',triplesOf',uniqTriplesOf',mkRdf') where

import Control.Monad
import qualified Data.Map as Map
import Data.RDF.GraphTestUtils
import Data.RDF.Namespace
import Data.RDF.Graph.TriplesList
import Data.RDF.Types
import Test.QuickCheck.Arbitrary

instance Arbitrary TriplesList where
  arbitrary = liftM3 mkRdf arbitraryTs (return Nothing) (return $ PrefixMappings Map.empty)
  --coarbitrary = undefined

empty' :: TriplesList
empty' = empty

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> TriplesList
mkRdf' = mkRdf

triplesOf' :: TriplesList -> Triples
triplesOf' = triplesOf

uniqTriplesOf' :: TriplesList -> Triples
uniqTriplesOf' = uniqTriplesOf
