{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving , DeriveGeneric #-}

-- |"TriplesGraph" contains a list-backed graph implementation suitable
-- for smallish graphs or for temporary graphs that will not be queried.
-- It maintains the triples in the order that they are given in, and is
-- especially useful for holding N-Triples, where it is often desirable
-- to preserve the order of the triples when they were originally parsed.
-- Duplicate triples are not filtered. If you might have duplicate triples,
-- use @MGraph@ instead, which is also more efficient. However, the query
-- functions of this graph (select, query) remove duplicates from their
-- result triples (but triplesOf does not) since it is usually cheap
-- to do so.
module Data.RDF.Graph.TriplesList (TriplesList) where

import Prelude hiding (pred)
import Control.DeepSeq (NFData)
import Data.Binary
import qualified Data.Map as Map
import Data.RDF.Namespace
import Data.RDF.Query
import Data.RDF.Types
import Data.List (nub)
import GHC.Generics

-- |A simple implementation of the 'RDF' type class that represents
-- the graph internally as a list of triples.
--
-- Note that this type of RDF is fine for interactive
-- experimentation and querying of smallish (<10,000 triples) graphs,
-- but there are better options for larger graphs or graphs that you
-- will do many queries against (e.g., @MGraph@ is faster for queries).
--
-- The time complexity of the functions (where n == num_triples) are:
--
--  * 'empty'    : O(1)
--
--  * 'mkRdf'  : O(n)
--
--  * 'triplesOf': O(1)
--
--  * 'select'   : O(n)
--
--  * 'query'    : O(n)
newtype TriplesList = TriplesList (Triples, Maybe BaseUrl, PrefixMappings)
                       deriving (Generic,NFData)

instance Binary TriplesList

instance Rdf TriplesList where
  data RDF TriplesList = RDF TriplesList
  baseUrl           = baseUrl'
  prefixMappings    = prefixMappings'
  addPrefixMappings = addPrefixMappings'
  empty             = empty'
  mkRdf             = mkRdf'
  triplesOf         = triplesOf'
  uniqTriplesOf     = uniqTriplesOf'
  select            = select'
  query             = query'
  showGraph         = showGraph'

instance Show TriplesList where
  show (TriplesList (ts, _, _)) = concatMap (\t -> show t ++ "\n") ts

showGraph' :: RDF TriplesList -> [Char]
showGraph' (RDF (TriplesList (ts, _, _))) = concatMap (\t -> show t ++ "\n") ts

prefixMappings' :: RDF TriplesList -> PrefixMappings
prefixMappings' (RDF (TriplesList (_, _, pms))) = pms

addPrefixMappings' :: RDF TriplesList -> PrefixMappings -> Bool -> RDF TriplesList
addPrefixMappings' (RDF (TriplesList (ts, baseURL, pms))) pms' replace =
  let merge = if replace then flip mergePrefixMappings else mergePrefixMappings
  in  RDF (TriplesList (ts, baseURL, merge pms pms'))
  
baseUrl' :: RDF TriplesList -> Maybe BaseUrl
baseUrl' (RDF (TriplesList (_, baseURL, _))) = baseURL

empty' :: RDF TriplesList
empty' = RDF (TriplesList ([], Nothing, PrefixMappings Map.empty))

-- We no longer remove duplicates here, as it is very time consuming and is often not
-- necessary (raptor does not seem to remove dupes either). Instead, we remove dupes
-- from the results of the select' and query' functions, since it is cheap to do
-- there in most cases, but not when triplesOf' is called.
mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF TriplesList
mkRdf' ts baseURL pms = RDF (TriplesList (ts, baseURL, pms))

triplesOf' :: RDF TriplesList -> Triples
triplesOf' (RDF (TriplesList (ts, _, _))) = ts

uniqTriplesOf' :: RDF TriplesList -> Triples
uniqTriplesOf' = nub . expandTriples

select' :: RDF TriplesList -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' g s p o = filter (matchSelect s p o) $ triplesOf g

query' :: RDF TriplesList -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
query' g s p o = filter (matchPattern s p o) $ triplesOf g

matchSelect :: NodeSelector -> NodeSelector -> NodeSelector -> Triple -> Bool
matchSelect s p o t =
  match s (subjectOf t) && match p (predicateOf t) && match o (objectOf t)
  where
    match Nothing   _ = True
    match (Just fn) n = fn n

matchPattern :: Maybe Subject -> Maybe Predicate -> Maybe Object -> Triple -> Bool
matchPattern subj pred obj t = smatch t && pmatch t && omatch t
  where
    smatch trp = matchNode subj (subjectOf trp)
    pmatch trp = matchNode pred (predicateOf trp)
    omatch trp = matchNode obj (objectOf trp)

matchNode :: Maybe Node -> Node -> Bool
matchNode Nothing   _  = True
matchNode (Just n1) n2 = n1 == n2
