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
module Data.RDF.Graph.TList (TList) where

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
newtype TList = TList (Triples, Maybe BaseUrl, PrefixMappings)
                       deriving (Generic,NFData)

instance Binary TList

instance Rdf TList where
  data RDF TList = RDF TList
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

instance Show TList where
  show (TList (ts, _, _)) = concatMap (\t -> show t ++ "\n") ts

showGraph' :: RDF TList -> [Char]
showGraph' (RDF (TList (ts, _, _))) = concatMap (\t -> show t ++ "\n") ts

prefixMappings' :: RDF TList -> PrefixMappings
prefixMappings' (RDF (TList (_, _, pms))) = pms

addPrefixMappings' :: RDF TList -> PrefixMappings -> Bool -> RDF TList
addPrefixMappings' (RDF (TList (ts, baseURL, pms))) pms' replace =
  let merge = if replace then flip mergePrefixMappings else mergePrefixMappings
  in  RDF (TList (ts, baseURL, merge pms pms'))
  
baseUrl' :: RDF TList -> Maybe BaseUrl
baseUrl' (RDF (TList (_, baseURL, _))) = baseURL

empty' :: RDF TList
empty' = RDF (TList ([], Nothing, PrefixMappings Map.empty))

-- We no longer remove duplicates here, as it is very time consuming and is often not
-- necessary (raptor does not seem to remove dupes either). Instead, we remove dupes
-- from the results of the select' and query' functions, since it is cheap to do
-- there in most cases, but not when triplesOf' is called.
mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF TList
mkRdf' ts baseURL pms = RDF (TList (ts, baseURL, pms))

triplesOf' :: RDF TList -> Triples
triplesOf' (RDF (TList (ts, _, _))) = ts

uniqTriplesOf' :: RDF TList -> Triples
uniqTriplesOf' = nub . expandTriples

select' :: RDF TList -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' g s p o = filter (matchSelect s p o) $ triplesOf g

query' :: RDF TList -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
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
