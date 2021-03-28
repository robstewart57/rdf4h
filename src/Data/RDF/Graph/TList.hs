{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | "TriplesGraph" contains a list-backed graph implementation suitable
--  for smallish graphs or for temporary graphs that will not be queried.
--  It maintains the triples in the order that they are given in, and is
--  especially useful for holding N-Triples, where it is often desirable
--  to preserve the order of the triples when they were originally parsed.
--  Duplicate triples are not filtered. If you might have duplicate triples,
--  use @MGraph@ instead, which is also more efficient. However, the query
--  functions of this graph (select, query) remove duplicates from their
--  result triples (but triplesOf does not) since it is usually cheap
--  to do so.
module Data.RDF.Graph.TList (TList) where

#if MIN_VERSION_base(4,9,0)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#else
#endif
#else
#endif
import Control.DeepSeq (NFData)
import Data.Binary
import Data.List (nub)
import Data.Maybe
import Data.RDF.Namespace
import Data.RDF.Query
import Data.RDF.Types
import GHC.Generics
import Prelude

-- | A simple implementation of the 'RDF' type class that represents
--  the graph internally as a list of triples.
--
--  Note that this type of RDF is fine for interactive
--  experimentation and querying of smallish (<10,000 triples) graphs,
--  but there are better options for larger graphs or graphs that you
--  will do many queries against (e.g., @MGraph@ is faster for queries).
--
--  The time complexity of the functions (where n == num_triples) are:
--
--   * 'empty'    : O(1)
--
--   * 'mkRdf'  : O(n)
--
--   * 'triplesOf': O(1)
--
--   * 'select'   : O(n)
--
--   * 'query'    : O(n)
data TList deriving (Generic)

instance Binary TList

instance NFData TList

data instance RDF TList = TListC (Triples, Maybe BaseUrl, PrefixMappings, Int)
  deriving (Generic, NFData)

instance Rdf TList where
  baseUrl = baseUrl'
  prefixMappings = prefixMappings'
  addPrefixMappings = addPrefixMappings'
  empty = empty'
  mkRdf = mkRdf'
  addTriple = addTriple'
  removeTriple = removeTriple'
  bnodeGen = bnodeGen'
  triplesOf = triplesOf'
  uniqTriplesOf = uniqTriplesOf'
  select = select'
  query = query'
  showGraph = showGraph'

showGraph' :: RDF TList -> String
showGraph' gr = concatMap (\t -> show t <> "\n") (expandTriples gr)

prefixMappings' :: RDF TList -> PrefixMappings
prefixMappings' (TListC (_, _, pms, _)) = pms

addPrefixMappings' :: RDF TList -> PrefixMappings -> Bool -> RDF TList
addPrefixMappings' (TListC (ts, baseURL, pms, nextId)) pms' replace =
  let merge = if replace then flip (<>) else (<>)
   in TListC (ts, baseURL, merge pms pms', nextId)

baseUrl' :: RDF TList -> Maybe BaseUrl
baseUrl' (TListC (_, baseURL, _, _)) = baseURL

empty' :: RDF TList
empty' = TListC (mempty, Nothing, PrefixMappings mempty, 0)

-- We no longer remove duplicates here, as it is very time consuming and is often not
-- necessary (raptor does not seem to remove dupes either). Instead, we remove dupes
-- from the results of the select' and query' functions, since it is cheap to do
-- there in most cases, but not when triplesOf' is called.
mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> Maybe Int -> RDF TList
mkRdf' ts baseURL pms i = TListC (ts, baseURL, pms, fromMaybe (bnodeGenCount ts) i)

addTriple' :: RDF TList -> Triple -> RDF TList
addTriple' (TListC (ts, bURL, preMapping, nextId)) t = TListC (t : ts, bURL, preMapping, nextId)

removeTriple' :: RDF TList -> Triple -> RDF TList
removeTriple' (TListC (ts, bURL, preMapping, nextId)) t = TListC (newTs, bURL, preMapping, nextId)
  where
    newTs = filter (/= t) ts

bnodeGen' :: RDF TList -> (Node, RDF TList)
bnodeGen' (TListC (ts, bUrl, pms, nextId)) =
  (BNodeGen nextId, TListC (ts, bUrl, pms, nextId + 1))

triplesOf' :: RDF TList -> Triples
triplesOf' ((TListC (ts, _, _, _))) = ts

uniqTriplesOf' :: RDF TList -> Triples
uniqTriplesOf' = nub . expandTriples

select' :: RDF TList -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' g s p o = nub $ filter (matchSelect s p o) $ triplesOf g

query' :: RDF TList -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
query' g s p o = nub $ filter (matchPattern s p o) $ triplesOf g

matchSelect :: NodeSelector -> NodeSelector -> NodeSelector -> Triple -> Bool
matchSelect s p o (Triple s' p' o') = match s s' && match p p' && match o o'
  where
    match fn n = maybe True ($ n) fn

matchPattern :: Maybe Subject -> Maybe Predicate -> Maybe Object -> Triple -> Bool
matchPattern s p o (Triple s' p' o') = match s s' && match p p' && match o o'
  where
    match n1 n2 = maybe True (== n2) n1
