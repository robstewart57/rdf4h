{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}


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

import Prelude
#if MIN_VERSION_base(4,9,0)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#else
#endif
#else
#endif
import Control.DeepSeq (NFData)
import Data.Binary
import Data.RDF.Namespace
import Data.RDF.Query
import Data.RDF.Types (RDF,Rdf(..),Triple(..),Subject,Predicate,Object,NodeSelector,Triples,BaseUrl)
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

data TList deriving (Generic)

instance Binary TList
instance NFData TList

data instance RDF TList = TListC (Triples, Maybe BaseUrl, PrefixMappings)
                       deriving (Generic,NFData)

instance Rdf TList where
  baseUrl           = baseUrl'
  prefixMappings    = prefixMappings'
  addPrefixMappings = addPrefixMappings'
  empty             = empty'
  mkRdf             = mkRdf'
  addTriple         = addTriple'
  removeTriple      = removeTriple'
  triplesOf         = triplesOf'
  uniqTriplesOf     = uniqTriplesOf'
  select            = select'
  query             = query'
  showGraph         = showGraph'

showGraph' :: RDF TList -> String
showGraph' gr = concatMap (\t -> show t <> "\n") (expandTriples gr)

prefixMappings' :: RDF TList -> PrefixMappings
prefixMappings' (TListC(_, _, pms)) = pms

addPrefixMappings' :: RDF TList -> PrefixMappings -> Bool -> RDF TList
addPrefixMappings' (TListC(ts, baseURL, pms)) pms' replace =
  let merge = if replace then flip (<>) else (<>)
  in  TListC(ts, baseURL, merge pms pms')

baseUrl' :: RDF TList -> Maybe BaseUrl
baseUrl' (TListC(_, baseURL, _)) = baseURL

empty' :: RDF TList
empty' = TListC(mempty, Nothing, PrefixMappings mempty)

-- We no longer remove duplicates here, as it is very time consuming and is often not
-- necessary (raptor does not seem to remove dupes either). Instead, we remove dupes
-- from the results of the select' and query' functions, since it is cheap to do
-- there in most cases, but not when triplesOf' is called.
mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF TList
mkRdf' ts baseURL pms = TListC (ts, baseURL, pms)

addTriple' :: RDF TList -> Triple -> RDF TList
addTriple' (TListC (ts, bURL, preMapping)) t = TListC (t:ts,bURL,preMapping)

removeTriple' :: RDF TList -> Triple -> RDF TList
removeTriple' (TListC (ts, bURL, preMapping)) t = TListC (newTs,bURL,preMapping)
  where newTs = filter (/= t) ts

triplesOf' :: RDF TList -> Triples
triplesOf' ((TListC(ts, _, _))) = ts

uniqTriplesOf' :: RDF TList -> Triples
uniqTriplesOf' = nub . expandTriples

select' :: RDF TList -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' g s p o = nub $ filter (matchSelect s p o) $ triplesOf g

query' :: RDF TList -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
query' g s p o = nub $ filter (matchPattern s p o) $ triplesOf g

matchSelect :: NodeSelector -> NodeSelector -> NodeSelector -> Triple -> Bool
matchSelect s p o (Triple s' p' o') = match s s' && match p p' && match o o'
  where match fn n = maybe True ($ n) fn

matchPattern :: Maybe Subject -> Maybe Predicate -> Maybe Object -> Triple -> Bool
matchPattern s p o (Triple s' p' o') = match s s' && match p p' && match o o'
  where match n1 n2 = maybe True (==n2) n1
