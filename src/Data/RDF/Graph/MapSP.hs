{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}


-- |A graph implementation mapping (S,P) pairs to O, backed by 'Data.Map'.

module Data.RDF.Graph.MapSP (SP) where

import Prelude hiding (pred)
import Control.DeepSeq (NFData)
import Data.RDF.Types
import Data.RDF.Query
import Data.RDF.Namespace
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import GHC.Generics
import Data.Binary (Binary)

-- |A map-based graph implementation.
--
-- This instance of 'RDF' is an adjacency map with each subject
-- mapping to a mapping from a predicate node to to the adjacent nodes
-- via that predicate.
--
-- Given the following triples graph::
--
-- @
--   (http:\/\/example.com\/s1,http:\/\/example.com\/p1,http:\/\/example.com\/o1)
--   (http:\/\/example.com\/s1,http:\/\/example.com\/p1,http:\/\/example.com\/o2)
--   (http:\/\/example.com\/s1,http:\/\/example.com\/p2,http:\/\/example.com\/o1)
--   (http:\/\/example.com\/s2,http:\/\/example.com\/p3,http:\/\/example.com\/o3)
-- @
--
-- the in-memory map representation of the triples graph is:
--
-- @
-- key:(http:\/\/example.com\/s1,http:\/\/example.com\/p1),
-- value:[http:\/\/example.com\/o1,http:\/\/example.com\/o2];
--
-- key:(http:\/\/example.com\/s1,http:\/\/example.com\/p2),
-- value:[http:\/\/example.com\/o1];
--
-- key:(http:\/\/example.com\/s2,http:\/\/example.com\/p3),
-- value:[http:\/\/example.com\/o3];
-- @

data SP deriving (Generic)
instance Binary SP
instance NFData SP

data instance RDF SP = SP (SPMap, Maybe BaseUrl, PrefixMappings)
                     deriving (Generic,NFData)

instance Rdf SP where
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

-- instance Show SP where
--   show (SP (tsMap,_,_)) =
--     let ts = (concatMap (\((s,p),oList) -> map (Triple s p) oList) . Map.toList) tsMap
--     in concatMap (\t -> show t <> "\n") ts

showGraph' :: RDF SP -> String
showGraph' (SP (tsMap,_,_)) =
  let ts = (concatMap (\((s,p),oList) -> map (Triple s p) oList) . Map.toList) tsMap
    in concatMap (\t -> show t <> "\n") ts

type SPMap = Map (Subject,Predicate) [Object]

baseUrl' :: RDF SP -> Maybe BaseUrl
baseUrl' (SP (_, baseURL, _)) = baseURL

prefixMappings' :: RDF SP -> PrefixMappings
prefixMappings' (SP (_, _, pms)) = pms

addPrefixMappings' :: RDF SP -> PrefixMappings -> Bool -> RDF SP
addPrefixMappings' (SP (tsMap, baseURL, pms)) pms' replace =
  let merge = if replace then flip mergePrefixMappings else mergePrefixMappings
  in  SP (tsMap, baseURL, merge pms pms')

empty' :: RDF SP
empty' = SP (Map.empty, Nothing, PrefixMappings Map.empty)

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF SP
mkRdf' triples baseURL pms = SP (tsMap, baseURL, pms)
    where
      tsMap = sortAndGroup triples
      sortAndGroup xs = Map.fromListWith (<>) [((s,p), [o]) | Triple s p o <- xs]

triplesOf' :: RDF SP -> Triples
triplesOf' (SP (tsMap,_,_)) = (concatMap (\((s,p),oList) -> map (Triple s p) oList) . Map.toList) tsMap

uniqTriplesOf' :: RDF SP -> Triples
uniqTriplesOf' = nub . expandTriples

select' :: RDF SP -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' gr Nothing Nothing Nothing =
    triplesOf' gr

select' (SP (tsMap,_,_))   Nothing  (Just pSelector) Nothing  =
    Map.foldrWithKey findTripleWithP [] tsMap
    where
      findTripleWithP (s,p) oList ts = if pSelector p
                                       then map (Triple s p) oList <> ts
                                       else ts

select' (SP (tsMap,_,_))    Nothing  Nothing  (Just oSelector) =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = map (Triple s p) (filter oSelector oList) <> ts

select' (SP (tsMap,_,_))    Nothing  (Just pSelector) (Just oSelector) =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if pSelector p
                                       then map (Triple s p) (filter oSelector oList) <> ts
                                       else ts

select' (SP (tsMap,_,_))    (Just sSelector) Nothing  Nothing  =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s
                                       then map (Triple s p) oList <> ts
                                       else ts

select' (SP (tsMap,_,_))    (Just sSelector) (Just pSelector) Nothing =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s && pSelector p
                                       then map (Triple s p) oList <> ts
                                       else ts

select' (SP (tsMap,_,_))    (Just sSelector) Nothing  (Just oSelector) =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s
                                        then map (Triple s p) (filter oSelector oList) <> ts
                                        else ts

select' (SP (tsMap,_,_))    (Just sSelector) (Just pSelector) (Just oSelector) =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s && pSelector p
                                       then map (Triple s p) (filter oSelector oList) <> ts
                                       else ts

query' :: RDF SP -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
query' gr Nothing  Nothing  Nothing  =
    triplesOf' gr

query' (SP (tsMap,_,_))    Nothing  (Just p) Nothing  =
    Map.foldrWithKey findTripleWithP [] tsMap
    where
      findTripleWithP (s,p') oList ts = if p == p'
                                        then map (Triple s p) oList <> ts
                                        else ts

query' (SP (tsMap,_,_))    Nothing  Nothing  (Just o) =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = map (Triple s p) (filter (== o) oList) <> ts

query' (SP (tsMap,_,_))    Nothing  (Just p) (Just o) =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p') oList ts = if p == p'
                                        then map (Triple s p) (filter (== o) oList) <> ts
                                        else ts

query' (SP (tsMap,_,_))    (Just s) Nothing  Nothing  =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s',p) oList ts = if s == s'
                                        then map (Triple s p) oList <> ts
                                        else ts

-- optimal pattern for this SP instance
query' (SP (tsMap,_,_))    (Just s) (Just p) Nothing =
    (map (Triple s p) . Map.findWithDefault [] (s,p)) tsMap

query' (SP (tsMap,_,_))    (Just s) Nothing  (Just o) =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s',p) oList ts = if s == s'
                                        then map (Triple s p) (filter (== o) oList) <> ts
                                        else ts

query' (SP (tsMap,_,_))    (Just s) (Just p) (Just o) =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s',p') oList ts = if s == s' && p == p'
                                        then map (Triple s p) (filter (== o) oList) <> ts
                                        else ts
