{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving #-}
-- |A graph implementation mapping (S,P) pairs to O, backed by 'Data.Map'.

module Data.RDF.Graph.MapSP (MapSP) where

import Prelude hiding (pred)
import Control.DeepSeq (NFData)
import Data.RDF.Types
import Data.RDF.Query
import Data.RDF.Namespace
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

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

newtype MapSP = MapSP (SPMap, Maybe BaseUrl, PrefixMappings)
                 deriving (NFData)

instance Rdf MapSP where
  data RDF MapSP = RDF MapSP
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

instance Show MapSP where
  show (MapSP (tsMap,_,_)) =
    let ts = (concatMap (\((s,p),oList) -> map (Triple s p) oList) . Map.toList) tsMap
    in concatMap (\t -> show t ++ "\n") ts

showGraph' :: RDF MapSP -> String
showGraph' (RDF (MapSP (tsMap,_,_))) =
  let ts = (concatMap (\((s,p),oList) -> map (Triple s p) oList) . Map.toList) tsMap
    in concatMap (\t -> show t ++ "\n") ts

type SPMap = Map (Subject,Predicate) [Object]

baseUrl' :: RDF MapSP -> Maybe BaseUrl
baseUrl' (RDF (MapSP (_, baseURL, _))) = baseURL

prefixMappings' :: RDF MapSP -> PrefixMappings
prefixMappings' (RDF (MapSP (_, _, pms))) = pms

addPrefixMappings' :: RDF MapSP -> PrefixMappings -> Bool -> RDF MapSP
addPrefixMappings' (RDF (MapSP (tsMap, baseURL, pms))) pms' replace =
  let merge = if replace then flip mergePrefixMappings else mergePrefixMappings
  in  RDF $ MapSP (tsMap, baseURL, merge pms pms')

empty' :: RDF MapSP
empty' = RDF $ MapSP (Map.empty, Nothing, PrefixMappings Map.empty)

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF MapSP
mkRdf' triples baseURL pms = RDF $ MapSP (tsMap, baseURL, pms)
    where
      tsMap = sortAndGroup triples
      sortAndGroup xs = Map.fromListWith (++) [((s,p), [o]) | Triple s p o <- xs]

triplesOf' :: RDF MapSP -> Triples
triplesOf' (RDF (MapSP (tsMap,_,_))) = (concatMap (\((s,p),oList) -> map (Triple s p) oList) . Map.toList) tsMap

uniqTriplesOf' :: RDF MapSP -> Triples
uniqTriplesOf' = nub . expandTriples

select' :: RDF MapSP -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' gr Nothing Nothing Nothing =
    triplesOf' gr

select' (RDF (MapSP (tsMap,_,_)))   Nothing  (Just pSelector) Nothing  =
    Map.foldrWithKey findTripleWithP [] tsMap
    where
      findTripleWithP (s,p) oList ts = if pSelector p
                                       then map (Triple s p) oList ++ ts
                                       else ts

select' (RDF (MapSP (tsMap,_,_)))    Nothing  Nothing  (Just oSelector) =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = map (Triple s p) (filter oSelector oList) ++ ts

select' (RDF (MapSP (tsMap,_,_)))    Nothing  (Just pSelector) (Just oSelector) =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if pSelector p
                                       then map (Triple s p) (filter oSelector oList) ++ ts
                                       else ts

select' (RDF (MapSP (tsMap,_,_)))    (Just sSelector) Nothing  Nothing  =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s
                                       then map (Triple s p) oList ++ ts
                                       else ts

select' (RDF (MapSP (tsMap,_,_)))    (Just sSelector) (Just pSelector) Nothing =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s && pSelector p
                                       then map (Triple s p) oList ++ ts
                                       else ts

select' (RDF (MapSP (tsMap,_,_)))    (Just sSelector) Nothing  (Just oSelector) =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s
                                        then map (Triple s p) (filter oSelector oList) ++ ts
                                        else ts

select' (RDF (MapSP (tsMap,_,_)))    (Just sSelector) (Just pSelector) (Just oSelector) =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s && pSelector p
                                       then map (Triple s p) (filter oSelector oList) ++ ts
                                       else ts

query' :: RDF MapSP -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
query' gr Nothing  Nothing  Nothing  =
    triplesOf' gr

query' (RDF (MapSP (tsMap,_,_)))    Nothing  (Just p) Nothing  =
    Map.foldrWithKey findTripleWithP [] tsMap
    where
      findTripleWithP (s,p') oList ts = if p == p'
                                        then map (Triple s p) oList ++ ts
                                        else ts

query' (RDF (MapSP (tsMap,_,_)))    Nothing  Nothing  (Just o) =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = map (Triple s p) (filter (== o) oList) ++ ts

query' (RDF (MapSP (tsMap,_,_)))    Nothing  (Just p) (Just o) =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p') oList ts = if p == p'
                                        then map (Triple s p) (filter (== o) oList) ++ ts
                                        else ts

query' (RDF (MapSP (tsMap,_,_)))    (Just s) Nothing  Nothing  =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s',p) oList ts = if s == s'
                                        then map (Triple s p) oList ++ ts
                                        else ts

-- optimal pattern for this MapSP instance
query' (RDF (MapSP (tsMap,_,_)))    (Just s) (Just p) Nothing =
    (map (Triple s p) . Map.findWithDefault [] (s,p)) tsMap

query' (RDF (MapSP (tsMap,_,_)))    (Just s) Nothing  (Just o) =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s',p) oList ts = if s == s'
                                        then map (Triple s p) (filter (== o) oList) ++ ts
                                        else ts

query' (RDF (MapSP (tsMap,_,_)))    (Just s) (Just p) (Just o) =
    Map.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s',p') oList ts = if s == s' && p == p'
                                        then map (Triple s p) (filter (== o) oList) ++ ts
                                        else ts
