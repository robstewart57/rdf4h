{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving #-}
-- |A graph implementation mapping (S,P) pairs to O, backed by 'Data.Map'.

module Data.RDF.Graph.HashMapSP (HashMapSP) where

import Prelude hiding (pred)
import Control.DeepSeq (NFData)
import Data.RDF.Types
import Data.RDF.Query
import Data.RDF.Namespace
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.List

-- |A map-based graph implementation.

newtype HashMapSP = HashMapSP (SPMap, Maybe BaseUrl, PrefixMappings)
                 deriving (NFData)

instance Rdf HashMapSP where
  data RDF HashMapSP = RDF HashMapSP
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

instance Show (HashMapSP) where
  show (HashMapSP (tsMap,_,_)) =
    let ts = (concatMap (\((s,p),oList) -> map (Triple s p) oList) . HashMap.toList) tsMap
    in concatMap (\t -> show t ++ "\n") ts

showGraph' :: RDF HashMapSP -> [Char]
showGraph' (RDF (HashMapSP (tsMap,_,_))) =
  let ts = (concatMap (\((s,p),oList) -> map (Triple s p) oList) . HashMap.toList) tsMap
  in concatMap (\t -> show t ++ "\n") ts
  
-- instance Show (RDF HashMapSP) where
--   show gr = concatMap (\t -> show t ++ "\n")  (triplesOf gr)

type SPMap = HashMap (Subject,Predicate) [Object]

baseUrl' :: RDF HashMapSP -> Maybe BaseUrl
baseUrl' (RDF (HashMapSP (_, baseURL, _))) = baseURL

prefixMappings' :: RDF HashMapSP -> PrefixMappings
prefixMappings' (RDF (HashMapSP (_, _, pms))) = pms

addPrefixMappings' :: RDF HashMapSP -> PrefixMappings -> Bool -> RDF HashMapSP
addPrefixMappings' (RDF (HashMapSP (tsMap, baseURL, pms))) pms' replace =
  let merge = if replace then flip mergePrefixMappings else mergePrefixMappings
  in  RDF $ HashMapSP (tsMap, baseURL, merge pms pms')

empty' :: RDF HashMapSP
empty' = RDF $ HashMapSP (HashMap.empty, Nothing, PrefixMappings Map.empty)

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF HashMapSP
mkRdf' triples baseURL pms = RDF $ HashMapSP (tsMap, baseURL, pms)
    where
      tsMap = sortAndGroup triples
      sortAndGroup xs = HashMap.fromListWith (++) [((s,p), [o]) | Triple s p o <- xs]

triplesOf' :: RDF HashMapSP -> Triples
triplesOf' (RDF (HashMapSP (tsMap,_,_))) = (concatMap (\((s,p),oList) -> map (Triple s p) oList) . HashMap.toList) tsMap

uniqTriplesOf' :: RDF HashMapSP -> Triples
uniqTriplesOf' = nub . expandTriples

select' :: RDF HashMapSP -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' gr Nothing Nothing Nothing =
    triplesOf' gr

select' (RDF (HashMapSP (tsMap,_,_)))    Nothing  (Just pSelector) Nothing  =
    HashMap.foldrWithKey findTripleWithP [] tsMap
    where
      findTripleWithP (s,p) oList ts = if pSelector p
                                       then map (Triple s p) oList ++ ts
                                       else ts

select' (RDF (HashMapSP (tsMap,_,_)))    Nothing  Nothing  (Just oSelector) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = map (Triple s p) (filter oSelector oList) ++ ts

select' (RDF (HashMapSP (tsMap,_,_)))    Nothing  (Just pSelector) (Just oSelector) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if pSelector p
                                       then map (Triple s p) (filter oSelector oList) ++ ts
                                       else ts

select' (RDF (HashMapSP (tsMap,_,_)))    (Just sSelector) Nothing  Nothing  =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s
                                       then map (Triple s p) oList ++ ts
                                       else ts

select' (RDF (HashMapSP (tsMap,_,_)))    (Just sSelector) (Just pSelector) Nothing =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s && pSelector p
                                       then map (Triple s p) oList ++ ts
                                       else ts

select' (RDF (HashMapSP (tsMap,_,_)))    (Just sSelector) Nothing  (Just oSelector) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s
                                        then map (Triple s p) (filter oSelector oList) ++ ts
                                        else ts

select' (RDF (HashMapSP (tsMap,_,_)))    (Just sSelector) (Just pSelector) (Just oSelector) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s && pSelector p
                                       then map (Triple s p) (filter oSelector oList) ++ ts
                                       else ts

query' :: RDF HashMapSP -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
query' gr Nothing  Nothing  Nothing  =
    triplesOf' gr

query' (RDF (HashMapSP (tsMap,_,_)))    Nothing  (Just p) Nothing  =
    HashMap.foldrWithKey findTripleWithP [] tsMap
    where
      findTripleWithP (s,p') oList ts = if p == p'
                                        then map (Triple s p) oList ++ ts
                                        else ts

query' (RDF (HashMapSP (tsMap,_,_)))    Nothing  Nothing  (Just o) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = map (Triple s p) (filter (== o) oList) ++ ts

query' (RDF (HashMapSP (tsMap,_,_)))    Nothing  (Just p) (Just o) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p') oList ts = if p == p'
                                        then map (Triple s p) (filter (== o) oList) ++ ts
                                        else ts

query' (RDF (HashMapSP (tsMap,_,_)))    (Just s) Nothing  Nothing  =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s',p) oList ts = if s == s'
                                        then map (Triple s p) oList ++ ts
                                        else ts

-- optimal pattern for this RDF HashMapSP instance
query' (RDF (HashMapSP (tsMap,_,_)))    (Just s) (Just p) Nothing =
    (map (Triple s p) . HashMap.lookupDefault [] (s,p)) tsMap

query' (RDF (HashMapSP (tsMap,_,_)))    (Just s) Nothing  (Just o) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s',p) oList ts = if s == s'
                                        then map (Triple s p) (filter (== o) oList) ++ ts
                                        else ts

query' (RDF (HashMapSP (tsMap,_,_)))    (Just s) (Just p) (Just o) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s',p') oList ts = if s == s' && p == p'
                                        then map (Triple s p) (filter (== o) oList) ++ ts
                                        else ts
