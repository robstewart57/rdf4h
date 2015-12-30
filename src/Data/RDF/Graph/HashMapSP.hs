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

instance RDF HashMapSP where
  baseUrl           = baseUrl'
  prefixMappings    = prefixMappings'
  addPrefixMappings = addPrefixMappings'
  empty             = empty'
  mkRdf             = mkRdf'
  triplesOf         = triplesOf'
  uniqTriplesOf     = uniqTriplesOf'
  select            = select'
  query             = query'

instance Show HashMapSP where
  show gr = concatMap (\t -> show t ++ "\n")  (triplesOf gr)

type SPMap = HashMap (Subject,Predicate) [Object]

baseUrl' :: HashMapSP -> Maybe BaseUrl
baseUrl' (HashMapSP (_, baseURL, _)) = baseURL

prefixMappings' :: HashMapSP -> PrefixMappings
prefixMappings' (HashMapSP (_, _, pms)) = pms

addPrefixMappings' :: HashMapSP -> PrefixMappings -> Bool -> HashMapSP
addPrefixMappings' (HashMapSP (tsMap, baseURL, pms)) pms' replace =
  let merge = if replace then flip mergePrefixMappings else mergePrefixMappings
  in  HashMapSP (tsMap, baseURL, merge pms pms')

empty' :: HashMapSP
empty' = HashMapSP (HashMap.empty, Nothing, PrefixMappings Map.empty)

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> HashMapSP
mkRdf' triples baseURL pms = HashMapSP (tsMap, baseURL, pms)
    where
      tsMap = sortAndGroup triples
      sortAndGroup xs = HashMap.fromListWith (++) [((s,p), [o]) | Triple s p o <- xs]

triplesOf' :: HashMapSP -> Triples
triplesOf' (HashMapSP (tsMap,_,_)) = (concatMap (\((s,p),oList) -> map (Triple s p) oList) . HashMap.toList) tsMap

uniqTriplesOf' :: HashMapSP -> Triples
uniqTriplesOf' = nub . expandTriples

select' :: HashMapSP -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' gr Nothing Nothing Nothing =
    triplesOf' gr

select' (HashMapSP (tsMap,_,_))    Nothing  (Just pSelector) Nothing  =
    HashMap.foldrWithKey findTripleWithP [] tsMap
    where
      findTripleWithP (s,p) oList ts = if pSelector p
                                       then map (Triple s p) oList ++ ts
                                       else ts

select' (HashMapSP (tsMap,_,_))    Nothing  Nothing  (Just oSelector) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = map (Triple s p) (filter oSelector oList) ++ ts

select' (HashMapSP (tsMap,_,_))    Nothing  (Just pSelector) (Just oSelector) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if pSelector p
                                       then map (Triple s p) (filter oSelector oList) ++ ts
                                       else ts

select' (HashMapSP (tsMap,_,_))    (Just sSelector) Nothing  Nothing  =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s
                                       then map (Triple s p) oList ++ ts
                                       else ts

select' (HashMapSP (tsMap,_,_))    (Just sSelector) (Just pSelector) Nothing =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s && pSelector p
                                       then map (Triple s p) oList ++ ts
                                       else ts

select' (HashMapSP (tsMap,_,_))    (Just sSelector) Nothing  (Just oSelector) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s
                                        then map (Triple s p) (filter oSelector oList) ++ ts
                                        else ts

select' (HashMapSP (tsMap,_,_))    (Just sSelector) (Just pSelector) (Just oSelector) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s && pSelector p
                                       then map (Triple s p) (filter oSelector oList) ++ ts
                                       else ts

query' :: HashMapSP -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
query' gr Nothing  Nothing  Nothing  =
    triplesOf' gr

query' (HashMapSP (tsMap,_,_))    Nothing  (Just p) Nothing  =
    HashMap.foldrWithKey findTripleWithP [] tsMap
    where
      findTripleWithP (s,p') oList ts = if p == p'
                                        then map (Triple s p) oList ++ ts
                                        else ts

query' (HashMapSP (tsMap,_,_))    Nothing  Nothing  (Just o) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = map (Triple s p) (filter (== o) oList) ++ ts

query' (HashMapSP (tsMap,_,_))    Nothing  (Just p) (Just o) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p') oList ts = if p == p'
                                        then map (Triple s p) (filter (== o) oList) ++ ts
                                        else ts

query' (HashMapSP (tsMap,_,_))    (Just s) Nothing  Nothing  =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s',p) oList ts = if s == s'
                                        then map (Triple s p) oList ++ ts
                                        else ts

-- optimal pattern for this HashMapSP instance
query' (HashMapSP (tsMap,_,_))    (Just s) (Just p) Nothing =
    (map (Triple s p) . HashMap.lookupDefault [] (s,p)) tsMap

query' (HashMapSP (tsMap,_,_))    (Just s) Nothing  (Just o) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s',p) oList ts = if s == s'
                                        then map (Triple s p) (filter (== o) oList) ++ ts
                                        else ts

query' (HashMapSP (tsMap,_,_))    (Just s) (Just p) (Just o) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s',p') oList ts = if s == s' && p == p'
                                        then map (Triple s p) (filter (== o) oList) ++ ts
                                        else ts
