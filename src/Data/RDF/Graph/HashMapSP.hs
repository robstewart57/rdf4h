{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}

-- |A graph implementation mapping (S,P) pairs to O, backed by 'Data.Map'.

module Data.RDF.Graph.HashMapSP (HashSP) where

import Prelude hiding (pred)
import Control.DeepSeq (NFData)
import Data.RDF.Types
import Data.RDF.Query
import Data.RDF.Namespace
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.List
import GHC.Generics
import Data.Binary (Binary)

data HashSP deriving (Generic)

instance Binary HashSP
instance NFData HashSP

-- |A map-based graph implementation.

data instance RDF HashSP = HashSP (SPMap, Maybe BaseUrl, PrefixMappings)
                 deriving (NFData,Generic)

instance Rdf HashSP where
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

-- instance Show (HashSP) where
--   show (HashSP (tsMap,_,_)) =
--     let ts = (concatMap (\((s,p),oList) -> map (Triple s p) oList) . HashMap.toList) tsMap
--     in concatMap (\t -> show t ++ "\n") ts

showGraph' :: RDF HashSP -> [Char]
showGraph' (HashSP (tsMap,_,_)) =
  let ts = (concatMap (\((s,p),oList) -> map (Triple s p) oList) . HashMap.toList) tsMap
  in concatMap (\t -> show t ++ "\n") ts
  
-- instance Show (HashSP) where
--   show gr = concatMap (\t -> show t ++ "\n")  (triplesOf gr)

type SPMap = HashMap (Subject,Predicate) [Object]

baseUrl' :: RDF HashSP -> Maybe BaseUrl
baseUrl' (HashSP (_, baseURL, _)) = baseURL

prefixMappings' :: RDF HashSP -> PrefixMappings
prefixMappings' (HashSP (_, _, pms)) = pms

addPrefixMappings' :: RDF HashSP -> PrefixMappings -> Bool -> RDF HashSP
addPrefixMappings' (HashSP (tsMap, baseURL, pms)) pms' replace =
  let merge = if replace then flip mergePrefixMappings else mergePrefixMappings
  in  HashSP (tsMap, baseURL, merge pms pms')

empty' :: RDF HashSP
empty' = HashSP (HashMap.empty, Nothing, PrefixMappings Map.empty)

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF HashSP
mkRdf' triples baseURL pms = HashSP (tsMap, baseURL, pms)
    where
      tsMap = sortAndGroup triples
      sortAndGroup xs = HashMap.fromListWith (++) [((s,p), [o]) | Triple s p o <- xs]

triplesOf' :: RDF HashSP -> Triples
triplesOf' (HashSP (tsMap,_,_)) = (concatMap (\((s,p),oList) -> map (Triple s p) oList) . HashMap.toList) tsMap

uniqTriplesOf' :: RDF HashSP -> Triples
uniqTriplesOf' = nub . expandTriples

select' :: RDF HashSP -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' gr Nothing Nothing Nothing =
    triplesOf' gr

select' (HashSP (tsMap,_,_))    Nothing  (Just pSelector) Nothing  =
    HashMap.foldrWithKey findTripleWithP [] tsMap
    where
      findTripleWithP (s,p) oList ts = if pSelector p
                                       then map (Triple s p) oList ++ ts
                                       else ts

select' (HashSP (tsMap,_,_))    Nothing  Nothing  (Just oSelector) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = map (Triple s p) (filter oSelector oList) ++ ts

select' (HashSP (tsMap,_,_))    Nothing  (Just pSelector) (Just oSelector) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if pSelector p
                                       then map (Triple s p) (filter oSelector oList) ++ ts
                                       else ts

select' (HashSP (tsMap,_,_))    (Just sSelector) Nothing  Nothing  =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s
                                       then map (Triple s p) oList ++ ts
                                       else ts

select' (HashSP (tsMap,_,_))    (Just sSelector) (Just pSelector) Nothing =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s && pSelector p
                                       then map (Triple s p) oList ++ ts
                                       else ts

select' (HashSP (tsMap,_,_))    (Just sSelector) Nothing  (Just oSelector) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s
                                        then map (Triple s p) (filter oSelector oList) ++ ts
                                        else ts

select' (HashSP (tsMap,_,_))    (Just sSelector) (Just pSelector) (Just oSelector) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = if sSelector s && pSelector p
                                       then map (Triple s p) (filter oSelector oList) ++ ts
                                       else ts

query' :: RDF HashSP -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
query' gr Nothing  Nothing  Nothing  =
    triplesOf' gr

query' (HashSP (tsMap,_,_))    Nothing  (Just p) Nothing  =
    HashMap.foldrWithKey findTripleWithP [] tsMap
    where
      findTripleWithP (s,p') oList ts = if p == p'
                                        then map (Triple s p) oList ++ ts
                                        else ts

query' (HashSP (tsMap,_,_))    Nothing  Nothing  (Just o) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p) oList ts = map (Triple s p) (filter (== o) oList) ++ ts

query' (HashSP (tsMap,_,_))    Nothing  (Just p) (Just o) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s,p') oList ts = if p == p'
                                        then map (Triple s p) (filter (== o) oList) ++ ts
                                        else ts

query' (HashSP (tsMap,_,_))    (Just s) Nothing  Nothing  =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s',p) oList ts = if s == s'
                                        then map (Triple s p) oList ++ ts
                                        else ts

-- optimal pattern for this RDF HashSP instance
query' (HashSP (tsMap,_,_))    (Just s) (Just p) Nothing =
    (map (Triple s p) . HashMap.lookupDefault [] (s,p)) tsMap

query' (HashSP (tsMap,_,_))    (Just s) Nothing  (Just o) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s',p) oList ts = if s == s'
                                        then map (Triple s p) (filter (== o) oList) ++ ts
                                        else ts

query' (HashSP (tsMap,_,_))    (Just s) (Just p) (Just o) =
    HashMap.foldrWithKey findTripleWithS [] tsMap
    where
      findTripleWithS (s',p') oList ts = if s == s' && p == p'
                                        then map (Triple s p) (filter (== o) oList) ++ ts
                                        else ts
