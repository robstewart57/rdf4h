{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |A graph implementation mapping hashed S to a mapping of
--  hashed P to hashed O, backed by 'Data.HashMap'.

module Data.RDF.Graph.AdjHashMap (AdjHashMap) where

import Prelude hiding (pred)
#if MIN_VERSION_base(4,9,0)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#else
#endif
#else
#endif
import Data.List
import Data.Binary (Binary)
import Data.RDF.Types
import Data.RDF.Query
import Data.Hashable ()
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Control.Monad (mfilter)
import Control.DeepSeq (NFData)
import GHC.Generics

-- |A map-based graph implementation.
--
-- This instance of 'RDF' is an adjacency map with each subject
-- mapping to a mapping from a predicate node to the adjacent nodes
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
-- where
--
-- > hash "http://example.com/s1" = 1600134414
-- > hash "http://example.com/s2" = 1600134413
-- > hash "http://example.com/p1" = 1616912099
-- > hash "http://example.com/p2" = 1616912096
-- > hash "http://example.com/p3" = 1616912097
-- > hash "http://example.com/o1" = 1935686794
-- > hash "http://example.com/o2" = 1935686793
-- > hash "http://example.com/o3" = 1935686792
--
-- the in-memory hashmap representation of the triples graph is:
--
-- @
-- key:1600134414, value:(key:1616912099, value:[1935686794    -- (..\/s1,..\/p1,..\/o1)
--                                              ,1935686793];  -- (..\/s1,..\/p1,..\/o2)
--                        key:1616912096, value:[1935686794]); -- (..\/s1,..\/p2,..\/o1)
-- key:1600134413, value:(key:1616912097, value:[1935686792])  -- (..\/s1,..\/p3,..\/o3)
-- @
--
-- Worst-case time complexity of the graph functions, with respect
-- to the number of triples, are:
--
--  * 'empty'    : O(1)
--
--  * 'mkRdf'  : O(n)
--
--  * 'triplesOf': O(n)
--
--  * 'select'   : O(n)
--
--  * 'query'    : O(log n)
-- newtype HashS = HashS (TMaps, Maybe BaseUrl, PrefixMappings)
--                  deriving (NFData)

data AdjHashMap deriving (Generic)

instance Binary AdjHashMap
instance NFData AdjHashMap

data instance RDF AdjHashMap = AdjHashMap (TMaps, Maybe BaseUrl, PrefixMappings)
                 deriving (NFData,Generic)

instance Rdf AdjHashMap where
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
  addTriple         = addTriple'
  removeTriple      = removeTriple'

-- instance Show (AdjHashMap) where
--   show (AdjHashMap ((spoMap, _), _, _)) =
--     let ts = concatMap (uncurry tripsSubj) subjPredMaps
--           where subjPredMaps = HashMap.toList spoMap
--     in concatMap (\t -> show t <> "\n") ts

showGraph' :: RDF AdjHashMap -> String
showGraph' ((AdjHashMap ((spoMap, _), _, _))) =
    let ts = concatMap (uncurry tripsSubj) subjPredMaps
          where subjPredMaps = HashMap.toList spoMap
    in concatMap (\t -> show t <> "\n") ts

-- instance Show (RDF AdjHashMap) where
--   show gr = concatMap (\t -> show t <> "\n")  (triplesOf gr)

-- some convenience type alias for readability

-- An adjacency map for a subject, mapping from a predicate node to
-- to the adjacent nodes via that predicate.
type AdjacencyMap = HashMap Predicate Adjacencies
type Adjacencies = HashSet Node

type TMap   = HashMap Node AdjacencyMap
type TMaps  = (TMap, TMap)

baseUrl' :: RDF AdjHashMap -> Maybe BaseUrl
baseUrl' (AdjHashMap (_, baseURL, _)) = baseURL

prefixMappings' :: RDF AdjHashMap -> PrefixMappings
prefixMappings' (AdjHashMap (_, _, pms)) = pms

addPrefixMappings' :: RDF AdjHashMap -> PrefixMappings -> Bool -> RDF AdjHashMap
addPrefixMappings' (AdjHashMap (ts, baseURL, pms)) pms' replace =
  let merge = if replace then flip (<>) else (<>)
  in  AdjHashMap (ts, baseURL, merge pms pms')

empty' :: RDF AdjHashMap
empty' = AdjHashMap ((mempty, mempty), Nothing, PrefixMappings mempty)

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF AdjHashMap
mkRdf' ts baseURL pms = AdjHashMap (mergeTs (mempty, mempty) ts, baseURL, pms)

addTriple' :: RDF AdjHashMap -> Triple -> RDF AdjHashMap
addTriple' (AdjHashMap (tmaps, baseURL, pms)) t =
  let newTMaps = mergeTs tmaps [t]
  in AdjHashMap (newTMaps, baseURL, pms)

removeTriple' :: RDF AdjHashMap -> Triple -> RDF AdjHashMap
removeTriple' (AdjHashMap ((spo, ops), baseURL, pms)) (Triple s p o) =
  AdjHashMap (new_tmaps, baseURL, pms)
  where
    new_tmaps = (removeT s p o spo, removeT o p s ops)
    removeT s' p' o' = HashMap.alter (removePO p' o') s'
    removePO p' o' po = mfilter (not . null) $ HashMap.alter (removeO o') p' <$> po
    removeO o' os = mfilter (not . null) $ Set.delete o' <$> os

mergeTs :: TMaps -> Triples -> TMaps
mergeTs = foldl' mergeT
  where
    mergeT :: TMaps -> Triple -> TMaps
    mergeT (spo, ops) (Triple s p o) = (insertT s p o spo, insertT o p s ops)
    insertT :: Node -> Predicate -> Node -> TMap -> TMap
    insertT s p o = let newPO = HashMap.singleton p (Set.singleton o)
                    in HashMap.insertWith (HashMap.unionWith mappend) s newPO

-- 3 following functions support triplesOf
triplesOf' :: RDF AdjHashMap -> Triples
triplesOf' (AdjHashMap ((spoMap, _), _, _)) = concatMap (uncurry tripsSubj) subjPredMaps
  where subjPredMaps = HashMap.toList spoMap

-- naive implementation for now
uniqTriplesOf' :: RDF AdjHashMap -> Triples
uniqTriplesOf' = nub . expandTriples

tripsSubj :: Subject -> AdjacencyMap -> Triples
tripsSubj s adjMap = concatMap (tfsp s) (HashMap.toList adjMap)
  where tfsp s' (p, m) = Triple s' p <$> Set.toList m

-- supports select
select' :: RDF AdjHashMap -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' r Nothing Nothing Nothing = triplesOf r
select' (AdjHashMap ((_, ops),_,_)) Nothing p o = selectSPO o p Nothing (\a b c -> Triple c b a) ops
select' (AdjHashMap ((spo, _),_,_)) s       p o = selectSPO s p o       Triple                   spo

selectSPO :: NodeSelector -> NodeSelector -> NodeSelector -> (Node -> Node -> Node -> Triple) -> TMap -> Triples
selectSPO Nothing  p o t = concatMap (selectPO p o t) . HashMap.toList
selectSPO (Just s) p o t = concatMap (selectPO p o t) . filter (s . fst) . HashMap.toList

selectPO :: NodeSelector -> NodeSelector -> (Node -> Node -> Node -> Triple) -> (Node, AdjacencyMap) -> Triples
selectPO Nothing  o t (s, po) = concatMap (selectO o t s) . HashMap.toList $ po
selectPO (Just p) o t (s, po) = concatMap (selectO o t s) . filter (p . fst) . HashMap.toList $ po

selectO :: NodeSelector -> (Node -> Node -> Node -> Triple) -> Node -> (Node, Adjacencies) -> Triples
selectO o t s (p, os) = t s p <$> Set.toList os'
  where os' = maybe os (`Set.filter` os) o

-- support query
query' :: RDF AdjHashMap -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
query' r Nothing Nothing Nothing = triplesOf r
query' (AdjHashMap ((_, ops), _, _)) Nothing p o = querySPO o p Nothing (\a b c -> Triple c b a)  ops
query' (AdjHashMap ((spo, _), _, _)) s       p o = querySPO s p o       Triple                    spo

querySPO :: Maybe Node -> Maybe Node -> Maybe Node -> (Node -> Node -> Node -> Triple) -> TMap -> Triples
querySPO Nothing  p o t = concatMap (uncurry $ queryPO p o t) . HashMap.toList
querySPO (Just s) p o t = maybe mempty (queryPO p o t s) . HashMap.lookup s

queryPO :: Maybe Node -> Maybe Node -> (Node -> Node -> Node -> Triple) -> Node -> AdjacencyMap -> Triples
queryPO Nothing  o t s po = concatMap (uncurry $ queryO o t s) . HashMap.toList $ po
queryPO (Just p) o t s po = maybe mempty (queryO o t s p) $ HashMap.lookup p po

queryO :: Maybe Node -> (Node -> Node -> Node -> Triple) -> Node -> Node -> Adjacencies -> Triples
queryO Nothing  t s p os = t s p <$> Set.toList os
queryO (Just o) t s p os
  | o `Set.member` os = [t s p o]
  | otherwise         = mempty
