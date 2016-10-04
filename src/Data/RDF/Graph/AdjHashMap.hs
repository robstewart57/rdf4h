{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |A graph implementation mapping hashed S to a mapping of
--  hashed P to hashed O, backed by 'Data.HashMap'.

module Data.RDF.Graph.AdjHashMap (AdjHashMap) where

import Prelude hiding (pred)
import Control.DeepSeq (NFData)
import Data.RDF.Types
import Data.RDF.Query
import Data.RDF.Namespace
import qualified Data.Map as Map
import Data.Hashable()
import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet(HashSet)
import qualified Data.HashSet as Set
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

-- instance Show (AdjHashMap) where
--   show (AdjHashMap ((spoMap, _), _, _)) =
--     let ts = concatMap (uncurry tripsSubj) subjPredMaps
--           where subjPredMaps = HashMap.toList spoMap
--     in concatMap (\t -> show t ++ "\n") ts

showGraph' :: RDF AdjHashMap -> [Char]
showGraph' ((AdjHashMap ((spoMap, _), _, _))) =
    let ts = concatMap (uncurry tripsSubj) subjPredMaps
          where subjPredMaps = HashMap.toList spoMap
    in concatMap (\t -> show t ++ "\n") ts

-- instance Show (RDF AdjHashMap) where
--   show gr = concatMap (\t -> show t ++ "\n")  (triplesOf gr)

-- some convenience type alias for readability

-- An adjacency map for a subject, mapping from a predicate node to
-- to the adjacent nodes via that predicate.
type AdjacencyMap = HashMap Predicate (HashSet Node)

type Adjacencies = HashSet Node

type TMap   = HashMap Node AdjacencyMap
type TMaps  = (TMap, TMap)


baseUrl' :: RDF AdjHashMap -> Maybe BaseUrl
baseUrl' (AdjHashMap (_, baseURL, _)) = baseURL

prefixMappings' :: RDF AdjHashMap -> PrefixMappings
prefixMappings' (AdjHashMap (_, _, pms)) = pms

addPrefixMappings' :: RDF AdjHashMap -> PrefixMappings -> Bool -> RDF AdjHashMap
addPrefixMappings' (AdjHashMap (ts, baseURL, pms)) pms' replace = 
  let merge = if replace then flip mergePrefixMappings else mergePrefixMappings
  in  AdjHashMap (ts, baseURL, merge pms pms')

empty' :: RDF AdjHashMap
empty' = AdjHashMap ((HashMap.empty, HashMap.empty), Nothing, PrefixMappings Map.empty)

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF AdjHashMap
mkRdf' ts baseURL pms = AdjHashMap (mergeTs (HashMap.empty, HashMap.empty) ts, baseURL, pms)

mergeTs :: TMaps -> [Triple] -> TMaps
mergeTs = foldl' mergeT
  where
    mergeT :: TMaps -> Triple -> TMaps
    mergeT m t = mergeT' m (subjectOf t) (predicateOf t) (objectOf t)

mergeT' :: TMaps -> Subject -> Predicate -> Object -> TMaps
mergeT' (spo, ops) s p o = (mergeT'' spo s p o, mergeT'' ops o p s)

mergeT'' :: TMap -> Subject -> Predicate -> Object -> TMap
mergeT'' m s p o =
  if s `HashMap.member` m then
    (if p `HashMap.member` adjs then HashMap.insert s addPredObj m
       else HashMap.insert s addNewPredObjMap m)
    else HashMap.insert s newPredMap m
  where
    adjs = HashMap.lookupDefault HashMap.empty s m
    newPredMap :: HashMap Predicate (HashSet Object)
    newPredMap = HashMap.singleton p (Set.singleton o)
    addNewPredObjMap :: HashMap Predicate (HashSet Object)
    addNewPredObjMap = HashMap.insert p (Set.singleton o) adjs
    addPredObj :: HashMap Predicate (HashSet Object)
    addPredObj = HashMap.insert p (Set.insert o (get p adjs)) adjs
    --get :: (Ord k, Hashable k) => k -> HashMap k v -> v
    get = HashMap.lookupDefault Set.empty

-- 3 following functions support triplesOf
triplesOf' :: RDF AdjHashMap -> Triples
triplesOf' (AdjHashMap ((spoMap, _), _, _)) = concatMap (uncurry tripsSubj) subjPredMaps
  where subjPredMaps = HashMap.toList spoMap

-- naive implementation for now
uniqTriplesOf' :: RDF AdjHashMap -> Triples
uniqTriplesOf' = nub . expandTriples

tripsSubj :: Subject -> AdjacencyMap -> Triples
tripsSubj s adjMap = concatMap (uncurry (tfsp s)) (HashMap.toList adjMap)
  where tfsp = tripsForSubjPred

tripsForSubjPred :: Subject -> Predicate -> Adjacencies -> Triples
tripsForSubjPred s p adjs = map (Triple s p) (Set.toList adjs)

-- supports select
select' :: RDF AdjHashMap -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' (AdjHashMap ((spoMap,_),_,_)) subjFn predFn objFn =
  map (\(s,p,o) -> Triple s p o) $ Set.toList $ sel1 subjFn predFn objFn spoMap

sel1 :: NodeSelector -> NodeSelector -> NodeSelector -> TMap -> HashSet (Node, Node, Node)
sel1 (Just subjFn) p o spoMap =
  Set.unions $ map (sel2 p o) $ filter (\(x,_) -> subjFn x) $ HashMap.toList spoMap
sel1 Nothing p o spoMap = Set.unions $ map (sel2 p o) $ HashMap.toList spoMap

sel2 :: NodeSelector -> NodeSelector -> (Node, HashMap Node (HashSet Node)) -> HashSet (Node, Node, Node)
sel2 (Just predFn) mobjFn (s, ps) =
  Set.map (\(p,o) -> (s,p,o)) $
  foldl' Set.union Set.empty $
  map (sel3 mobjFn) poMapS :: HashSet (Node, Node, Node)
  where
    poMapS :: [(Node, HashSet Node)]
    poMapS = filter (\(k,_) -> predFn k) $ HashMap.toList ps
sel2 Nothing mobjFn (s, ps) =
  Set.map (\(p,o) -> (s,p,o)) $
  foldl' Set.union Set.empty $
  map (sel3 mobjFn) poMaps
  where
    poMaps = HashMap.toList ps

sel3 :: NodeSelector -> (Node, HashSet Node) -> HashSet (Node, Node)
sel3 (Just objFn) (p, os) = Set.map (\o -> (p, o)) $ Set.filter objFn os
sel3 Nothing      (p, os) = Set.map (\o -> (p, o)) os

-- support query
query' :: RDF AdjHashMap -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
query' (AdjHashMap (m,_ , _)) subj pred obj = map (\(s, p, o) -> Triple s p o) $ Set.toList (q1 subj pred obj m)

q1 :: Maybe Node -> Maybe Node -> Maybe Node -> TMaps -> HashSet (Node, Node, Node)
q1 (Just s) p o        (spoMap, _     ) = q2 p o (s, HashMap.lookupDefault HashMap.empty s spoMap)
q1 s        p (Just o) (_     , opsMap) = Set.map (\(o',p',s') -> (s',p',o')) $ q2 p s (o, HashMap.lookupDefault HashMap.empty o opsMap)
q1 Nothing (Just p) Nothing (spoMap,_) =
  let (filtered::HashMap Node (HashMap Node (HashSet Node)))
        = HashMap.filter (\poAdjMap -> HashMap.member p poAdjMap) spoMap
      subjXS = HashMap.toList filtered :: [(Node,AdjacencyMap)]
      ys = concatMap (\(s,poMap) ->
                        let objs = map snd (HashMap.toList poMap :: [(Predicate,HashSet Node)])
                        in concatMap (\objHash -> map (\o -> (s,p,o)) (Set.toList objHash)) objs
                     ) subjXS
  in Set.fromList ys
{-
      (xs::[(Subject,Predicate,Object)]) = HashMap.foldlWithKey' f [] filtered
      f triples s poAdjMap =
        let objs = HashMap.elems poAdjMap
        in triples ++ concatMap (\oSet -> map (\o -> (s,p,o)) (Set.toList oSet)) objs
  in Set.fromList xs
-}

q1 Nothing  p o        (spoMap, _     ) =
  let hashSets = map (q2 p o) (HashMap.toList spoMap) :: [HashSet (Node,Node,Node)]
  in Set.unions hashSets

-- | takes a @Maybe Predicate@ and a @Maybe Object@, a subject and
-- predicate map tuple, and returns a (s,p,o) hash set.
q2
  :: Maybe Node
  -> Maybe Node
  -> (Node, HashMap Node (HashSet Node))
  -> HashSet (Node, Node, Node)
q2 (Just p) o (s, pmap) =
  let objAdjHashMapetMaybe = HashMap.lookup p pmap :: Maybe (HashSet Node) -- lookup object hash set
  in case objAdjHashMapetMaybe of
       Nothing -> Set.empty
       Just objAdjHashMapet ->
         let poAdjHashMapet = q3 o (p,objAdjHashMapet)
         in Set.map (\(p', o') -> (s, p', o')) poAdjHashMapet
q2 Nothing o (s, pmap) =
  Set.map (\(x, y) -> (s, x, y)) $ Set.unions $ map (q3 o) opmaps
  where
    opmaps :: [(Node, HashSet Node)]
    opmaps = HashMap.toList pmap

-- | looks up an object in an object hash set. If it exists, then this
-- function returns a hashset with a set with single (p,o) tuple
-- element. If the @Maybe Object@ value is @Empty@, then create a hash
-- set of (p,o) tuples for every object in the object hash set.
q3 :: Maybe Node -- ^ object
   -> (Node, HashSet Node) -- ^ predicate and object hash set tuple
   -> HashSet (Node, Node) -- ^ hash set of (p,o) tuples
q3 (Just o) (p, os) =
  if o `Set.member` os -- if the queried object is in the object set
    then Set.singleton (p, o) -- return a set with a single (p,o) tuple element 
    else Set.empty -- otherwise return an empty set
q3 Nothing (p, os) = Set.map (\o -> (p, o)) os
