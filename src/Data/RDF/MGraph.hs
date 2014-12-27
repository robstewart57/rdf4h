{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving #-}
-- |A simple graph implementation backed by 'Data.HashMap'.

module Data.RDF.MGraph(MGraph, empty, mkRdf, triplesOf, uniqTriplesOf, select, query)

where

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

-- |A map-based graph implementation.
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
newtype MGraph = MGraph (TMaps, Maybe BaseUrl, PrefixMappings)
                 deriving (NFData)

instance RDF MGraph where
  baseUrl           = baseUrl'
  prefixMappings    = prefixMappings'
  addPrefixMappings = addPrefixMappings'
  empty             = empty'
  mkRdf             = mkRdf'
  triplesOf         = triplesOf'
  uniqTriplesOf     = uniqTriplesOf'
  select            = select'
  query             = query'

instance Show MGraph where
  show gr = concatMap (\t -> show t ++ "\n")  (triplesOf gr)

-- some convenience type alias for readability

-- An adjacency map for a subject, mapping from a predicate node to
-- to the adjacent nodes via that predicate.
type AdjacencyMap = HashMap Predicate (HashSet Node)

type Adjacencies = HashSet Node

type TMap   = HashMap Node AdjacencyMap
type TMaps  = (TMap, TMap)


baseUrl' :: MGraph -> Maybe BaseUrl
baseUrl' (MGraph (_, baseURL, _)) = baseURL

prefixMappings' :: MGraph -> PrefixMappings
prefixMappings' (MGraph (_, _, pms)) = pms

addPrefixMappings' :: MGraph -> PrefixMappings -> Bool -> MGraph
addPrefixMappings' (MGraph (ts, baseURL, pms)) pms' replace = 
  let merge = if replace then flip mergePrefixMappings else mergePrefixMappings
  in  MGraph (ts, baseURL, merge pms pms')

empty' :: MGraph
empty' = MGraph ((HashMap.empty, HashMap.empty), Nothing, PrefixMappings Map.empty)

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> MGraph
mkRdf' ts baseURL pms = MGraph (mergeTs (HashMap.empty, HashMap.empty) ts, baseURL, pms)

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
triplesOf' :: MGraph -> Triples
triplesOf' (MGraph ((spoMap, _), _, _)) = concatMap (uncurry tripsSubj) subjPredMaps
  where subjPredMaps = HashMap.toList spoMap

-- naive implementation for now
uniqTriplesOf' :: MGraph -> Triples
uniqTriplesOf' = nub . expandTriples

tripsSubj :: Subject -> AdjacencyMap -> Triples
tripsSubj s adjMap = concatMap (uncurry (tfsp s)) (HashMap.toList adjMap)
  where tfsp = tripsForSubjPred

tripsForSubjPred :: Subject -> Predicate -> Adjacencies -> Triples
tripsForSubjPred s p adjs = map (Triple s p) (Set.toList adjs)

-- supports select
select' :: MGraph -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' (MGraph ((spoMap,_),_,_)) subjFn predFn objFn =
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
query' :: MGraph -> Maybe Node -> Maybe Predicate -> Maybe Node -> Triples
query' (MGraph (m,_ , _)) subj pred obj = map f $ Set.toList $ q1 subj pred obj m
  where f (s, p, o) = Triple s p o

q1 :: Maybe Node -> Maybe Node -> Maybe Node -> TMaps -> HashSet (Node, Node, Node)
q1 (Just s) p o        (spoMap, _     ) = q2 p o (s, HashMap.lookupDefault HashMap.empty s spoMap)
q1 s        p (Just o) (_     , opsMap) = Set.map (\(o',p',s') -> (s',p',o')) $ q2 p s (o, HashMap.lookupDefault HashMap.empty o opsMap)
q1 Nothing  p o        (spoMap, _     ) = Set.unions $ map (q2 p o) $ HashMap.toList spoMap

q2 :: Maybe Node -> Maybe Node -> (Node, HashMap Node (HashSet Node)) -> HashSet (Node, Node, Node)
q2 (Just p) o (s, pmap) =
  maybe Set.empty (Set.map (\ (p', o') -> (s, p', o')) . q3 o . (p,)) $ HashMap.lookup p pmap
q2 Nothing o (s, pmap) = Set.map (\(x,y) -> (s,x,y)) $ Set.unions $ map (q3 o) opmaps
  where opmaps ::[(Node, HashSet Node)]
        opmaps = HashMap.toList pmap

q3 :: Maybe Node -> (Node, HashSet Node) -> HashSet (Node, Node)
q3 (Just o) (p, os) = if o `Set.member` os then Set.singleton (p, o) else Set.empty
q3 Nothing  (p, os) = Set.map (\o -> (p, o)) os
