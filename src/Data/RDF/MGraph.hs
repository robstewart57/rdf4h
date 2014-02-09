{-# LANGUAGE TupleSections #-}
-- |A simple graph implementation backed by 'Data.Map'.

module Data.RDF.MGraph(MGraph, empty, mkRdf, triplesOf, select, query)

where

import Prelude hiding (pred)
import Data.RDF.Types
import Data.RDF.Query
import Data.RDF.Namespace
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
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

instance RDF MGraph where
  baseUrl           = baseUrl'
  prefixMappings    = prefixMappings'
  addPrefixMappings = addPrefixMappings'
  empty             = empty'
  mkRdf             = mkRdf'
  triplesOf         = triplesOf'
  select            = select'
  query             = query'

-- some convenience type alias for readability

-- An adjacency map for a subject, mapping from a predicate node to
-- to the adjacent nodes via that predicate.
type AdjacencyMap = Map Predicate (Set Node)

type Adjacencies = Set Node

type TMap   = Map Node AdjacencyMap
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
empty' = MGraph ((Map.empty, Map.empty), Nothing, PrefixMappings Map.empty)

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> MGraph
mkRdf' ts baseURL pms = MGraph (mergeTs (Map.empty, Map.empty) ts, baseURL, pms)

mergeTs :: TMaps -> [Triple] -> TMaps
mergeTs = foldl' mergeT
  where
    mergeT :: TMaps -> Triple -> TMaps
    mergeT m t = mergeT' m (subjectOf t) (predicateOf t) (objectOf t)

mergeT' :: TMaps -> Subject -> Predicate -> Object -> TMaps
mergeT' (spo, ops) s p o = (mergeT'' spo s p o, mergeT'' ops o p s)

mergeT'' :: TMap -> Subject -> Predicate -> Object -> TMap
mergeT'' m s p o =
  if s `Map.member` m then
    (if p `Map.member` adjs then Map.insert s addPredObj m
       else Map.insert s addNewPredObjMap m)
    else Map.insert s newPredMap m
  where
    adjs = get s m
    newPredMap :: Map Predicate (Set Object)
    newPredMap = Map.singleton p (Set.singleton o)
    addNewPredObjMap :: Map Predicate (Set Object)
    addNewPredObjMap = Map.insert p (Set.singleton o) adjs
    addPredObj :: Map Predicate (Set Object)
    addPredObj = Map.insert p (Set.insert o (get p adjs)) adjs
    get :: Ord k => k -> Map k v -> v
    get = Map.findWithDefault undefined

-- 3 following functions support triplesOf
triplesOf' :: MGraph -> Triples
triplesOf' (MGraph ((spoMap, _), _, _)) = concatMap (uncurry tripsSubj) subjPredMaps
  where subjPredMaps = Map.toList spoMap

tripsSubj :: Subject -> AdjacencyMap -> Triples
tripsSubj s adjMap = concatMap (uncurry (tfsp s)) (Map.toList adjMap)
  where tfsp = tripsForSubjPred

tripsForSubjPred :: Subject -> Predicate -> Adjacencies -> Triples
tripsForSubjPred s p adjs = map (Triple s p) (Set.elems adjs)

-- supports select
select' :: MGraph -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' (MGraph ((spoMap,_),_,_)) subjFn predFn objFn =
  map (\(s,p,o) -> Triple s p o) $ Set.toList $ sel1 subjFn predFn objFn spoMap

sel1 :: NodeSelector -> NodeSelector -> NodeSelector -> TMap -> Set (Node, Node, Node)
sel1 (Just subjFn) p o spoMap =
  Set.unions $ map (sel2 p o) $ filter (\(x,_) -> subjFn x) $ Map.toList spoMap
sel1 Nothing p o spoMap = Set.unions $ map (sel2 p o) $ Map.toList spoMap

sel2 :: NodeSelector -> NodeSelector -> (Node, Map Node (Set Node)) -> Set (Node, Node, Node)
sel2 (Just predFn) mobjFn (s, ps) =
  Set.map (\(p,o) -> (s,p,o)) $
  foldl' Set.union Set.empty $
  map (sel3 mobjFn) poMapS :: Set (Node, Node, Node)
  where
    poMapS :: [(Node, Set Node)]
    poMapS = filter (\(k,_) -> predFn k) $ Map.toList ps
sel2 Nothing mobjFn (s, ps) =
  Set.map (\(p,o) -> (s,p,o)) $
  foldl' Set.union Set.empty $
  map (sel3 mobjFn) poMaps
  where
    poMaps = Map.toList ps

sel3 :: NodeSelector -> (Node, Set Node) -> Set (Node, Node)
sel3 (Just objFn) (p, os) = Set.map (\o -> (p, o)) $ Set.filter objFn os
sel3 Nothing      (p, os) = Set.map (\o -> (p, o)) os

-- support query
query' :: MGraph -> Maybe Node -> Maybe Predicate -> Maybe Node -> Triples
query' (MGraph (m,_ , _)) subj pred obj = map f $ Set.toList $ q1 subj pred obj m
  where f (s, p, o) = Triple s p o

q1 :: Maybe Node -> Maybe Node -> Maybe Node -> TMaps -> Set (Node, Node, Node)
q1 (Just s) p o        (spoMap, _     ) = q2 p o (s, Map.findWithDefault Map.empty s spoMap)
q1 s        p (Just o) (_     , opsMap) = Set.map (\(o',p',s') -> (s',p',o')) $ q2 p s (o, Map.findWithDefault Map.empty o opsMap)
q1 Nothing  p o        (spoMap, _     ) = Set.unions $ map (q2 p o) $ Map.toList spoMap

q2 :: Maybe Node -> Maybe Node -> (Node, Map Node (Set Node)) -> Set (Node, Node, Node)
q2 (Just p) o (s, pmap) =
  maybe Set.empty (Set.map (\ (p', o') -> (s, p', o')) . q3 o . (p,)) $ Map.lookup p pmap
q2 Nothing o (s, pmap) = Set.map (\(x,y) -> (s,x,y)) $ Set.unions $ map (q3 o) opmaps
  where opmaps ::[(Node, Set Node)]
        opmaps = Map.toList pmap

q3 :: Maybe Node -> (Node, Set Node) -> Set (Node, Node)
q3 (Just o) (p, os) = if o `Set.member` os then Set.singleton (p, o) else Set.empty
q3 Nothing  (p, os) = Set.map (\o -> (p, o)) os
