-- |"MGraph" contains a graph implementation backed by a 'Data.Map'.
module Text.RDF.MGraph(MGraph, empty, mkGraph, triplesOf, select, query)

where

import Text.RDF.Core
import Text.RDF.Namespace

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
--  * 'mkGraph'  : O(n)
--
--  * 'triplesOf': O(n)
--
--  * 'select'   : O(n)
--
--  * 'query'    : O(log n)
newtype MGraph = MGraph (SPOMap, Maybe BaseUrl, PrefixMappings)

instance Graph MGraph where
  baseUrl         = baseUrl'
  prefixMappings  = prefixMappings'
  empty           = empty'
  mkGraph         = mkGraph'
  triplesOf       = triplesOf'
  select          = select'
  query           = query'

-- some convenience type alias for readability

-- An adjacency map for a subject, mapping from a predicate node to
-- to the adjacent nodes via that predicate.
type AdjacencyMap = Map Predicate Adjacencies

-- 
type Adjacencies = Set Object
type SPOMap    = Map Subject AdjacencyMap

baseUrl' :: MGraph -> Maybe BaseUrl
baseUrl' (MGraph (_, baseUrl, _)) = baseUrl

prefixMappings' :: MGraph -> PrefixMappings
prefixMappings' (MGraph (_, _, pms)) = pms

empty' :: MGraph 
empty' = MGraph (Map.empty, Nothing, PrefixMappings Map.empty)

mkGraph' :: Triples -> Maybe BaseUrl -> PrefixMappings -> MGraph
mkGraph' ts baseUrl pms = MGraph ((mergeTs Map.empty ts), baseUrl, pms)

mergeTs :: SPOMap -> [Triple] -> SPOMap
mergeTs = foldl' mergeT
  where
    mergeT :: SPOMap -> Triple -> SPOMap
    mergeT m t = mergeT' m (subjectOf t) (predicateOf t) (objectOf t)

mergeT' :: SPOMap -> Subject -> Predicate -> Object -> SPOMap
mergeT' m s p o = 
  case s `Map.member` m of
    False -> Map.insert s (newPredMap p o) m
    True  -> case p `Map.member` adjs of
               False -> Map.insert s (addNewPredObjMap p o adjs) m
               True  -> Map.insert s (addPredObj p o adjs) m
  where
    adjs = get s m
    newPredMap :: Predicate -> Object -> Map Predicate (Set Object)
    newPredMap p o = Map.singleton p (Set.singleton o)
    addNewPredObjMap :: Predicate -> Object -> Map Predicate (Set Object) ->
                       Map Predicate (Set Object)
    addNewPredObjMap p o = Map.insert p (Set.singleton o)
    addPredObj :: Predicate -> Object -> Map Predicate (Set Object) ->
                    Map Predicate (Set Object)
    addPredObj p o = Map.insert p (Set.insert o (get p adjs))
    get :: Ord k => k -> Map k v -> v
    get = Map.findWithDefault undefined
    
-- 3 following functions support triplesOf
triplesOf' :: MGraph -> Triples
triplesOf' (MGraph (spoMap, _, _)) = concatMap (uncurry tripsSubj) subjPredMaps
  where subjPredMaps = Map.toList spoMap

tripsSubj :: Subject -> AdjacencyMap -> Triples
tripsSubj s adjMap = concatMap (uncurry (tfsp s)) (Map.toList adjMap)
  where tfsp = tripsForSubjPred

tripsForSubjPred :: Subject -> Predicate -> Adjacencies -> Triples
tripsForSubjPred s p adjs = map (triple s p) (Set.elems adjs)

-- supports select
select' :: MGraph -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' (MGraph (spoMap,_,_)) subjFn predFn objFn = 
  map (\(s,p,o) -> triple s p o) $ Set.toList $ sel1 subjFn predFn objFn spoMap

sel1 :: NodeSelector -> NodeSelector -> NodeSelector -> SPOMap -> Set (Node, Node, Node)
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
query' (MGraph (spoMap,_ , _)) subj pred obj = map f $ Set.toList $ q1 subj pred obj spoMap
  where f (s, p, o) = triple s p o

q1 :: Maybe Node -> Maybe Node -> Maybe Node -> SPOMap -> Set (Node, Node, Node)
q1 (Just s) p o spoMap = q2 p o (s, Map.findWithDefault Map.empty s spoMap)
q1 Nothing  p o spoMap = Set.unions $ map (q2 p o) $ Map.toList spoMap

q2 :: Maybe Node -> Maybe Node -> (Node, Map Node (Set Node)) -> Set (Node, Node, Node)
q2 (Just p) o (s, pmap) = 
  case p `Map.member` pmap of
    False  -> Set.empty
    True   -> Set.map (\(p', o') -> (s, p', o')) $ 
                q3 o (p, Map.findWithDefault undefined p pmap)
q2 Nothing o (s, pmap) = Set.map (\(x,y) -> (s,x,y)) $ Set.unions $ map (q3 o) opmaps
  where opmaps ::[(Node, Set Node)]
        opmaps = Map.toList pmap
        
q3 :: Maybe Node -> (Node, Set Node) -> Set (Node, Node)
q3 (Just o) (p, os) = if o `Set.member` os then Set.singleton (p, o) else Set.empty
q3 Nothing  (p, os) = Set.map (\o -> (p, o)) os
