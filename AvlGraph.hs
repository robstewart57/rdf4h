-- |"AvlGraph" contains a graph implementation backed by a 'Data.Map.AVL'.
module AvlGraph(AvlGraph,
                empty, mkGraph, triplesOf, select, query)

where

import RDF

import Data.Map.AVL (Map)
import qualified Data.Map.AVL as Map

import Data.Set.AVL (Set)
import qualified Data.Set.AVL as Set

import Data.List

-- |An AVL map-based graph implementation.
--
-- The time complexity of the functions of an 'AvlGraph', relative
-- to the number of triples unless otherwise specified, are:
--
--  * 'empty'    : O(1)
--
--  * 'mkGraph'  : O(n)
--
--  * 'triplesOf': O(n)
--
--  * 'select'   : ?
--
--  * 'query'    : O(log n)
newtype AvlGraph = AvlGraph SPOMap

instance Graph AvlGraph where
  empty          = AvlGraph Map.empty
  mkGraph ts     = AvlGraph (mergeTs Map.empty ts)
  triplesOf      = triplesOf'
  select         = select'
  query          = query'

-- The adjacencies of a node is a map of predicate nodes
-- to adjacent nodes via that predicate.
type AdjacencyMap = Map Predicate Adjacencies

type Adjacencies = Set Object
type SPOMap    = Map Subject AdjacencyMap


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
triplesOf' :: AvlGraph -> [Triple]
triplesOf' (AvlGraph spoMap) = concatMap (uncurry tripsSubj) subjPredMaps
  where subjPredMaps = Map.toList spoMap

tripsSubj :: Subject -> AdjacencyMap -> [Triple]
tripsSubj s adjMap = concatMap (uncurry (tfsp s)) (Map.toList adjMap)
  where tfsp = tripsForSubjPred

tripsForSubjPred :: Subject -> Predicate -> Adjacencies -> [Triple]
tripsForSubjPred s p adjs = map (triple s p) (Set.elems adjs)

-- supports select
select' :: Selector -> AvlGraph -> [Triple]
select' sltr gr = filter (match sltr) ts
  where match sltr t = sltr (subjectOf t) (predicateOf t) (objectOf t)
        ts = triplesOf' gr

-- support query
query' :: AvlGraph -> Maybe Node -> Maybe Predicate -> Maybe Node -> [Triple]
query' (AvlGraph spoMap) subj pred obj = map f $ Set.toList $ q1 subj pred obj spoMap
  where f (s, p, o) = triple s p o
{-
subj1 -> pred1 -> obj1, obj2, obj3
         pred2 -> obj1, obj4
subj2 -> pred1 -> obj3, obj5
         pred3 -> obj6, obj7
-}

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
