module AvlGraph(AvlGraph)

where

import RDF

import Data.Map.AVL (Map)
import qualified Data.Map.AVL as Map

import Data.Set.AVL (Set)
import qualified Data.Set.AVL as Set

import Data.List

-- |The adjacencies of a node is a map of predicate nodes
-- to adjacent nodes via that predicate.
type AdjacencyMap = Map Predicate Adjacencies

type Adjacencies = Set Object
type Subject   = Node
type Predicate = Node
type Object    = Node
type SPOMap    = Map Subject AdjacencyMap

-- |An AVL map-based graph implementation.
data AvlGraph = AvlGraph SPOMap

instance Graph AvlGraph where
  empty          = empty'
  mkGraph        = mkGraph'
  triplesOf      = triplesOf'
  select         = select'
  querySubj      = querySubj'
  querySubjPred  = querySubjPred'
 
empty' :: AvlGraph
empty' = AvlGraph Map.empty

mkGraph' :: [Triple] -> AvlGraph
mkGraph' ts = AvlGraph (addTriples Map.empty ts)

addTriples :: SPOMap -> [Triple] -> SPOMap
addTriples = foldl' addTriple
  where
    addTriple :: SPOMap -> Triple -> SPOMap
    addTriple m t = mergeSPO m (subjectOf t) (predicateOf t) (objectOf t)

mergeObject :: AdjacencyMap -> Predicate -> Object -> AdjacencyMap
mergeObject m p o = 
  case p `Map.member` m of
    False -> Map.insert p (Set.singleton o) m
    True  -> Map.insert p (Set.insert o (get p m)) m

mergeSPO :: SPOMap ->
            Subject -> Predicate -> Object -> 
            SPOMap
mergeSPO m s p o = 
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
    
    
 -- |Get map value for key, failing if there is no such key in map.
get :: Ord k => k -> Map k v -> v
get = Map.findWithDefault undefined
    

triplesOf' :: AvlGraph -> [Triple]
triplesOf' (AvlGraph spoMap) = concatMap (uncurry tripsSubj) subjPredMaps
  where subjPredMaps = Map.toList spoMap

tripsSubj :: Subject -> AdjacencyMap -> [Triple]
tripsSubj s adjMap = concatMap (uncurry (tfsp s)) (Map.toList adjMap)
  where tfsp = tripsForSubjPred

tripsForSubjPred :: Subject -> Predicate -> Adjacencies -> [Triple]
tripsForSubjPred s p adjs = map (triple s p) (Set.elems adjs)

select' :: Selector -> AvlGraph -> [Triple]
select' sltr gr@(AvlGraph spoMap) = filter (match sltr) ts
  where match sltr t = sltr (subjectOf t) (predicateOf t) (objectOf t)
        ts = triplesOf' gr

adjsForSubjPred :: Subject -> Predicate -> SPOMap -> Adjacencies
adjsForSubjPred subj pred spoMap = 
  let adjMaps = adjMapsForSubj subj spoMap
      adjSets = map (adjsForPred pred) adjMaps
  in foldl' Set.union Set.empty adjSets

adjMapsForSubj :: Subject -> SPOMap -> [AdjacencyMap]
adjMapsForSubj subj = map snd . filter' . Map.toList
  where filter' = filter ((==) subj . fst)

adjsForPred :: Predicate -> AdjacencyMap -> Adjacencies
adjsForPred = Map.findWithDefault Set.empty

querySubj' :: AvlGraph -> Subject -> [Triple]
querySubj' (AvlGraph spoMap) subj = makeTriples subj predAssocList
  where 
    predMaps :: [Map Predicate Adjacencies]
    predMaps = adjMapsForSubj subj spoMap
    predAssocList :: [(Predicate, Adjacencies)]
    predAssocList = concatAssocList predMaps
  
makeTriples :: Subject -> [(Predicate, Adjacencies)] -> [Triple]
makeTriples s = concatMap (\(p, adjs) -> makeTriples' s p (Set.toList adjs))
  where
    makeTriples' :: Subject -> Predicate -> [Object] -> [Triple]
    makeTriples' s p objs = map (\o -> triple s p o) objs

concatAssocList :: [Map Predicate Adjacencies] -> [(Predicate, Adjacencies)]
concatAssocList = concatMap Map.toList

mapSubjPred :: Subject -> Predicate -> Adjacencies -> [Triple]
mapSubjPred s p adjs = map (triple s p) (Set.toList adjs)

querySubjPred' :: AvlGraph -> Subject -> Predicate -> [Triple]
querySubjPred' (AvlGraph spoMap) subj pred = map convert adjacencies
  where
    adjacencies = Set.toList (adjsForSubjPred subj pred spoMap)
    convert     = triple subj pred