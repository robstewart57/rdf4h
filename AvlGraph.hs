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
   empty     = empty'
   mkGraph   = mkGraph'
   triplesOf = triplesOf'
   select    = select'

empty' :: AvlGraph
empty' = AvlGraph Map.empty

mkGraph' :: [Triple] -> AvlGraph
mkGraph' ts = AvlGraph (f' Map.empty ts)

f' :: SPOMap -> [Triple] -> SPOMap
f' m []    = m
f' m (t:ts) = f' (f m t) ts

f :: SPOMap -> Triple -> SPOMap
f m t = mergeSPO m s p o
  where 
    s = subjectOf t
    p = predicateOf t
    o = objectOf t

mergeObject :: AdjacencyMap -> Predicate -> Object -> AdjacencyMap
mergeObject m p o = 
  case p `Map.member` m of
    False -> Map.insert p (Set.singleton o) m
    True  -> Map.insert p (Set.insert o (Map.findWithDefault undefined p m)) m

mergeSPO :: SPOMap ->
            Subject -> Predicate -> Object -> 
            SPOMap
mergeSPO m s p o = 
  case s `Map.member` m of
    False -> Map.insert s (Map.singleton p (Set.singleton o)) m
    True  -> let adjs = Map.findWithDefault (error "") s m
             in case p `Map.member` adjs of
                  False -> Map.insert s (Map.insert p (Set.singleton o) adjs) m
                  True  -> Map.insert s (Map.insert p (Set.insert o (Map.findWithDefault undefined p adjs)) adjs) m

triplesOf' :: AvlGraph -> [Triple]
triplesOf' (AvlGraph spoMap) = concatMap (uncurry tripsSubj) (Map.toList spoMap)

tripsSubj :: Subject -> AdjacencyMap -> [Triple]
tripsSubj s adjMap = concatMap (\(pred, objs) -> 
                       triplesForSubjectPredicate s pred objs) (Map.toList adjMap)

triplesForSubjectPredicate :: Subject -> Predicate -> Adjacencies -> [Triple]
triplesForSubjectPredicate s p adjs = map (triple s p) (Set.elems adjs)

select' :: Selector -> AvlGraph -> [Triple]
select' sltr gr@(AvlGraph spoMap) = filter (match sltr) ts
  where match sltr t = sltr (subjectOf t) (predicateOf t) (objectOf t)
        ts = triplesOf' gr

subjectMatches :: Subject -> (Subject, AdjacencyMap) -> Bool
subjectMatches subj (s, m) = subj == s

adjacencyMapsForSubject :: Subject -> SPOMap -> [AdjacencyMap]
adjacencyMapsForSubject subj m = map snd $ filter (\(x,y) -> x == subj) (Map.toList m)

adjacenciesForPredicate :: Predicate -> AdjacencyMap -> Adjacencies
adjacenciesForPredicate = Map.findWithDefault Set.empty

adjacenciesForSubjectPredicate :: Subject -> Predicate -> SPOMap -> Adjacencies
adjacenciesForSubjectPredicate subj pred spoMap = 
  let adjMaps = adjacencyMapsForSubject subj spoMap
      adjSets = map (adjacenciesForPredicate pred) adjMaps
  in foldl' Set.union Set.empty adjSets
