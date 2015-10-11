{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, BangPatterns #-}

module Data.RDF.PatriciaTreeGraph where

import Data.RDF.Namespace
import Data.RDF.Query
import Data.RDF.Types

import Control.DeepSeq (NFData(rnf))
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as PT
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Data.IntMap as IntMap
import Data.List
import qualified Data.Map as Map
import Data.Maybe

newtype PatriciaTreeGraph = PatriciaTreeGraph (PT.Gr Node Node,IntMap.IntMap Node, Maybe BaseUrl, PrefixMappings)
                            deriving (Show)

instance NFData (PT.Gr Node Node)
  where rnf x = seq x ()

instance RDF PatriciaTreeGraph where
  baseUrl           = baseUrl'
  prefixMappings    = prefixMappings'
  addPrefixMappings = addPrefixMappings'
  empty             = empty'
  mkRdf             = mkRdf'
  triplesOf         = triplesOf'
  uniqTriplesOf     = uniqTriplesOf'
  select            = select'
  query             = query'

empty' :: PatriciaTreeGraph
empty' = PatriciaTreeGraph (G.empty,IntMap.empty, Nothing, PrefixMappings Map.empty)

prefixMappings' :: PatriciaTreeGraph -> PrefixMappings
prefixMappings' (PatriciaTreeGraph (_,_,_,pms')) = pms'

addPrefixMappings' :: PatriciaTreeGraph -> PrefixMappings -> Bool -> PatriciaTreeGraph
addPrefixMappings' (PatriciaTreeGraph (g, idxLookup, baseURL, pms)) pms' replace =
  let merge = if replace then flip mergePrefixMappings else mergePrefixMappings
  in  PatriciaTreeGraph (g, idxLookup, baseURL, merge pms pms')

baseUrl' :: PatriciaTreeGraph -> Maybe BaseUrl
baseUrl' (PatriciaTreeGraph _) = Nothing

data AutoIncrMap = AutoIncrMap
    { theMap :: Map.Map Node (Int,Node)
    , idxPtr :: !Int }

{-
init       -> []
insert www -> [0,www]
insert ttt -> [1,ttt]
insert ttt -> [1,ttt]
-}
insertIncr :: Node -> AutoIncrMap -> (Int,AutoIncrMap)
insertIncr !node mp =
    let x = Map.lookup node (theMap mp)
    in if isJust x
       then
           let (i,_) = fromJust x
           in (i,mp)
       else
           let curIdx = idxPtr mp
               mp' = mp { idxPtr = curIdx + 1
                        , theMap = Map.insert node (idxPtr mp, node) (theMap mp) }
           in (curIdx, mp')

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> PatriciaTreeGraph
mkRdf' ts base' pms' =
    -- step 1: subjects and objects assigned node ID (an Int)
    let (mp,ledges) = foldl' f ((AutoIncrMap Map.empty 0),[]) ts

        -- step 2: for all triples, create arc with
        --         - predicate node the arc label
        --         - subject the source node ID
        --         - object the target node ID
        f (mp',edges) (Triple s p o) =
            let (sIdx,mp'')  = insertIncr s mp'
                (oIdx,mp''') = insertIncr o mp''
                edge = (sIdx,oIdx,p)
            in (mp''',edge : edges)

        lnodes = Map.elems (theMap mp)
        intIdx = IntMap.fromList lnodes
        ptGraph = G.mkGraph lnodes ledges

    in PatriciaTreeGraph (ptGraph ,intIdx, base', pms')

triplesOf' :: PatriciaTreeGraph -> Triples
triplesOf' (PatriciaTreeGraph (g,idxLookup,_,_)) =
    map (\(sIdx,oIdx,p) ->
             let [s,o] = map (\idx -> fromJust $ IntMap.lookup idx idxLookup) [sIdx,oIdx]
             in Triple s p o) (G.labEdges g)

uniqTriplesOf' :: PatriciaTreeGraph -> Triples
uniqTriplesOf' ptG@(PatriciaTreeGraph (g,idxLookup,_,_)) =
    nub $ map (\(sIdx,oIdx,p) ->
             let [s,o] = map (\idx -> fromJust $ IntMap.lookup idx idxLookup) [sIdx,oIdx]
             in expandTriple (prefixMappings ptG) (Triple s p o)) (G.labEdges g)

mkTriples :: IntMap.IntMap Node -> Node -> [(Node, IntMap.Key)] -> [(Node, IntMap.Key)] ->  [Triple]
mkTriples idxLookup thisNode adjsIn adjsOut =
    let ts1 = map (\(predNode,subjIdx) ->
                   let s = fromJust (IntMap.lookup subjIdx idxLookup)
                   in Triple s predNode thisNode
                  )  adjsIn

        ts2 = map (\(predNode,objIdx) ->
                       let o = fromJust (IntMap.lookup objIdx idxLookup)
                       in Triple thisNode predNode o
                  ) adjsOut
    in ts1 ++ ts2

select' :: PatriciaTreeGraph -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' (PatriciaTreeGraph (g,idxLookup,_,_)) maybeSubjSel maybePredSel maybeObjSel =

    let cfun ( adjsIn , _nodeIdx , thisNode , adjsOut )
            | isNothing  maybeSubjSel && isNothing maybePredSel && isNothing maybeObjSel =
                         mkTriples idxLookup thisNode adjsIn adjsOut

            | isJust maybeSubjSel && isNothing maybePredSel && isNothing maybeObjSel =
                       let adjsIn' = filter (\(_p,idxSubj) -> fromJust maybeSubjSel (fromJust (IntMap.lookup idxSubj idxLookup))) adjsIn
                           ts1 = mkTriples idxLookup thisNode adjsIn' []
                           ts2 = if fromJust maybeSubjSel thisNode
                                 then mkTriples idxLookup thisNode [] adjsOut
                                 else []
                       in ts1 ++ ts2
            | isNothing maybeSubjSel && isJust maybePredSel && isNothing maybeObjSel =
                       let adjsIn'  = filter (\(p,_idxSubj) -> fromJust maybePredSel p ) adjsIn
                           adjsOut' = filter (\(p,_idxObj) -> fromJust maybePredSel p ) adjsOut
                           ts1 = if not (null adjsIn')
                                 then mkTriples idxLookup thisNode adjsIn' []
                                 else []
                           ts2 = if not (null adjsOut')
                                 then mkTriples idxLookup thisNode [] adjsOut'
                                 else []
                       in ts1 ++ ts2

            | isNothing maybeSubjSel && isNothing maybePredSel && isJust maybeObjSel =
                       let adjsOut' = filter (\(_p,idxObj) -> fromJust maybeObjSel (fromJust (IntMap.lookup idxObj idxLookup)) ) adjsOut
                           ts1 = mkTriples idxLookup thisNode [] adjsOut'
                           ts2 = if fromJust maybeObjSel thisNode
                                 then mkTriples idxLookup thisNode adjsIn []
                                 else []
                       in ts1 ++ ts2

            | isJust maybeSubjSel && isJust maybePredSel && isNothing maybeObjSel =
                       let adjsIn' = filter (\(p,idxSubj) -> fromJust maybeSubjSel (fromJust (IntMap.lookup idxSubj idxLookup))
                                                            && fromJust maybePredSel p ) adjsIn
                           adjsOut' = filter (\(p,_idxObj) -> fromJust maybePredSel p ) adjsOut
                           ts1 = mkTriples idxLookup thisNode adjsIn' []
                           ts2 = if fromJust maybeSubjSel thisNode
                                 then mkTriples idxLookup thisNode [] adjsOut'
                                 else []
                       in ts1 ++ ts2

            | isJust maybeSubjSel && isNothing maybePredSel && isJust maybeObjSel =
                       let adjsIn' = filter (\(_p,idxSubj) -> fromJust maybeSubjSel (fromJust (IntMap.lookup idxSubj idxLookup)) ) adjsIn
                           adjsOut' = filter (\(_p,idxObj) -> fromJust maybeObjSel  (fromJust (IntMap.lookup idxObj idxLookup))  ) adjsOut
                           ts1 = if fromJust maybeObjSel thisNode
                                 then mkTriples idxLookup thisNode adjsIn' []
                                 else []
                           ts2 = if fromJust maybeSubjSel thisNode
                                 then mkTriples idxLookup thisNode [] adjsOut'
                                 else []
                       in ts1 ++ ts2

            | isNothing maybeSubjSel && isJust maybePredSel && isJust maybeObjSel =
                       let adjsIn' = filter (\(p,_idxSubj) -> fromJust maybePredSel p ) adjsIn
                           adjsOut' = filter (\(p,idxObj) -> fromJust maybeObjSel (fromJust (IntMap.lookup idxObj idxLookup))
                                                            && fromJust maybePredSel p ) adjsOut
                           ts1 = if fromJust maybeObjSel thisNode
                                 then mkTriples idxLookup thisNode adjsIn' []
                                 else []
                           ts2 = mkTriples idxLookup thisNode [] adjsOut'
                       in ts1 ++ ts2

            | isJust maybeSubjSel && isJust maybePredSel && isJust maybeObjSel =
                       let adjsIn' = filter (\(p,idxSubj) -> fromJust maybeSubjSel (fromJust (IntMap.lookup idxSubj idxLookup))
                                                            && fromJust maybePredSel p ) adjsIn
                           adjsOut' = filter (\(p,idxObj) -> fromJust maybeObjSel (fromJust (IntMap.lookup idxObj idxLookup))
                                                            && fromJust maybePredSel p ) adjsOut
                           ts1 = if fromJust maybeObjSel thisNode
                                 then mkTriples idxLookup thisNode adjsIn' []
                                 else []
                           ts2 = if fromJust maybeSubjSel thisNode
                                 then mkTriples idxLookup thisNode [] adjsOut'
                                 else []
                       in ts1 ++ ts2

        cfun ( _ , _ , _ , _) = undefined -- not sure why this pattern is needed to exhaust cfun arg patterns

    in concat $ DFS.dfsWith' cfun g

query' :: PatriciaTreeGraph -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
query' (PatriciaTreeGraph (g,idxLookup,_,_)) maybeSubj maybePred maybeObj =

    let cfun ( adjsIn , _nodeIdx , thisNode , adjsOut )
            | isNothing  maybeSubj && isNothing maybePred && isNothing maybeObj =
                       mkTriples idxLookup thisNode adjsIn adjsOut

            | isJust maybeSubj && isNothing maybePred && isNothing maybeObj =
                       let adjsIn' = filter (\(_p,idxSubj) -> fromJust (IntMap.lookup idxSubj idxLookup) == fromJust maybeSubj ) adjsIn
                           ts1 = mkTriples idxLookup thisNode adjsIn' []
                           ts2 = if thisNode == fromJust maybeSubj
                                 then mkTriples idxLookup thisNode [] adjsOut
                                 else []
                       in ts1 ++ ts2

            | isNothing maybeSubj && isJust maybePred && isNothing maybeObj =
                       let adjsIn'  = filter (\(p,_idxSubj) -> p == fromJust maybePred ) adjsIn
                           adjsOut' = filter (\(p,_idxObj) -> p  == fromJust maybePred ) adjsOut
                           ts1 = if not (null adjsIn')
                                 then mkTriples idxLookup thisNode adjsIn' []
                                 else []
                           ts2 = if not (null adjsOut')
                                 then mkTriples idxLookup thisNode [] adjsOut'
                                 else []
                       in ts1 ++ ts2

            | isNothing maybeSubj && isNothing maybePred && isJust maybeObj =
                       let adjsOut' = filter (\(_p,idxObj) -> fromJust (IntMap.lookup idxObj idxLookup) == fromJust maybeObj ) adjsOut
                           ts1 = mkTriples idxLookup thisNode [] adjsOut'
                           ts2 = if thisNode == fromJust maybeObj
                                 then mkTriples idxLookup thisNode adjsIn []
                                 else []
                       in ts1 ++ ts2

            | isJust maybeSubj && isJust maybePred && isNothing maybeObj =
                       let adjsIn' = filter (\(p,idxSubj) -> fromJust (IntMap.lookup idxSubj idxLookup) == fromJust maybeSubj
                                                            && p  == fromJust maybePred ) adjsIn
                           adjsOut' = filter (\(p,_idxObj) -> p  == fromJust maybePred ) adjsOut
                           ts1 = mkTriples idxLookup thisNode adjsIn' []
                           ts2 = if thisNode == fromJust maybeSubj
                                 then mkTriples idxLookup thisNode [] adjsOut'
                                 else []
                       in ts1 ++ ts2

            | isJust maybeSubj && isNothing maybePred && isJust maybeObj =
                       let adjsIn' = filter (\(_p,idxSubj) -> fromJust (IntMap.lookup idxSubj idxLookup) == fromJust maybeSubj ) adjsIn
                           adjsOut' = filter (\(_p,idxObj) -> fromJust (IntMap.lookup idxObj idxLookup) == fromJust maybeObj ) adjsOut
                           ts1 = if thisNode == fromJust maybeObj
                                 then mkTriples idxLookup thisNode adjsIn' []
                                 else []
                           ts2 = if thisNode == fromJust maybeSubj
                                 then mkTriples idxLookup thisNode [] adjsOut'
                                 else []
                       in ts1 ++ ts2

            | isNothing maybeSubj && isJust maybePred && isJust maybeObj =
                       let adjsIn' = filter (\(p,_idxSubj) -> p  == fromJust maybePred ) adjsIn
                           adjsOut' = filter (\(p,idxObj) -> fromJust (IntMap.lookup idxObj idxLookup) == fromJust maybeObj
                                                            && p  == fromJust maybePred ) adjsOut
                           ts1 = if thisNode == fromJust maybeObj
                                 then mkTriples idxLookup thisNode adjsIn' []
                                 else []
                           ts2 = mkTriples idxLookup thisNode [] adjsOut'
                       in ts1 ++ ts2

            | isJust maybeSubj && isJust maybePred && isJust maybeObj =
                       let adjsIn' = filter (\(p,idxSubj) -> fromJust (IntMap.lookup idxSubj idxLookup) == fromJust maybeSubj
                                                            && p  == fromJust maybePred ) adjsIn
                           adjsOut' = filter (\(p,idxObj) -> fromJust (IntMap.lookup idxObj idxLookup) == fromJust maybeObj
                                                            && p  == fromJust maybePred ) adjsOut
                           ts1 = if thisNode == fromJust maybeObj
                                 then mkTriples idxLookup thisNode adjsIn' []
                                 else []
                           ts2 = mkTriples idxLookup thisNode [] adjsOut'
                       in ts1 ++ ts2

        cfun ( _ , _ , _ , _ ) = undefined  -- not sure why this pattern is needed to exhaust cfun arg patterns

    in concat $ DFS.dfsWith' cfun g
