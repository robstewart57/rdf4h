module Data.RDF.PatriciaTreeGraph where

import Data.RDF.Namespace
import Data.RDF.Query
import Data.RDF.Types

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as PT
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Data.IntMap as IntMap
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T

newtype PatriciaTreeGraph = PatriciaTreeGraph (PT.Gr Node Node,IntMap.IntMap Node, Maybe BaseUrl, PrefixMappings)
                            deriving (Show)

instance RDF PatriciaTreeGraph where
  baseUrl           = baseUrl'
  prefixMappings    = prefixMappings'
  addPrefixMappings = addPrefixMappings'
  empty             = empty'
  mkRdf             = mkRdf'
  triplesOf         = uniqTriplesOf'
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

-- in case we want to expand UNode nodes in each triple in mkRdf'
expandNode (Just (BaseUrl b)) (UNode t) = UNode (b `T.append` t)
expandNode _ node = node

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> PatriciaTreeGraph
mkRdf' ts base' pms' =
    let xs = concatMap (\(Triple s _p o) -> [s,o]) ts
        lnodes = zip [0..length xs-1] xs

        uriIdx = Map.fromList (map (\(a,b) -> (b,a)) lnodes)
        intIdx = IntMap.fromList lnodes

        ledges = map (\(Triple s p o) ->
                          let si = fromJust $ Map.lookup s uriIdx
                              oi = fromJust $ Map.lookup o uriIdx
                          in (si,oi,expandNode base' p)) ts

        ptGraph = G.mkGraph lnodes ledges

    in PatriciaTreeGraph (ptGraph ,intIdx, base', pms')

{- will this remain as an RDF method?
triplesOf' :: PatriciaTreeGraph -> Triples
triplesOf' (PatriciaTreeGraph (g,idxLookup,_,_)) =
    map (\(sIdx,oIdx,p) ->
             let [s,o] = map (\idx -> fromJust $ IntMap.lookup idx idxLookup) [sIdx,oIdx]
             in Triple s p o) (G.labEdges g)
-}

uniqTriplesOf' :: PatriciaTreeGraph -> Triples
uniqTriplesOf' ptG@(PatriciaTreeGraph (g,idxLookup,_,_)) =
    nub $ map (\(sIdx,oIdx,p) ->
             let [s,o] = map (\idx -> fromJust $ IntMap.lookup idx idxLookup) [sIdx,oIdx]
             in expandTriple ptG (Triple s p o)) (G.labEdges g)

select' :: PatriciaTreeGraph -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' ptG@(PatriciaTreeGraph (g,idxLookup,_,_)) maybeSubjSel maybePredSel maybeObjSel =
    let mkTriples nodeIdx = map (\(p,subjIdx) ->
                                    let o = fromJust (IntMap.lookup nodeIdx idxLookup)
                                        s = fromJust (IntMap.lookup subjIdx idxLookup)
                                    in expandTriple ptG (Triple s p o) )  -- expand the triple

        cfun ( adjsIn , nodeIdx , _nodeLbl , _adjsOut ) =
            let ts | isJust maybeSubjSel && isNothing maybePredSel && isNothing maybeObjSel =
                       let ss = filter (\(_p,idxSubj) -> let subjNode = fromJust (IntMap.lookup idxSubj idxLookup)
                                                       in fromJust maybeSubjSel subjNode) adjsIn

                       in mkTriples nodeIdx ss

                   | isJust maybeSubjSel && isJust maybePredSel && isNothing maybeObjSel =
                       let ss = filter (\(p,idxSubj) -> let subjNode = fromJust (IntMap.lookup idxSubj idxLookup)
                                                       in fromJust maybeSubjSel subjNode
                                                          && fromJust maybePredSel p) adjsIn

                       in mkTriples nodeIdx ss

                   | isJust maybeSubjSel && isNothing maybePredSel && isJust maybeObjSel =
                       let ss = filter (\(_p,idxSubj) -> let subjNode = fromJust (IntMap.lookup idxSubj idxLookup); objNode  = fromJust (IntMap.lookup nodeIdx idxLookup)
                                                       in fromJust maybeSubjSel subjNode
                                                          && fromJust maybeObjSel objNode) adjsIn

                       in mkTriples nodeIdx ss

                   | isJust maybeSubjSel && isJust maybePredSel && isJust maybeObjSel =
                       let ss = filter (\(p,idxSubj) -> let subjNode = fromJust (IntMap.lookup idxSubj idxLookup); objNode  = fromJust (IntMap.lookup nodeIdx idxLookup)
                                                       in fromJust maybeSubjSel subjNode
                                                          && fromJust maybePredSel p
                                                          && fromJust maybeObjSel objNode) adjsIn

                       in mkTriples nodeIdx ss

                   | isNothing maybeSubjSel && isJust maybePredSel && isNothing maybeObjSel =
                       let ss = filter (\(p,_idxSubj) -> fromJust maybePredSel p) adjsIn

                       in mkTriples nodeIdx ss

                   | isNothing maybeSubjSel && isJust maybePredSel && isJust maybeObjSel =
                       let ss = filter (\(p,_idxSubj) -> let objNode  = fromJust (IntMap.lookup nodeIdx idxLookup)
                                                       in fromJust maybePredSel p
                                                           && fromJust maybeObjSel objNode) adjsIn

                       in mkTriples nodeIdx ss

                   | isNothing maybeSubjSel && isNothing maybePredSel && isJust maybeObjSel =
                       let objNode = fromJust (IntMap.lookup nodeIdx idxLookup)
                       in if fromJust maybeObjSel objNode
                          then mkTriples nodeIdx adjsIn
                          else []

                   | isNothing  maybeSubjSel && isNothing maybePredSel && isNothing maybeObjSel =
                        mkTriples nodeIdx adjsIn
            in ts

       -- is depth first better or worse than breadth first?
    in concat $ DFS.dfsWith' cfun g

query' :: PatriciaTreeGraph -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
query' ptG@(PatriciaTreeGraph (g,idxLookup,_,_)) maybeSubj maybePred maybeObj =
    let mkTriples nodeIdx = map (\(p,subjIdx) ->
                              let o = fromJust (IntMap.lookup nodeIdx idxLookup)
                                  s = fromJust (IntMap.lookup subjIdx idxLookup)
                              in expandTriple ptG (Triple s p o) ) -- expand the triple

        cfun ( adjsIn , nodeIdx , _nodeLbl , _adjsOut ) =
            let ts | isJust maybeSubj && isNothing maybePred && isNothing maybeObj =
                    let ss = filter (\(_p,idxSubj) -> fromJust (IntMap.lookup idxSubj idxLookup) == fromJust maybeSubj ) adjsIn
                    in mkTriples nodeIdx ss

                   | isJust maybeSubj && isJust maybePred && isNothing maybeObj =
                    let ss = filter (\(p,idxSubj) -> fromJust (IntMap.lookup idxSubj idxLookup) == fromJust maybeSubj
                                                    && p == fromJust maybePred) adjsIn
                    in mkTriples nodeIdx ss

                   | isJust maybeSubj && isJust maybePred && isJust maybeObj =
                    let objNode = fromJust (IntMap.lookup nodeIdx idxLookup)
                        ss = filter (\(p,idxSubj) -> fromJust (IntMap.lookup idxSubj idxLookup) == fromJust maybeSubj
                                                    && p == fromJust maybePred) adjsIn
                    in if objNode == fromJust maybeObj
                       then mkTriples nodeIdx ss
                       else []

                   | isNothing maybeSubj && isJust maybePred && isNothing maybeObj =
                    let ss = filter (\(p,_idxSubj) -> p == fromJust maybePred) adjsIn
                    in mkTriples nodeIdx ss

                   | isNothing maybeSubj && isJust maybePred && isJust maybeObj =
                    let objNode = fromJust (IntMap.lookup nodeIdx idxLookup)
                        ss = filter (\(p,_idxSubj) -> p == fromJust maybePred) adjsIn
                    in if objNode == fromJust maybeObj
                       then mkTriples nodeIdx ss
                       else []

                   | isNothing maybeSubj && isNothing maybePred && isJust maybeObj =
                    let objNode = fromJust (IntMap.lookup nodeIdx idxLookup)
                    in if objNode == fromJust maybeObj
                       then mkTriples nodeIdx adjsIn
                       else []

                   | isNothing  maybeSubj && isNothing maybePred && isNothing maybeObj =
                        mkTriples nodeIdx adjsIn

            in ts

       -- is depth first better or worse than breadth first?
    in concat $ DFS.dfsWith' cfun g
