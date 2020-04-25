{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- [TODO] Remove when the missing NFData instance is added to Alga.

module Data.RDF.Graph.AlgebraicGraph
  ( AlgebraicGraph,
  )
where

import qualified Algebra.Graph.Labelled as G
import Control.DeepSeq (NFData (..))
import Data.Binary
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.RDF.Namespace
import Data.RDF.Query
import Data.RDF.Types (BaseUrl, Node, NodeSelector, Object, Predicate, RDF, Rdf (..), Subject, Triple (..), Triples)
#if MIN_VERSION_base(4,9,0)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#else
#endif
#else
#endif
import GHC.Generics

data AlgebraicGraph deriving (Generic)

instance Binary AlgebraicGraph

instance NFData AlgebraicGraph

data instance RDF AlgebraicGraph
  = AlgebraicGraph
      { _graph :: G.Graph (HashSet Node) Node,
        _baseUrl :: Maybe BaseUrl,
        _prefixMappings :: PrefixMappings
      }
  deriving (Generic, NFData)

instance Rdf AlgebraicGraph where
  baseUrl = _baseUrl
  prefixMappings = _prefixMappings
  addPrefixMappings = addPrefixMappings'
  empty = empty'
  mkRdf = mkRdf'
  addTriple = addTriple'
  removeTriple = removeTriple'
  triplesOf = triplesOf'
  uniqTriplesOf = triplesOf'
  select = select'
  query = query'
  showGraph = showGraph'

toEdge :: Triple -> (HashSet Predicate, Subject, Object)
toEdge (Triple s p o) = (HS.singleton p, s, o)

toTriples :: (HashSet Predicate, Subject, Object) -> Triples
toTriples (ps, s, o) = [Triple s p o | p <- HS.toList ps]

showGraph' :: RDF AlgebraicGraph -> String
showGraph' r = concatMap (\t -> show t ++ "\n") (expandTriples r)

addPrefixMappings' :: RDF AlgebraicGraph -> PrefixMappings -> Bool -> RDF AlgebraicGraph
addPrefixMappings' (AlgebraicGraph g baseURL pms) pms' replace =
  let merge = if replace then flip (<>) else (<>)
   in AlgebraicGraph g baseURL (merge pms pms')

empty' :: RDF AlgebraicGraph
empty' = AlgebraicGraph G.empty mempty (PrefixMappings mempty)

mkRdf' :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF AlgebraicGraph
mkRdf' ts baseURL pms =
  let g = G.edges . fmap toEdge $ ts
   in AlgebraicGraph g baseURL pms

addTriple' :: RDF AlgebraicGraph -> Triple -> RDF AlgebraicGraph
addTriple' (AlgebraicGraph g baseURL pms) (Triple s p o) =
  let g' = G.edge (HS.singleton p) s o
   in AlgebraicGraph (G.overlay g g') baseURL pms

removeTriple' :: RDF AlgebraicGraph -> Triple -> RDF AlgebraicGraph
removeTriple' (AlgebraicGraph g baseURL pms) (Triple s p o) =
  let ps = G.edgeLabel s o g
      g'
        | HS.null ps = g
        | elem p ps = G.replaceEdge (HS.delete p ps) s o g
        | otherwise = g
   in AlgebraicGraph g' baseURL pms

triplesOf' :: RDF AlgebraicGraph -> Triples
triplesOf' (AlgebraicGraph g _ _) = mconcat $ toTriples <$> G.edgeList g

select' :: RDF AlgebraicGraph -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' r Nothing Nothing Nothing = triplesOf r
select' (AlgebraicGraph g _ _) s p o = let (res, _, _) = G.foldg e v c g in res
  where
    e = (mempty, mempty, mempty)
    v x = (mempty, s ?? x, o ?? x)
    (??) f x' = let xs = HS.singleton x' in maybe xs (`HS.filter` xs) f
    c ps (ts1, ss1, os1) (ts2, ss2, os2) = (ts3, ss3, os3)
      where
        ss3 = ss1 <> ss2
        os3 = os1 <> os2
        ts3
          | HS.null ps' = ts1 <> ts2
          | otherwise = ts1 <> ts2 <> [Triple s' p' o' | s' <- HS.toList ss3, p' <- HS.toList ps', o' <- HS.toList os3]
        ps' = maybe ps (`HS.filter` ps) p

query' :: RDF AlgebraicGraph -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
query' r Nothing Nothing Nothing = triplesOf r
query' r s p o = select r ((==) <$> s) ((==) <$> p) ((==) <$> o)
