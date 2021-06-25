{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.RDF.State where

import           Control.Monad.State
import           Data.RDF            (RDF, Rdf, PrefixMappings, BaseUrl, Triples, Triple, NodeSelector)
import qualified Data.RDF            as RDF

-- | StateT monad which allows to combine more easily functions from 'Rdf'
-- typeclass, eg.:
--
-- @
-- import Data.RDF
-- import qualified Data.RDF.State as RDFS
--
-- main :: IO ()
-- main = do
--   let myEmptyGraph = empty :: RDF TList
--   newGraph <- execStateT (unRdfST createGraph) myEmptyGraph
--   putStrLn (showGraph newGraph)
--
-- createGraph :: (Rdf rdfImpl, Monad m) => RdfST rdfImpl m ()
-- createGraph = do
--   -- add a triple to the empty graph
--   let triple1 = triple (unode "http://www.example.com/rob")
--                        (unode "http://xmlns.com/foaf/0.1/interest")
--                        (unode "http://dbpedia.org/resource/Scotch_whisky")
--   RDFS.addTriple triple1
--
--   -- add another triple to the graph
--   let triple2 = triple (unode "http://www.example.com/rob")
--                        (unode "http://xmlns.com/foaf/0.1/interest")
--                        (unode "http://dbpedia.org/resource/Haskell_(programming_language)")
--   RDFS.addTriple triple2
--
--   -- remove one of my interests
--   RDFS.removeTriple triple1
-- @
newtype RdfST rdfImpl m a = RdfST { unRdfST :: StateT (RDF rdfImpl) m a }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState (RDF rdfImpl),
      MonadTrans,
      MonadIO
    )

-- | Same as 'Data.RDF.Types.baseUrl'
baseUrl
  :: (Rdf rdfImpl, Monad m)
  => RdfST rdfImpl m (Maybe BaseUrl)
baseUrl = gets RDF.baseUrl

-- | Same as 'Data.RDF.Types.prefixMappings'
prefixMappings
  :: (Rdf rdfImpl, Monad m)
  => RdfST rdfImpl m PrefixMappings
prefixMappings = gets RDF.prefixMappings

-- | Same as 'Data.RDF.Types.addPrefixMappings', but with the Bool arg as False
addPrefixMappings
  :: (Rdf rdfImpl, Monad m)
  => PrefixMappings
  -> RdfST rdfImpl m ()
addPrefixMappings mappings =
  get >>= \graph -> put $ RDF.addPrefixMappings graph mappings False

-- | Same as 'Data.RDF.Types.addPrefixMappings', but with the Bool arg as True
replacePrefixMappings
  :: (Rdf rdfImpl, Monad m)
  => PrefixMappings
  -> RdfST rdfImpl m ()
replacePrefixMappings mappings =
  get >>= \graph -> put $ RDF.addPrefixMappings graph mappings True

-- | Same as 'Data.RDF.Types.empty'
empty
  :: (Rdf rdfImpl, Monad m)
  => RdfST rdfImpl m ()
empty = put RDF.empty

-- | Same as 'Data.RDF.Types.mkRdf'
mkRdf
  :: (Rdf rdfImpl, Monad m)
  => Triples
  -> Maybe BaseUrl
  -> PrefixMappings
  -> RdfST rdfImpl m ()
mkRdf triples baseUrlMaybe mappings =
  put $ RDF.mkRdf triples baseUrlMaybe mappings

-- | Same as 'Data.RDF.Types.addTriple'
addTriple
  :: (Rdf rdfImpl, Monad m)
  => Triple -> RdfST rdfImpl m ()
addTriple triple = get >>= \graph -> put $ RDF.addTriple graph triple

-- | Same as 'Data.RDF.Types.removeTriple'
removeTriple
  :: (Rdf rdfImpl, Monad m)
  => Triple -> RdfST rdfImpl m ()
removeTriple triple = get >>= \graph -> put $ RDF.removeTriple graph triple

-- | Same as 'Data.RDF.Types.triplesOf'
triplesOf
  :: (Rdf rdfImpl, Monad m)
  => RdfST rdfImpl m Triples
triplesOf = gets RDF.triplesOf

-- | Same as 'Data.RDF.Types.uniqTriplesOf'
uniqTriplesOf
  :: (Rdf rdfImpl, Monad m)
  => RdfST rdfImpl m Triples
uniqTriplesOf = gets RDF.uniqTriplesOf

-- | Same as 'Data.RDF.Types.select'
select
  :: (Rdf rdfImpl, Monad m)
  => NodeSelector
  -> NodeSelector
  -> NodeSelector
  -> RdfST rdfImpl m Triples
select s p o = get >>= \graph -> return $ RDF.select graph s p o

-- | Same as 'Data.RDF.Types.query'
query
  :: (Rdf rdfImpl, Monad m)
  => Maybe RDF.Node
  -> Maybe RDF.Node
  -> Maybe RDF.Node
  -> RdfST rdfImpl m Triples
query s p o = get >>= \graph -> return $ RDF.query graph s p o

-- | Same as 'Data.RDF.Types.showGraph'
showGraph
  :: (Rdf rdfImpl, Monad m)
  => RdfST rdfImpl m String
showGraph = gets RDF.showGraph
