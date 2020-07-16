-- |The Core module exports all serializers and parsers,
--  types, and query functions of the library.

module Data.RDF (

  Rdf(..),
  RdfSerializer(..),
  RdfParser(..),

  -- * RDF types and query functions
  module Data.RDF.IRI,
  module Data.RDF.Types,
  module Data.RDF.Query,
  module Data.RDF.Namespace,

  -- * RDF type class instances
  module Data.RDF.Graph.TList,
  module Data.RDF.Graph.AdjHashMap,
  module Data.RDF.Graph.AlgebraicGraph,
  -- module Data.RDF.Graph.HashMapSP,
  -- module Data.RDF.Graph.MapSP,

  -- * RDF parsers and serializers
  module Text.RDF.RDF4H.NTriplesSerializer,
  module Text.RDF.RDF4H.NTriplesParser,
  module Text.RDF.RDF4H.TurtleSerializer,
  module Text.RDF.RDF4H.TurtleParser,
  module Text.RDF.RDF4H.XmlParser,
  --module Text.RDF.RDF4H.XmlParserHXT,
) where

import Data.RDF.Namespace
import Data.RDF.Graph.TList
import Data.RDF.Graph.AdjHashMap
import Data.RDF.Graph.AlgebraicGraph
-- import Data.RDF.Graph.HashMapSP
-- import Data.RDF.Graph.MapSP
import Text.RDF.RDF4H.NTriplesSerializer
import Text.RDF.RDF4H.TurtleSerializer
import Text.RDF.RDF4H.NTriplesParser
import Text.RDF.RDF4H.TurtleParser
import Text.RDF.RDF4H.XmlParser
--import Text.RDF.RDF4H.XmlParserHXT
import Data.RDF.IRI
import Data.RDF.Types
import Data.RDF.Query
