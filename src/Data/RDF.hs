-- |The Core module exports all serializers and parsers,
--  types, and query functions of the library.

module Data.RDF (

  RDF(..),
  RdfSerializer(..),
  RdfParser(..),

  -- * Export types and query functions
  module Data.RDF.Types,
  module Data.RDF.Query,

  -- * Export RDF type class instances
  module Data.RDF.Graph.TriplesList,
  module Data.RDF.Graph.HashMapS,
  module Data.RDF.Graph.HashMapSP,
  module Data.RDF.Graph.MapSP,
  module Data.RDF.Graph.TriplesPatriciaTree,

  -- * Export RDF parsers and serializers
  module Text.RDF.RDF4H.NTriplesSerializer,
  module Text.RDF.RDF4H.NTriplesParser,
  module Text.RDF.RDF4H.TurtleSerializer,
  module Text.RDF.RDF4H.TurtleParser,
  module Text.RDF.RDF4H.XmlParser,
)
where

import Data.RDF.Namespace
import Data.RDF.Graph.TriplesList
import Data.RDF.Graph.HashMapS
import Data.RDF.Graph.HashMapSP
import Data.RDF.Graph.MapSP
import Data.RDF.Graph.TriplesPatriciaTree
import Text.RDF.RDF4H.NTriplesSerializer
import Text.RDF.RDF4H.TurtleSerializer
import Text.RDF.RDF4H.NTriplesParser
import Text.RDF.RDF4H.TurtleParser
import Text.RDF.RDF4H.XmlParser
import Data.RDF.Types
import Data.RDF.Query
