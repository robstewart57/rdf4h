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
  module Data.RDF.TriplesGraph,
  module Data.RDF.MGraph,
  module Data.RDF.PatriciaTreeGraph,

  -- * Export RDF parsers and serializers
  module Text.RDF.RDF4H.NTriplesSerializer,
  module Text.RDF.RDF4H.NTriplesParser,
  module Text.RDF.RDF4H.TurtleSerializer,
  module Text.RDF.RDF4H.TurtleParser,
  module Text.RDF.RDF4H.XmlParser,
)
where

import Data.RDF.Namespace
import Data.RDF.TriplesGraph
import Data.RDF.MGraph
import Data.RDF.PatriciaTreeGraph
import Text.RDF.RDF4H.NTriplesSerializer
import Text.RDF.RDF4H.TurtleSerializer
import Text.RDF.RDF4H.NTriplesParser
import Text.RDF.RDF4H.TurtleParser
import Text.RDF.RDF4H.XmlParser
import Data.RDF.Types
import Data.RDF.Query
