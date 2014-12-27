module Main where

import Test.Framework (defaultMain)

import qualified Data.RDF.TriplesGraph_Test as TriplesGraph
import qualified Data.RDF.MGraph_Test as MGraph
import qualified Data.RDF.PatriciaTreeGraph_Test as PatriciaTreeGraph
import qualified Text.RDF.RDF4H.XmlParser_Test as XmlParser
import qualified Text.RDF.RDF4H.TurtleParser_ConformanceTest as TurtleParser
import qualified W3C.TurtleTest as W3CTurtleTest
import qualified W3C.RdfXmlTest as W3CRdfXmlTest
import qualified W3C.NTripleTest as W3CNTripleTest
import Data.RDF.GraphTestUtils

main :: IO ()
main = defaultMain (
                      graphTests "TriplesGraph"
                         TriplesGraph.triplesOf'
                         TriplesGraph.uniqTriplesOf'
                         TriplesGraph.empty'
                         TriplesGraph.mkRdf'

                   ++ graphTests "MGraph"
                         MGraph.triplesOf'
                         MGraph.uniqTriplesOf'
                         MGraph.empty'
                         MGraph.mkRdf'

                   ++ graphTests "PatriciaTreeGraph"
                         PatriciaTreeGraph.triplesOf'
                         PatriciaTreeGraph.uniqTriplesOf'
                         PatriciaTreeGraph.empty'
                         PatriciaTreeGraph.mkRdf'

                   ++ TurtleParser.tests
                   ++ XmlParser.tests
                   ++ W3CTurtleTest.tests
                   ++ W3CRdfXmlTest.tests
                   -- ++ W3CNTripleTest.tests -- contains infinite loop
                   )
