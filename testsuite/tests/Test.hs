module Main where

import Test.Framework (defaultMain)

import qualified Data.RDF.TriplesGraph_Test as TriplesGraph
import qualified Data.RDF.MGraph_Test as MGraph
import qualified Data.RDF.PatriciaTreeGraph_Test as PatriciaTreeGraph
import qualified Text.RDF.RDF4H.XmlParser_Test as XmlParser
import qualified Text.RDF.RDF4H.TurtleParser_ConformanceTest as TurtleParser
import Data.RDF.GraphTestUtils

main :: IO ()
main = defaultMain (  graphTests "TriplesGraph" TriplesGraph.triplesOf' TriplesGraph.uniqTriplesOf' TriplesGraph.empty' TriplesGraph.mkRdf'
                   ++ graphTests "MGraph" MGraph.triplesOf' MGraph.uniqTriplesOf' MGraph.empty' MGraph.mkRdf'
                   ++ graphTests "PatriciaTreeGraph" PatriciaTreeGraph.triplesOf' PatriciaTreeGraph.uniqTriplesOf' PatriciaTreeGraph.empty' PatriciaTreeGraph.mkRdf'
                   ++ TurtleParser.tests
                   ++ XmlParser.tests
                   )
