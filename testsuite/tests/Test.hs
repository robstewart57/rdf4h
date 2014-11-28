module Main where

import Test.Framework (defaultMain)

import qualified Data.RDF.TriplesGraph_Test as TriplesGraph
import qualified Data.RDF.MGraph_Test as MGraph
import qualified Text.RDF.RDF4H.XmlParser_Test as XmlParser
import qualified Text.RDF.RDF4H.TurtleParser_ConformanceTest as TurtleParser
import qualified W3C.TurtleTest as W3CTurtleTest
import qualified W3C.RdfXmlTest as W3CRdfXmlTest

main :: IO ()
main = defaultMain (  TriplesGraph.tests
                   ++ MGraph.tests
                   ++ TurtleParser.tests
                   ++ XmlParser.tests
                   ++ W3CTurtleTest.tests
                   ++ W3CRdfXmlTest.tests
                   )
