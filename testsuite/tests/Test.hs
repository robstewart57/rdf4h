module Main where

import Test.Framework (defaultMain)

import qualified Data.RDF.Graph.TriplesList_Test as TriplesList
import qualified Data.RDF.Graph.HashMapS_Test as HashMapS
import qualified Data.RDF.Graph.HashMapSP_Test as HashMapSP
import qualified Data.RDF.Graph.MapSP_Test as MapSP
import qualified Data.RDF.Graph.TriplesPatriciaTree_Test as TriplesPatriciaTree
import qualified Text.RDF.RDF4H.XmlParser_Test as XmlParser
import qualified Text.RDF.RDF4H.TurtleParser_ConformanceTest as TurtleParser
import qualified W3C.TurtleTest as W3CTurtleTest
import qualified W3C.RdfXmlTest as W3CRdfXmlTest
import qualified W3C.NTripleTest as W3CNTripleTest
import Data.RDF.GraphTestUtils

main :: IO ()
main = defaultMain (
                      graphTests "TriplesList"
                         TriplesList.triplesOf'
                         TriplesList.uniqTriplesOf'
                         TriplesList.empty'
                         TriplesList.mkRdf'

                   ++ graphTests "HashMapS"
                         HashMapS.triplesOf'
                         HashMapS.uniqTriplesOf'
                         HashMapS.empty'
                         HashMapS.mkRdf'

                   ++ graphTests "HashMapSP"
                         HashMapSP.triplesOf'
                         HashMapSP.uniqTriplesOf'
                         HashMapSP.empty'
                         HashMapSP.mkRdf'

                   ++ graphTests "MapSP"
                         MapSP.triplesOf'
                         MapSP.uniqTriplesOf'
                         MapSP.empty'
                         MapSP.mkRdf'

                   ++ graphTests "TriplesPatriciaTree"
                         TriplesPatriciaTree.triplesOf'
                         TriplesPatriciaTree.uniqTriplesOf'
                         TriplesPatriciaTree.empty'
                         TriplesPatriciaTree.mkRdf'

                   ++ TurtleParser.tests
                   ++ XmlParser.tests
                   ++ W3CTurtleTest.tests
                   ++ W3CRdfXmlTest.tests
                   ++ W3CNTripleTest.tests
                   )
