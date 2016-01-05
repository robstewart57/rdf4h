module Main where

import Test.Tasty (defaultMain,testGroup)

import qualified Data.RDF.Graph.TriplesList_Test as TriplesList
import qualified Data.RDF.Graph.HashMapS_Test as HashMapS
import qualified Data.RDF.Graph.HashMapSP_Test as HashMapSP
import qualified Data.RDF.Graph.MapSP_Test as MapSP
import qualified Data.RDF.Graph.TriplesPatriciaTree_Test as TriplesPatriciaTree
import           Data.RDF.Types
import qualified Text.RDF.RDF4H.XmlParser_Test as XmlParser
import qualified Text.RDF.RDF4H.TurtleParser_ConformanceTest as TurtleParser
import qualified W3C.TurtleTest as W3CTurtleTest
import qualified W3C.RdfXmlTest as W3CRdfXmlTest
import qualified W3C.NTripleTest as W3CNTripleTest
import           Data.RDF.GraphTestUtils

import qualified Data.Text as T
import System.Directory (getCurrentDirectory)
import W3C.Manifest

suiteFilesDirTurtle,suiteFilesDirXml,suiteFilesDirNTriples :: T.Text
suiteFilesDirTurtle = "rdf-tests/turtle/"
suiteFilesDirXml = "rdf-tests/rdf-xml/"
suiteFilesDirNTriples = "rdf-tests/ntriples/"

mfPathTurtle,mfPathXml,mfPathNTriples :: T.Text
mfPathTurtle = T.concat [suiteFilesDirTurtle, "manifest.ttl"]
mfPathXml = T.concat [suiteFilesDirXml, "manifest.ttl"]
mfPathNTriples = T.concat [suiteFilesDirNTriples, "manifest.ttl"]

mfBaseURITurtle,mfBaseURIXml,mfBaseURINTriples :: BaseUrl
mfBaseURITurtle   = BaseUrl "http://www.w3.org/2013/TurtleTests/"
mfBaseURIXml      = BaseUrl "http://www.w3.org/2013/RDFXMLTests/"
mfBaseURINTriples = BaseUrl "http://www.w3.org/2013/N-TriplesTests/"


main :: IO ()
main = do
  -- obtain manifest files for W3C tests before running tests
  -- justification for separation: http://stackoverflow.com/a/33046723
  dir <- getCurrentDirectory
  let fileSchemeUri suitesDir = T.pack ("file://" ++ dir ++ "/" ++ T.unpack suitesDir)
  turtleManifest <-
         loadManifest mfPathTurtle   (fileSchemeUri suiteFilesDirTurtle)

  xmlManifest <-
         loadManifest mfPathXml      (fileSchemeUri suiteFilesDirXml)

  nTriplesManifest <-
         loadManifest mfPathNTriples (fileSchemeUri suiteFilesDirNTriples)

  -- run tests
  defaultMain
       (testGroup "rdf4h tests"
        [ -- RDF graph API tests
          graphTests "TriplesList"
          TriplesList.triplesOf'
          TriplesList.uniqTriplesOf'
          TriplesList.empty'
          TriplesList.mkRdf'

        , graphTests "HashMapS"
          HashMapS.triplesOf'
          HashMapS.uniqTriplesOf'
          HashMapS.empty'
          HashMapS.mkRdf'

        , graphTests "HashMapSP"
          HashMapSP.triplesOf'
          HashMapSP.uniqTriplesOf'
          HashMapSP.empty'
          HashMapSP.mkRdf'

        , graphTests "MapSP"
          MapSP.triplesOf'
          MapSP.uniqTriplesOf'
          MapSP.empty'
          MapSP.mkRdf'

        , graphTests "TriplesPatriciaTree"
          TriplesPatriciaTree.triplesOf'
          TriplesPatriciaTree.uniqTriplesOf'
          TriplesPatriciaTree.empty'
          TriplesPatriciaTree.mkRdf'

          -- rdf4h unit tests
        , TurtleParser.tests
        , XmlParser.tests

          -- W3C tests
        , W3CTurtleTest.tests   turtleManifest
        , W3CRdfXmlTest.tests   xmlManifest
        , W3CNTripleTest.tests  nTriplesManifest
        ])
