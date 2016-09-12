module Main where

import Test.Tasty (defaultMain,testGroup)

import qualified Data.RDF.Graph.TList_Test as TList
import qualified Data.RDF.Graph.HashS_Test as HashS
import qualified Data.RDF.Graph.HashSP_Test as HashSP
import qualified Data.RDF.Graph.SP_Test as SP
-- very slow implementation, disabled for now.
-- import qualified Data.RDF.Graph.TriplesPatriciaTree_Test as TriplesPatriciaTree
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
          graphTests "TList"
          TList.triplesOf'
          TList.uniqTriplesOf'
          TList.empty'
          TList.mkRdf'

        , graphTests "HashS"
          HashS.triplesOf'
          HashS.uniqTriplesOf'
          HashS.empty'
          HashS.mkRdf'

        , graphTests "HashSP"
          HashSP.triplesOf'
          HashSP.uniqTriplesOf'
          HashSP.empty'
          HashSP.mkRdf'

        , graphTests "SP"
          SP.triplesOf'
          SP.uniqTriplesOf'
          SP.empty'
          SP.mkRdf'

        -- , graphTests "TriplesPatriciaTree"
        --   TriplesPatriciaTree.triplesOf'
        --   TriplesPatriciaTree.uniqTriplesOf'
        --   TriplesPatriciaTree.empty'
        --   TriplesPatriciaTree.mkRdf'

          -- rdf4h unit tests
        , TurtleParser.tests
        , XmlParser.tests

          -- W3C tests
        , W3CTurtleTest.tests   turtleManifest
        , W3CRdfXmlTest.tests   xmlManifest
        , W3CNTripleTest.tests  nTriplesManifest
        ])
