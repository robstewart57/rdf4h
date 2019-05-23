{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromJust)
import Data.RDF
import           Data.RDF.GraphImplTests
import           Data.RDF.PropertyTests
import           Data.RDF.IRITests
import qualified Data.Text as T
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import Test.Tasty (defaultMain,testGroup)
import W3C.Manifest
import qualified W3C.NTripleTest as W3CNTripleTest
import qualified W3C.RdfXmlTest as W3CRdfXmlTest
import qualified W3C.TurtleTest as W3CTurtleTest
import qualified Text.RDF.RDF4H.TurtleParser_ConformanceTest as TurtleUnitTest
import qualified Text.RDF.RDF4H.XmlParser_Test as XmlUnitTest

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
  let fileSchemeUri suitesDir =
        fromJust . filePathToUri $ (dir </> T.unpack suitesDir)
  turtleManifest <-
    loadManifest mfPathTurtle (fileSchemeUri suiteFilesDirTurtle)
  xmlManifest <- loadManifest mfPathXml (fileSchemeUri suiteFilesDirXml)
  nTriplesManifest <-
    loadManifest mfPathNTriples (fileSchemeUri suiteFilesDirNTriples)
  -- run tests
  defaultMain
    (testGroup
       "rdf4h tests"
       -- RDF graph API tests
       [ testGroup
         "property-tests"
         [(graphTests
           "TList"
           (empty :: RDF TList)
           (mkRdf :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF TList))
         ,
         (graphTests
           "AdjHashMap"
           (empty :: RDF AdjHashMap)
           (mkRdf :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF AdjHashMap))
         ,
         (graphTests
           "AlgebraicGraph"
           (empty :: RDF AlgebraicGraph)
           (mkRdf :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF AlgebraicGraph))]
       ,
         testGroup
         "graph-impl-unit-tests"
         [ graphImplTests ]
       ,
         testGroup
         "iri"
         [ iriTests ]
       ,

       -- RDF parser unit tests
       testGroup "parser-unit-tests-turtle"
       TurtleUnitTest.tests
       ,
       testGroup "parser-unit-tests-xml"
       XmlUnitTest.tests
       ,

       -- RDF parser W3C tests
       testGroup
       "parser-w3c-tests-ntriples"
       [ testGroup
         "parser-w3c-tests-ntriples-parsec"
         [W3CNTripleTest.testsParsec nTriplesManifest]
       , testGroup
         "parser-w3c-tests-ntriples-attoparsec"
         [W3CNTripleTest.testsAttoparsec nTriplesManifest]
       ]
       ,
       testGroup
       "parser-w3c-tests-turtle"
       [ testGroup
         "parser-w3c-tests-turtle-parsec"
         [W3CTurtleTest.testsParsec turtleManifest]
       , testGroup
         "parser-w3c-tests-turtle-attoparsec"
         [W3CTurtleTest.testsAttoparsec turtleManifest]
       ]
       ,
       testGroup
       "parser-w3c-tests-xml"
       [ W3CRdfXmlTest.tests xmlManifest
       ]
       ]
    )
