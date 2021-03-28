{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe (fromJust)
import Data.RDF
import Data.RDF.BlankNodeTests
import Data.RDF.GraphImplTests
import Data.RDF.IRITests
import Data.RDF.PropertyTests
import qualified Data.Text as T
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Tasty (defaultMain, testGroup)
import qualified Text.RDF.RDF4H.TurtleParser_ConformanceTest as TurtleUnitTest
import qualified Text.RDF.RDF4H.TurtleSerializerTest as TurtleSerializerTest
import qualified Text.RDF.RDF4H.XmlParser_Test as XmlUnitTest
import W3C.Manifest
import qualified W3C.NTripleTest as W3CNTripleTest
import qualified W3C.RdfXmlTest as W3CRdfXmlTest
import qualified W3C.TurtleTest as W3CTurtleTest

suiteFilesDirTurtle, suiteFilesDirXml, suiteFilesDirNTriples :: T.Text
suiteFilesDirTurtle = "rdf-tests/turtle/"
suiteFilesDirXml = "rdf-tests/rdf-xml/"
suiteFilesDirNTriples = "rdf-tests/ntriples/"

mfPathTurtle, mfPathXml, mfPathNTriples :: T.Text
mfPathTurtle = mconcat [suiteFilesDirTurtle, "manifest.ttl"]
mfPathXml = mconcat [suiteFilesDirXml, "manifest.ttl"]
mfPathNTriples = mconcat [suiteFilesDirNTriples, "manifest.ttl"]

mfBaseURITurtle, mfBaseURIXml, mfBaseURINTriples :: BaseUrl
mfBaseURITurtle = W3CTurtleTest.mfBaseURITurtle
mfBaseURIXml = W3CRdfXmlTest.mfBaseURIXml
mfBaseURINTriples = BaseUrl "http://www.w3.org/2013/N-TriplesTests/"

main :: IO ()
main = do
  -- obtain manifest files for W3C tests before running tests
  -- justification for separation: http://stackoverflow.com/a/33046723
  dir <- getCurrentDirectory
  let fileSchemeUri suitesDir =
        fromJust . filePathToUri $ (dir </> T.unpack suitesDir)
  turtleManifest <- loadManifest mfPathTurtle (unBaseUrl mfBaseURITurtle)
  xmlManifest <- loadManifest mfPathXml (unBaseUrl mfBaseURIXml)
  nTriplesManifest <-
    loadManifest mfPathNTriples (fileSchemeUri suiteFilesDirNTriples)
  -- run tests
  defaultMain
    ( testGroup
        "rdf4h tests"
        -- RDF graph API tests
        [ testGroup
            "property-tests"
            [ ( graphTests
                  "TList"
                  (empty :: RDF TList)
                  (mkRdf :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF TList)
              ),
              ( graphTests
                  "AdjHashMap"
                  (empty :: RDF AdjHashMap)
                  (mkRdf :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF AdjHashMap)
              ),
              ( graphTests
                  "AlgebraicGraph"
                  (empty :: RDF AlgebraicGraph)
                  (mkRdf :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF AlgebraicGraph)
              )
            ],
          testGroup
            "graph-impl-unit-tests"
            [graphImplTests],
          testGroup
            "iri"
            [iriTests],
          testGroup
            "bnode"
            [blankNodeTests],
          -- RDF parser unit tests
          testGroup
            "parser-unit-tests-turtle"
            TurtleUnitTest.tests,
          testGroup
            "parser-unit-tests-xml"
            XmlUnitTest.tests,
          -- RDF serializer unit tests
          testGroup "serializer-unit-tests-turtle" [TurtleSerializerTest.tests],
          -- RDF parser W3C tests
          testGroup
            "parser-w3c-tests-ntriples"
            [ testGroup
                "parser-w3c-tests-ntriples-parsec"
                [W3CNTripleTest.testsParsec nTriplesManifest],
              testGroup
                "parser-w3c-tests-ntriples-attoparsec"
                [W3CNTripleTest.testsAttoparsec nTriplesManifest]
            ],
          testGroup
            "parser-w3c-tests-turtle"
            [ testGroup
                "parser-w3c-tests-turtle-parsec"
                [W3CTurtleTest.testsParsec (dir </> T.unpack suiteFilesDirTurtle) turtleManifest],
              testGroup
                "parser-w3c-tests-turtle-attoparsec"
                [W3CTurtleTest.testsAttoparsec (dir </> T.unpack suiteFilesDirTurtle) turtleManifest]
            ],
          testGroup
            "parser-w3c-tests-xml"
            [ W3CRdfXmlTest.tests (dir </> T.unpack suiteFilesDirXml) xmlManifest
            ]
        ]
    )
