{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad
import qualified Data.Map as Map
import Data.RDF
import           Data.RDF.PropertyTests
import qualified Data.Text as T
import System.Directory (getCurrentDirectory)
import Test.QuickCheck.Arbitrary
import Test.Tasty (defaultMain,testGroup)
import W3C.Manifest
import qualified W3C.NTripleTest as W3CNTripleTest
import qualified W3C.RdfXmlTest as W3CRdfXmlTest
import qualified W3C.TurtleTest as W3CTurtleTest
import qualified Text.RDF.RDF4H.TurtleParser_ConformanceTest as TurtleUnitTest

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

instance Arbitrary (RDF TList) where
  arbitrary =
    liftM3
      mkRdf
      arbitraryTs
      (return Nothing)
      (return $ PrefixMappings Map.empty)

instance Arbitrary (RDF AdjHashMap) where
  arbitrary =
    liftM3
      mkRdf
      arbitraryTs
      (return Nothing)
      (return $ PrefixMappings Map.empty)

main :: IO ()
main
-- obtain manifest files for W3C tests before running tests
-- justification for separation: http://stackoverflow.com/a/33046723
 = do
  dir <- getCurrentDirectory
  let fileSchemeUri suitesDir =
        T.pack ("file://" ++ dir ++ "/" ++ T.unpack suitesDir)
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
           (mkRdf :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF AdjHashMap))]
       ,
       testGroup "parser-unit-tests-turtle"
       TurtleUnitTest.allCTests
       ,
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
