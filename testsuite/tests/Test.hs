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
import qualified Text.RDF.RDF4H.TurtleParser_ConformanceTest as TurtleParser
import qualified Text.RDF.RDF4H.XmlParser_Test as XmlParser
import W3C.Manifest
import qualified W3C.NTripleTest as W3CNTripleTest
import qualified W3C.RdfXmlTest as W3CRdfXmlTest
import qualified W3C.TurtleTest as W3CTurtleTest

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
       [ graphTests
           "TList"
           (empty :: RDF TList)
           (mkRdf :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF TList)
       , graphTests
           "AdjHashMap"
           (empty :: RDF AdjHashMap)
           (mkRdf :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF AdjHashMap)
         -- rdf4h unit tests
       , TurtleParser.tests
       , XmlParser.tests
         -- W3C tests
       , W3CTurtleTest.tests turtleManifest
       , W3CRdfXmlTest.tests xmlManifest
       , W3CNTripleTest.tests nTriplesManifest
       ])
