{-# LANGUAGE OverloadedStrings #-}

module W3C.RdfXmlTest
  ( tests
  ) where

import Data.Semigroup ((<>))
import Data.Maybe (fromJust)
import Test.Tasty
import qualified Test.Tasty.HUnit as TU
import qualified Data.Text as T

import W3C.Manifest
import W3C.W3CAssertions

import Data.RDF.Types
import Data.RDF.Query
import Text.RDF.RDF4H.XmlParser
import Text.RDF.RDF4H.NTriplesParser
import Data.RDF.Graph.TList

tests :: Manifest -> TestTree
tests = runManifestTests mfEntryToTest

-- Functions to map manifest test entries to unit tests.
-- They are defined here to avoid cluttering W3C.Manifest
-- with functions that may not be needed to those who
-- just want to parse Manifest files.
-- TODO: They should probably be moved to W3C.Manifest after all.
mfEntryToTest :: TestEntry -> TestTree
mfEntryToTest (TestXMLEval nm _ _ act' res') =
  let act = (UNode . fromJust . fileSchemeToFilePath) act'
      res = (UNode . fromJust . fileSchemeToFilePath) res'
      parsedRDF =  (fromEither <$> parseFile testParser (nodeURI act)) :: IO (RDF TList)
      expectedRDF = (fromEither <$> parseFile NTriplesParser (nodeURI res)) :: IO (RDF TList)
  in TU.testCase (T.unpack nm) $ assertIsIsomorphic parsedRDF expectedRDF
mfEntryToTest (TestXMLNegativeSyntax nm _ _ act') =
  let act = (UNode . fromJust . fileSchemeToFilePath) act'
      rdf = parseFile testParser (nodeURI act) :: IO (Either ParseFailure (RDF TList))
  in TU.testCase (T.unpack nm) $ assertIsNotParsed rdf
mfEntryToTest x = error $ "unknown TestEntry pattern in mfEntryToTest: " <> show x

mfBaseURIXml :: BaseUrl
mfBaseURIXml = BaseUrl "http://www.w3.org/2013/RDFXMLTests/"

testParser :: XmlParser
testParser = XmlParser (Just mfBaseURIXml) Nothing
