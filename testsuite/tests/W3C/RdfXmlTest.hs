{-# LANGUAGE OverloadedStrings #-}

module W3C.RdfXmlTest
  ( tests
  , mfBaseURIXml
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

tests :: String -> Manifest -> TestTree
tests = runManifestTests . mfEntryToTest

-- Functions to map manifest test entries to unit tests.
-- They are defined here to avoid cluttering W3C.Manifest
-- with functions that may not be needed to those who
-- just want to parse Manifest files.
-- TODO: They should probably be moved to W3C.Manifest after all.
mfEntryToTest :: String -> TestEntry -> TestTree
mfEntryToTest dir (TestXMLEval nm _ _ act res) =
  let pathExpected = getFilePath dir res
      pathAction = getFilePath dir act
      parsedRDF =  (fromEither <$> parseFile (testParser (nodeURI act)) pathAction) :: IO (RDF TList)
      expectedRDF = (fromEither <$> parseFile NTriplesParser pathExpected) :: IO (RDF TList)
  in TU.testCase (T.unpack nm) $ assertIsIsomorphic parsedRDF expectedRDF
mfEntryToTest dir (TestXMLNegativeSyntax nm _ _ act) =
  let pathAction = getFilePath dir act
      rdf = parseFile (testParser (nodeURI act)) pathAction :: IO (Either ParseFailure (RDF TList))
  in TU.testCase (T.unpack nm) $ assertIsNotParsed rdf
mfEntryToTest _ x = error $ "unknown TestEntry pattern in mfEntryToTest: " <> show x

getFilePath :: String -> Node -> String
getFilePath dir (UNode iri) = fixFilePath' iri
  where fixFilePath' = (dir <>) . T.unpack . fromJust . T.stripPrefix (unBaseUrl mfBaseURIXml)
getFilePath _ _ = error "Unexpected node"

mfBaseURIXml :: BaseUrl
mfBaseURIXml = BaseUrl "http://www.w3.org/2013/RDFXMLTests/"

testParser :: String -> XmlParser
testParser dUri = XmlParser (Just mfBaseURIXml) (Just . T.pack $ dUri)
