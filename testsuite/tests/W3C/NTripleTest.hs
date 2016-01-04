module W3C.NTripleTest where

import Data.Maybe (fromJust)
import Test.Tasty
import qualified Test.Tasty.HUnit as TU
import qualified Data.Text as T

import W3C.Manifest
import W3C.W3CAssertions

import Data.RDF.Types
import Text.RDF.RDF4H.NTriplesParser
import Data.RDF.Graph.TriplesList

tests :: Manifest -> TestTree
tests = runManifestTests mfEntryToTest

-- Functions to map manifest test entries to unit tests.
-- They are defined here to avoid cluttering W3C.Manifest
-- with functions that may not be needed to those who
-- just want to parse Manifest files.
-- TODO: They should probably be moved to W3C.Manifest after all.
mfEntryToTest :: TestEntry -> TestTree
mfEntryToTest (TestNTriplesPositiveSyntax nm _ _ act') =
  let act = (UNode . fromJust . fileSchemeToFilePath) act'
      rdf = parseFile testParser (nodeURI act) :: IO (Either ParseFailure TriplesList)
  in TU.testCase (T.unpack nm) $ assertIsParsed rdf
mfEntryToTest (TestNTriplesNegativeSyntax nm _ _ act') =
  let act = (UNode . fromJust . fileSchemeToFilePath) act'
      rdf = parseFile testParser (nodeURI act) :: IO (Either ParseFailure TriplesList)
  in TU.testCase (T.unpack nm) $ assertIsNotParsed rdf
mfEntryToTest x = error $ "unknown TestEntry pattern in mfEntryToTest: " ++ show x

testParser :: NTriplesParser
testParser = NTriplesParser
