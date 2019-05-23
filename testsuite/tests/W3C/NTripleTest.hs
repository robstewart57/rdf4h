module W3C.NTripleTest
  ( testsParsec
  , testsAttoparsec
  ) where

import Data.Semigroup ((<>))
import Data.Maybe (fromJust)
import Test.Tasty
import qualified Test.Tasty.HUnit as TU
import qualified Data.Text as T

import W3C.Manifest
import W3C.W3CAssertions

import Data.RDF.Types
import Text.RDF.RDF4H.NTriplesParser
import Text.RDF.RDF4H.ParserUtils
import Data.RDF.Graph.TList

testsParsec :: Manifest -> TestTree
testsParsec = runManifestTests (mfEntryToTest testParserParsec)

testsAttoparsec :: Manifest -> TestTree
testsAttoparsec = runManifestTests (mfEntryToTest testParserAttoparsec)

-- Functions to map manifest test entries to unit tests.
-- They are defined here to avoid cluttering W3C.Manifest
-- with functions that may not be needed to those who
-- just want to parse Manifest files.
-- TODO: They should probably be moved to W3C.Manifest after all.
mfEntryToTest :: NTriplesParserCustom -> TestEntry -> TestTree
mfEntryToTest testParser (TestNTriplesPositiveSyntax nm _ _ act') =
  let act = (UNode . fromJust . fileSchemeToFilePath) act'
      rdf = parseFile testParser (nodeURI act) :: IO (Either ParseFailure (RDF TList))
  in TU.testCase (T.unpack nm) $ assertIsParsed rdf
mfEntryToTest testParser (TestNTriplesNegativeSyntax nm _ _ act') =
  let act = (UNode . fromJust . fileSchemeToFilePath) act'
      rdf = parseFile testParser (nodeURI act) :: IO (Either ParseFailure (RDF TList))
  in TU.testCase (T.unpack nm) $ assertIsNotParsed rdf
mfEntryToTest _ x = error $ "unknown TestEntry pattern in mfEntryToTest: " <> show x

testParserParsec :: NTriplesParserCustom
testParserParsec = NTriplesParserCustom Parsec

testParserAttoparsec :: NTriplesParserCustom
testParserAttoparsec = NTriplesParserCustom Attoparsec
