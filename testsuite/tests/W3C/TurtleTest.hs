module W3C.TurtleTest where

-- 1. Load manifest.ttl
-- 2. Map every test to its result
-- (Determine the test type and call appropriate test function)

import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as TU
import qualified Data.Text as T

import W3C.Manifest

import Data.RDF.Types
import Data.RDF.Query
import Text.RDF.RDF4H.TurtleParser
import Data.RDF.TriplesGraph

manifests :: [(String, String)]
manifests = [("data/w3c/turtle/manifest.ttl", "http://www.w3.org/2013/TurtleTests/")]

tests :: [Test]
tests = [ testGroup "W3C Turtle Tests" allTurtleTests ]

allTurtleTests :: [Test]
allTurtleTests = [] -- TODO

-- Functions to map manifest test entries to unit tests.
-- They are defined here to avoid cluttering W3C.Manifest
-- with functions that may not be needed to those who
-- just want to parse Manifest files.
-- TODO: They should probably be moved to W3C.Manifest after all.
mfEntryToTest :: TestEntry -> Test
mfEntryToTest (TestTurtleEval nm cmt apr act res) = buildTest $ do
  parsedRDF <- parseFile parserA (nodeURI act) >>= return . fromEither :: IO TriplesGraph
  expectedRDF <- parseFile parserB (nodeURI res) >>= return . fromEither :: IO TriplesGraph
  return $ testCase (T.unpack nm) $ TU.assert $ isIsomorphic parsedRDF expectedRDF
  where parserA = TurtleParser (Just (BaseUrl baseIRI)) (Just baseIRI)
        parserB = TurtleParser (Just (BaseUrl baseIRI)) (Just baseIRI)
        baseIRI = "http://example.org/1/"
        nodeURI = \(UNode u) -> T.unpack u
mfEntryToTest (TestTurtleNegativeEval nm cmt apr act) = undefined -- TODO
mfEntryToTest (TestTurtlePositiveSyntax nm cmt apr act) = undefined -- TODO
mfEntryToTest (TestTurtleNegativeSyntax nm cmt apr act) = undefined -- TODO
