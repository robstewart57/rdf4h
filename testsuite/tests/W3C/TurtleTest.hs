module W3C.TurtleTest where

import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as TU
import qualified Data.Text.Lazy as T

import W3C.Manifest

import Data.RDF.Types
import Data.RDF.Query
import Text.RDF.RDF4H.TurtleParser
import Text.RDF.RDF4H.NTriplesParser
import Data.RDF.TriplesGraph

suiteFilesDir = "data/w3c/turtle/TurtleTests/"

mfPath = T.concat [suiteFilesDir, "manifest.ttl"]
mfBaseURI = BaseUrl "http://www.w3.org/2013/TurtleTests/"

tests :: [Test]
tests = [ buildTest allTurtleTests ]

allTurtleTests :: IO Test
allTurtleTests = do
  m <- loadManifest mfPath suiteFilesDir
  return $ testGroup (T.unpack $ description m) $ map (buildTest . mfEntryToTest) $ entries m

-- Functions to map manifest test entries to unit tests.
-- They are defined here to avoid cluttering W3C.Manifest
-- with functions that may not be needed to those who
-- just want to parse Manifest files.
-- TODO: They should probably be moved to W3C.Manifest after all.
mfEntryToTest :: TestEntry -> IO Test
mfEntryToTest (TestTurtleEval nm _ _ act res) = do
  parsedRDF <- parseFile testParser (nodeURI act) >>= return . fromEither :: IO TriplesGraph
  expectedRDF <- parseFile NTriplesParser (nodeURI res) >>= return . fromEither :: IO TriplesGraph
  return $ testCase (T.unpack nm) $ TU.assert $ isIsomorphic parsedRDF expectedRDF
mfEntryToTest (TestTurtleNegativeEval nm _ _ act) = do
  rdf <- parseFile testParser (nodeURI act) :: IO (Either ParseFailure TriplesGraph)
  return $ testCase (T.unpack nm) $ TU.assert $ isNotParsed rdf
mfEntryToTest (TestTurtlePositiveSyntax nm _ _ act) = do
  rdf <- parseFile testParser (nodeURI act) :: IO (Either ParseFailure TriplesGraph)
  return $ testCase (T.unpack nm) $ TU.assert $ isParsed rdf
mfEntryToTest (TestTurtleNegativeSyntax nm _ _ act) = do
  rdf <- parseFile testParser (nodeURI act) :: IO (Either ParseFailure TriplesGraph)
  return $ testCase (T.unpack nm) $ TU.assert $ isNotParsed rdf
mfEntryToTest x = error $ "unknown TestEntry pattern in mfEntryToTest: " ++ show x

isParsed :: Either a b -> Bool
isParsed (Left _) = False
isParsed (Right _) = True

isNotParsed :: Either a b -> Bool
isNotParsed = not . isParsed

nodeURI :: Node -> String
nodeURI = \(UNode u) -> T.unpack u

testParser :: TurtleParser
testParser = TurtleParser (Just mfBaseURI) Nothing
