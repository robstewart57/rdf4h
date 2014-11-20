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

mfPath = "data/w3c/turtle/manifest.ttl"
mfBaseURI = "http://www.w3.org/2013/TurtleTests/"

tests :: [Test]
tests = [ buildTest allTurtleTests ]

allTurtleTests :: IO Test
allTurtleTests = do
  m <- loadManifest mfPath mfBaseURI
  return $ testGroup "W3C Turtle Tests" $ map (buildTest . mfEntryToTest) $ entries m

-- Functions to map manifest test entries to unit tests.
-- They are defined here to avoid cluttering W3C.Manifest
-- with functions that may not be needed to those who
-- just want to parse Manifest files.
-- TODO: They should probably be moved to W3C.Manifest after all.
mfEntryToTest :: TestEntry -> IO Test
mfEntryToTest (TestTurtleEval nm cmt apr act res) = do
  parsedRDF <- parseFile parserA (nodeURI act) >>= return . fromEither :: IO TriplesGraph
  expectedRDF <- parseFile parserB (nodeURI res) >>= return . fromEither :: IO TriplesGraph
  return $ testCase (T.unpack nm) $ TU.assert $ isIsomorphic parsedRDF expectedRDF
  where parserA = TurtleParser (Just (BaseUrl mfBaseURI)) (Just mfBaseURI)
        parserB = TurtleParser (Just (BaseUrl mfBaseURI)) (Just mfBaseURI)
        nodeURI = \(UNode u) -> T.unpack $ T.concat ["data/w3c/turtle/", last $ T.split (\c -> c == '/') u]
mfEntryToTest (TestTurtleNegativeEval nm cmt apr act) = do
  parsedRDF <- parseFile parser (nodeURI act) :: IO (Either ParseFailure TriplesGraph)
  return $ testCase (T.unpack nm) $ TU.assert $ leftIsTrue parsedRDF
  where parser = TurtleParser (Just (BaseUrl mfBaseURI)) (Just mfBaseURI)
        nodeURI = \(UNode u) -> T.unpack $ T.concat ["data/w3c/turtle/", last $ T.split (\c -> c == '/') u]
mfEntryToTest (TestTurtlePositiveSyntax nm cmt apr act) = do
  parsedRDF <- parseFile parser (nodeURI act) :: IO (Either ParseFailure TriplesGraph)
  return $ testCase (T.unpack nm) $ TU.assert $ rightIsTrue parsedRDF
  where parser = TurtleParser (Just (BaseUrl mfBaseURI)) (Just mfBaseURI)
        nodeURI = \(UNode u) -> T.unpack $ T.concat ["data/w3c/turtle/", last $ T.split (\c -> c == '/') u]
mfEntryToTest (TestTurtleNegativeSyntax nm cmt apr act) = do
  parsedRDF <- parseFile parser (nodeURI act) :: IO (Either ParseFailure TriplesGraph)
  return $ testCase (T.unpack nm) $ TU.assert $ leftIsTrue parsedRDF
  where parser = TurtleParser (Just (BaseUrl mfBaseURI)) (Just mfBaseURI)
        nodeURI = \(UNode u) -> T.unpack $ T.concat ["data/w3c/turtle/", last $ T.split (\c -> c == '/') u]

rightIsTrue :: Either a b -> Bool
rightIsTrue (Left _) = False
rightIsTrue (Right _) = True

leftIsTrue :: Either a b -> Bool
leftIsTrue (Left _) = True
leftIsTrue (Right _) = False
