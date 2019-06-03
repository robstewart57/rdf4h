{-# LANGUAGE OverloadedStrings #-}

module W3C.TurtleTest
  ( testsParsec
  , testsAttoparsec
  , mfBaseURITurtle
  ) where

import Test.Tasty
import qualified Test.Tasty.HUnit as TU

import Data.Semigroup ((<>))
import Data.Maybe (fromJust)
import qualified Data.Text as T

import W3C.Manifest
import W3C.W3CAssertions

import Data.RDF.Types
import Data.RDF.Query
import Text.RDF.RDF4H.TurtleParser
import Text.RDF.RDF4H.NTriplesParser
import Text.RDF.RDF4H.ParserUtils
import Data.RDF.Graph.TList

testsParsec :: String -> Manifest -> TestTree
testsParsec = runManifestTests . (`mfEntryToTest` testParserParsec)

testsAttoparsec :: String -> Manifest -> TestTree
testsAttoparsec = runManifestTests . (`mfEntryToTest` testParserAttoparsec)

mfEntryToTest :: String -> (String -> TurtleParserCustom) -> TestEntry -> TestTree
mfEntryToTest dir parser (TestTurtleEval nm _ _ act res) =
  let pathExpected = getFilePath dir res
      pathAction = getFilePath dir act
      parsedRDF   = (fromEither <$> parseFile (parser (nodeURI act)) pathAction) :: IO (RDF TList)
      expectedRDF = (fromEither <$> parseFile NTriplesParser pathExpected) :: IO (RDF TList)
  in TU.testCase (T.unpack nm) $ assertIsIsomorphic parsedRDF expectedRDF
mfEntryToTest dir parser (TestTurtleNegativeEval nm _ _ act) =
  let pathAction = getFilePath dir act
      rdf = parseFile (parser (nodeURI act)) pathAction :: IO (Either ParseFailure (RDF TList))
  in TU.testCase (T.unpack nm) $ assertIsNotParsed rdf
mfEntryToTest dir parser (TestTurtlePositiveSyntax nm _ _ act) =
  let pathAction = getFilePath dir act
      rdf = parseFile (parser (nodeURI act)) pathAction :: IO (Either ParseFailure (RDF TList))
  in TU.testCase (T.unpack nm) $ assertIsParsed rdf
mfEntryToTest dir parser (TestTurtleNegativeSyntax nm _ _ act) =
  let pathAction = getFilePath dir act
      rdf = parseFile (parser (nodeURI act)) pathAction :: IO (Either ParseFailure (RDF TList))
  in TU.testCase (T.unpack nm) $ assertIsNotParsed rdf
mfEntryToTest _ _ x = error $ "unknown TestEntry pattern in mfEntryToTest: " <> show x

-- [NOTE] Was previously: http://www.w3.org/2013/TurtleTests/
mfBaseURITurtle :: BaseUrl
mfBaseURITurtle = BaseUrl "http://w3c.github.io/rdf-tests/turtle/"

-- testParser :: TurtleParser
-- testParser = TurtleParser (Just mfBaseURITurtle) Nothing

testParserParsec :: String -> TurtleParserCustom
testParserParsec dUrl = TurtleParserCustom (Just mfBaseURITurtle) (Just . T.pack $ dUrl) Parsec

testParserAttoparsec :: String -> TurtleParserCustom
testParserAttoparsec dUrl = TurtleParserCustom (Just mfBaseURITurtle) (Just . T.pack $ dUrl) Attoparsec

getFilePath :: String -> Node -> String
getFilePath dir (UNode iri) = fixFilePath' iri
  where fixFilePath' = (dir <>) . T.unpack . fromJust . T.stripPrefix (unBaseUrl mfBaseURITurtle)
getFilePath _ _ = error "Unexpected node"
