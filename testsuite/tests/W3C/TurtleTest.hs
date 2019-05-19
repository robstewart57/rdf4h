{-# LANGUAGE OverloadedStrings #-}

module W3C.TurtleTest
  ( testsParsec
  , testsAttoparsec
  ) where

import Test.Tasty
import qualified Test.Tasty.HUnit as TU

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

testsParsec :: Manifest -> TestTree
testsParsec = runManifestTests (mfEntryToTest testParserParsec)

testsAttoparsec :: Manifest -> TestTree
testsAttoparsec = runManifestTests (mfEntryToTest testParserAttoparsec)

mfEntryToTest :: TurtleParserCustom -> TestEntry -> TestTree
mfEntryToTest parser (TestTurtleEval nm _ _ act' res') =
  let act = (UNode . fromJust . fileSchemeToFilePath) act'
      res = (UNode . fromJust . fileSchemeToFilePath) res'
      parsedRDF   = (fromEither <$> parseFile parser (nodeURI act)) :: IO (RDF TList)
      expectedRDF = (fromEither <$> parseFile NTriplesParser (nodeURI res)) :: IO (RDF TList)
  in TU.testCase (T.unpack nm) $ assertIsIsomorphic parsedRDF expectedRDF
mfEntryToTest parser (TestTurtleNegativeEval nm _ _ act') =
  let act = (UNode . fromJust . fileSchemeToFilePath) act'
      rdf = parseFile parser (nodeURI act) :: IO (Either ParseFailure (RDF TList))
  in TU.testCase (T.unpack nm) $ assertIsNotParsed rdf
mfEntryToTest parser (TestTurtlePositiveSyntax nm _ _ act') =
  let act = (UNode . fromJust . fileSchemeToFilePath) act'
      rdf = parseFile parser (nodeURI act) :: IO (Either ParseFailure (RDF TList))
  in TU.testCase (T.unpack nm) $ assertIsParsed rdf
mfEntryToTest parser (TestTurtleNegativeSyntax nm _ _ act') =
  let act = (UNode . fromJust . fileSchemeToFilePath) act'
      rdf = parseFile parser (nodeURI act) :: IO (Either ParseFailure (RDF TList))
  in TU.testCase (T.unpack nm) $ assertIsNotParsed rdf
mfEntryToTest _ x = error $ "unknown TestEntry pattern in mfEntryToTest: " ++ show x

mfBaseURITurtle :: BaseUrl
mfBaseURITurtle   = BaseUrl "http://www.w3.org/2013/TurtleTests/"

-- testParser :: TurtleParser
-- testParser = TurtleParser (Just mfBaseURITurtle) Nothing

testParserParsec :: TurtleParserCustom
testParserParsec = TurtleParserCustom (Just mfBaseURITurtle) Nothing Parsec

testParserAttoparsec :: TurtleParserCustom
testParserAttoparsec = TurtleParserCustom (Just mfBaseURITurtle) Nothing Attoparsec
