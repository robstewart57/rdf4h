module W3C.TurtleTest where

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
import Data.RDF.Graph.TriplesList

tests :: Manifest -> TestTree
tests = runManifestTests mfEntryToTest

mfEntryToTest :: TestEntry -> TestTree
mfEntryToTest (TestTurtleEval nm _ _ act' res') =
  let act = (UNode . fromJust . fileSchemeToFilePath) act'
      res = (UNode . fromJust . fileSchemeToFilePath) res'
      parsedRDF   = parseFile testParser (nodeURI act) >>= return . fromEither :: IO TriplesList
      expectedRDF = parseFile NTriplesParser (nodeURI res) >>= return . fromEither :: IO TriplesList
  in TU.testCase (T.unpack nm) $ assertIsIsomorphic parsedRDF expectedRDF
mfEntryToTest (TestTurtleNegativeEval nm _ _ act') =
  let act = (UNode . fromJust . fileSchemeToFilePath) act'
      rdf = parseFile testParser (nodeURI act) :: IO (Either ParseFailure TriplesList)
  in TU.testCase (T.unpack nm) $ assertIsNotParsed rdf
mfEntryToTest (TestTurtlePositiveSyntax nm _ _ act') =
  let act = (UNode . fromJust . fileSchemeToFilePath) act'
      rdf = parseFile testParser (nodeURI act) :: IO (Either ParseFailure TriplesList)
  in TU.testCase (T.unpack nm) $ assertIsParsed rdf
mfEntryToTest (TestTurtleNegativeSyntax nm _ _ act') =
  let act = (UNode . fromJust . fileSchemeToFilePath) act'
      rdf = parseFile testParser (nodeURI act) :: IO (Either ParseFailure TriplesList)
  in TU.testCase (T.unpack nm) $ assertIsNotParsed rdf
mfEntryToTest x = error $ "unknown TestEntry pattern in mfEntryToTest: " ++ show x

mfBaseURITurtle :: BaseUrl
mfBaseURITurtle   = BaseUrl "http://www.w3.org/2013/TurtleTests/"

testParser :: TurtleParser
testParser = TurtleParser (Just mfBaseURITurtle) Nothing
