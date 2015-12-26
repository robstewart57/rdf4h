module W3C.RdfXmlTest where

import Data.Maybe (fromJust)
import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as TU
import qualified Data.Text as T
import System.Directory

import W3C.Manifest

import Data.RDF.Types
import Data.RDF.Query
import Text.RDF.RDF4H.XmlParser
import Text.RDF.RDF4H.NTriplesParser
import Data.RDF.Graph.TriplesList

suiteFilesDir = "data/w3c/rdf-xml/"

mfPath = T.concat [suiteFilesDir, "manifest.ttl"]
mfBaseURI = BaseUrl "http://www.w3.org/2013/RDFXMLTests/"

tests :: [Test]
tests = [ buildTest allRdfXmlTests ]

allRdfXmlTests :: IO Test
allRdfXmlTests = do
  dir <- getCurrentDirectory
  let fileSchemeURI = T.pack ("file://" ++ dir ++ "/" ++ T.unpack suiteFilesDir)
  m <- loadManifest mfPath fileSchemeURI
  return $ testGroup (T.unpack $ description m) $ map (buildTest . mfEntryToTest) $ entries m

-- Functions to map manifest test entries to unit tests.
-- They are defined here to avoid cluttering W3C.Manifest
-- with functions that may not be needed to those who
-- just want to parse Manifest files.
-- TODO: They should probably be moved to W3C.Manifest after all.
mfEntryToTest :: TestEntry -> IO Test
mfEntryToTest (TestXMLEval nm _ _ act' res') = do
  let act = (UNode . fromJust . fileSchemeToFilePath) act'
  let res = (UNode . fromJust . fileSchemeToFilePath) res'
  parsedRDF <- parseFile testParser (nodeURI act) >>= return . fromEither :: IO TriplesList
  expectedRDF <- parseFile NTriplesParser (nodeURI res) >>= return . fromEither :: IO TriplesList
  return $ testCase (T.unpack nm) $ TU.assert $ isIsomorphic parsedRDF expectedRDF
mfEntryToTest (TestXMLNegativeSyntax nm _ _ act') = do
  let act = (UNode . fromJust . fileSchemeToFilePath) act'
  rdf <- parseFile testParser (nodeURI act) :: IO (Either ParseFailure TriplesList)
  return $ testCase (T.unpack nm) $ TU.assert $ isNotParsed rdf
mfEntryToTest x = error $ "unknown TestEntry pattern in mfEntryToTest: " ++ show x

isParsed :: Either a b -> Bool
isParsed (Left _) = False
isParsed (Right _) = True

isNotParsed :: Either a b -> Bool
isNotParsed = not . isParsed

nodeURI :: Node -> String
nodeURI = \(UNode u) -> T.unpack u

testParser :: XmlParser
testParser = XmlParser (Just mfBaseURI) Nothing
