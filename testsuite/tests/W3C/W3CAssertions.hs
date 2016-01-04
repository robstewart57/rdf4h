module W3C.W3CAssertions where

import qualified Data.Text as T
import           Data.RDF
import qualified Test.HUnit as TU
import           Test.Tasty
import           W3C.Manifest

runManifestTests mfEntryToTest manifest = 
    testGroup (T.unpack $ description manifest) $ map mfEntryToTest $ entries manifest
              
assertIsIsomorphic r1 r2 = do
  gr1 <- r1
  gr2 <- r2
  TU.assertBool "not isomorphic" (isIsomorphic gr1 gr2)
                  
assertIsParsed :: (Show rdf, RDF rdf) => IO (Either ParseFailure rdf) -> TU.Assertion
assertIsParsed r1 = do
  gr1 <- r1
  TU.assertBool ("unable to parse, reason:\n" ++ show gr1) (isParsed gr1)

assertIsNotParsed :: (Show rdf, RDF rdf) => IO (Either ParseFailure rdf) -> TU.Assertion
assertIsNotParsed r1 = do
  gr1 <- r1
  TU.assertBool ("parsed unexpectantly:\n" ++ show gr1) (not (isParsed gr1))

isParsed :: Either a b -> Bool
isParsed (Left _) = False
isParsed (Right _) = True

nodeURI :: Node -> String
nodeURI (UNode u) = T.unpack u

