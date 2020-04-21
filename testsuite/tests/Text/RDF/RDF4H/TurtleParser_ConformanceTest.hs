{-# LANGUAGE OverloadedStrings #-}

module Text.RDF.RDF4H.TurtleParser_ConformanceTest
  ( tests,
  )
where

-- Testing imports

-- Import common libraries to facilitate tests

import Control.Applicative ((<|>))
import Data.RDF.Graph.TList
import Data.RDF.Query
import Data.RDF.Types
import Data.Semigroup ((<>))
import Data.String (fromString)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit as TU
import Text.Printf
import Text.RDF.RDF4H.TurtleParser

-- tests :: TestTree
-- tests = testGroup "TurtleParser" allCTests

-- A list of other tests to run, each entry of which is (directory, fname_without_ext).
otherTestFiles :: [(String, String)]
otherTestFiles =
  [ ("data/ttl", "example1"),
    ("data/ttl", "example2"),
    ("data/ttl", "example3"),
    ("data/ttl", "example5"),
    ("data/ttl", "example6"),
    ("data/ttl", "example7"),
    ("data/ttl", "example8"),
    ("data/ttl", "example9"),
    ("data/ttl", "example10"),
    ("data/ttl", "example11"),
    ("data/ttl", "example12"),
    ("data/ttl", "fawlty1")
  ]

-- The Base URI to be used for all conformance tests:
testBaseUri :: Text
testBaseUri = "http://www.w3.org/2001/sw/DataAccess/df1/tests/"

mtestBaseUri :: Maybe BaseUrl
mtestBaseUri = Just . BaseUrl $ testBaseUri

tests :: [TestTree]
tests =
  baseUrlTests
    : fmap (uncurry checkGoodOtherTest) otherTestFiles

baseUrlTests :: TestTree
baseUrlTests =
  let t1 = assertBaseUrlSuccess (parseFile (TurtleParser Nothing Nothing) "data/ttl/example13.ttl") "http://example.org" "baseUrl-test1"
   in testGroup "baseUrl-tests" $
        fmap (uncurry testCase) [("baseUrl-test1", t1)]

assertBaseUrlSuccess :: IO (Either ParseFailure (RDF TList)) -> Text -> String -> TU.Assertion
assertBaseUrlSuccess exprGr expectedBUrl testLabel = do
  g <- exprGr
  case g of
    Left (ParseFailure err) -> TU.assertFailure err
    Right g' ->
      case baseUrl g' of
        Just (BaseUrl bUrl) ->
          if bUrl == expectedBUrl
            then return ()
            else TU.assertFailure $ "wrong base URL: " <> testLabel
        Nothing -> TU.assertFailure $ "no base URL: " <> testLabel

checkGoodOtherTest :: String -> String -> TestTree
checkGoodOtherTest dir fname =
  let expGr = loadExpectedGraph1 (printf "%s/%s.out" dir fname :: String)
      inGr = loadInputGraph1 dir fname
   in doGoodConformanceTest expGr inGr $ printf "turtle-%s" fname

doGoodConformanceTest ::
  IO (Either ParseFailure (RDF TList)) ->
  IO (Either ParseFailure (RDF TList)) ->
  String ->
  TestTree
doGoodConformanceTest expGr inGr testname =
  let t1 = assertLoadSuccess (printf "expected (%s): " testname) expGr
      t2 = assertLoadSuccess (printf "   input (%s): " testname) inGr
      t3 = assertEquivalent testname expGr inGr
   in testGroup (printf "conformance-%s" testname) $ fmap (uncurry testCase) [("loading-expected-graph-data", t1), ("loading-input-graph-data", t2), ("comparing-graphs", t3)]

-- Determines if graphs are equivalent, returning Nothing if so or else a diagnostic message.
-- First graph is expected graph, second graph is actual.
equivalent :: Rdf a => Either ParseFailure (RDF a) -> Either ParseFailure (RDF a) -> Maybe String
equivalent (Left e) _ = Just $ "Parse failure of the expected graph: " <> show e
equivalent _ (Left e) = Just $ "Parse failure of the input graph: " <> show e
equivalent (Right gr1) (Right gr2) = checkSize <|> (test $! zip gr1ts gr2ts)
  where
    gr1ts = uordered $ triplesOf gr1
    gr2ts = uordered $ triplesOf gr2
    length1 = length gr1ts
    length2 = length gr2ts
    checkSize = if (length1 == length2) then Nothing else (Just $ "Size different. Expected: " <> (show length1) <> ", got: " <> (show length2))
    test [] = Nothing
    test ((t1, t2) : ts) = maybe (test ts) pure (compareTriple t1 t2)
    compareTriple t1@(Triple s1 p1 o1) t2@(Triple s2 p2 o2) =
      if equalNodes s1 s2 && equalNodes p1 p2 && equalNodes o1 o2
        then Nothing
        else Just ("Expected:\n  " <> show t1 <> "\nFound:\n  " <> show t2 <> "\n")
    -- I'm not sure it's right to compare blank nodes with generated
    -- blank nodes. This is because parsing an already generated blank
    -- node is parsed as a blank node. Moreover, a parser is free to
    -- generate the blank node how ever they wish. E.g. parsing [] could be:
    --
    -- _:genid1
    --
    -- or
    --
    -- _:Bb71dd4e4b81c097db8d7f79078bbc7c0
    --
    -- which just so happens to be what Apache Jena just created when
    -- [] was parsed.
    equalNodes (BNode _) (BNodeGen _) = True
    equalNodes (BNodeGen _) (BNode _) = True
    equalNodes (BNodeGen _) (BNodeGen _) = True
    equalNodes (BNode _) (BNode _) = True
    equalNodes n1 n2 = n1 == n2

loadInputGraph1 :: String -> String -> IO (Either ParseFailure (RDF TList))
loadInputGraph1 dir fname = parseFile parserConfig path
  where
    path = printf "%s/%s.ttl" dir fname :: String
    parserConfig = TurtleParser mtestBaseUri (mkDocUrl1 testBaseUri fname)

loadExpectedGraph1 :: String -> IO (Either ParseFailure (RDF TList))
loadExpectedGraph1 fname =
  parseFile (TurtleParser mtestBaseUri (mkDocUrl1 testBaseUri fname)) fname

assertLoadSuccess :: String -> IO (Either ParseFailure (RDF TList)) -> TU.Assertion
assertLoadSuccess idStr exprGr = do
  g <- exprGr
  case g of
    Left (ParseFailure err) -> TU.assertFailure $ idStr <> err
    Right _ -> return ()

assertEquivalent :: Rdf a => String -> IO (Either ParseFailure (RDF a)) -> IO (Either ParseFailure (RDF a)) -> TU.Assertion
assertEquivalent testname r1 r2 = do
  gr1 <- r1
  gr2 <- r2
  case equivalent gr1 gr2 of
    Nothing -> return ()
    (Just msg) -> fail $ "Graph " <> testname <> " not equivalent to expected:\n" <> msg

mkDocUrl1 :: Text -> String -> Maybe Text
mkDocUrl1 baseDocUrl fname = Just . fromString $ printf "%s%s.ttl" baseDocUrl fname
