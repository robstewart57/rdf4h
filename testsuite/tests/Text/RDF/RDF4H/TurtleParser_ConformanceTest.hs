module Text.RDF.RDF4H.TurtleParser_ConformanceTest where

-- Testing imports
import Test.Tasty
import Test.Tasty.HUnit as TU

-- Import common libraries to facilitate tests
import Control.Monad (liftM)
-- import Data.RDF.GraphTestUtils
import Data.RDF.Query
import Data.RDF.Graph.TList
import Data.RDF.Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf
import Text.RDF.RDF4H.TurtleParser

tests :: TestTree
tests = testGroup "TurtleParser" allCTests

-- A list of other tests to run, each entry of which is (directory, fname_without_ext).
otherTestFiles :: [(String, String)]
otherTestFiles = [("data/ttl", "example1"),
                  ("data/ttl", "example2"),
                  ("data/ttl", "example3"),
                  ("data/ttl", "example5"),
                  ("data/ttl", "example6"),
--                  ("data/ttl", "example7"), -- rdf4h URIs support RFC3986, not unicode IRIs in RFC3987
                  ("data/ttl", "example8"),
                  ("data/ttl", "example9"),
                  ("data/ttl", "fawlty1")
                 ]

-- The Base URI to be used for all conformance tests:
testBaseUri :: String
testBaseUri  = "http://www.w3.org/2001/sw/DataAccess/df1/tests/"

mtestBaseUri :: Maybe BaseUrl
mtestBaseUri = Just $ BaseUrl $ T.pack testBaseUri

fpath :: String -> Int -> String -> String
fpath name i ext = printf "data/ttl/conformance/%s-%02d.%s" name i ext :: String

allCTests :: [TestTree]
allCTests = ts1 ++ ts2 ++ ts3
   where
        ts1 = map (checkGoodConformanceTest) [0..30]
        ts2 = map (checkBadConformanceTest) [0..14]
        ts3 = map (uncurry checkGoodOtherTest) otherTestFiles

checkGoodConformanceTest :: Int -> TestTree
checkGoodConformanceTest i =
  let expGr = loadExpectedGraph "test" i
      inGr  = loadInputGraph    "test" i
  in doGoodConformanceTest expGr inGr (printf "test-%d" i :: String)

checkGoodOtherTest :: String -> String -> TestTree
checkGoodOtherTest dir fname =
    let expGr = loadExpectedGraph1 (printf "%s/%s.out" dir fname :: String)
        inGr  = loadInputGraph1 dir fname
    in doGoodConformanceTest expGr inGr $ printf "turtle-%s" fname

doGoodConformanceTest   :: IO (Either ParseFailure (RDF TList)) ->
                           IO (Either ParseFailure (RDF TList)) ->
                           String -> TestTree
doGoodConformanceTest expGr inGr testname =
    let t1 = assertLoadSuccess (printf "expected (%s): " testname) expGr
        t2 = assertLoadSuccess (printf "   input (%s): " testname) inGr
        t3 = assertEquivalent testname expGr inGr
    in testGroup (printf "conformance-%s" testname) $ map (uncurry testCase) [("loading-expected-graph-data", t1), ("loading-input-graph-data", t2), ("comparing-graphs", t3)]

checkBadConformanceTest :: Int -> TestTree
checkBadConformanceTest i =
  let t = assertLoadFailure (show i) (loadInputGraph "bad" i)
  in testCase (printf "loading-test-%d-negative" i) t

-- Determines if graphs are equivalent, returning Nothing if so or else a diagnostic message.
-- First graph is expected graph, second graph is actual.
equivalent :: Rdf a => Either ParseFailure (RDF a) -> Either ParseFailure (RDF a) -> Maybe String
equivalent (Left _) _                = Nothing
equivalent _        (Left _)         = Nothing
equivalent (Right gr1) (Right gr2)   = test $! zip gr1ts gr2ts
  where
    gr1ts = uordered $ triplesOf gr1
    gr2ts = uordered $ triplesOf gr2
    test []           = Nothing
    test ((t1,t2):ts) =
      case compareTriple t1 t2 of
        Nothing -> test ts
        err     -> err
    compareTriple t1 t2 =
      if equalNodes s1 s2 && equalNodes p1 p2 && equalNodes o1 o2
        then Nothing
        else Just ("Expected:\n  " ++ show t1 ++ "\nFound:\n  " ++ show t2 ++ "\n")
      where
        (s1, p1, o1) = f t1
        (s2, p2, o2) = f t2
        f t = (subjectOf t, predicateOf t, objectOf t)
    -- equalNodes (BNode fs1) (BNodeGen i) = T.reverse fs1 == T.pack ("_:genid" ++ show i)
    -- equalNodes (BNode fs1) (BNodeGen i) = fs1 == T.pack ("_:genid" ++ show i)

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
    equalNodes n1          n2           = n1 == n2

-- Returns a graph for a good ttl test that is intended to pass, and normalizes
-- triples into a format so that they can be compared with the expected output triples.
loadInputGraph :: String -> Int -> IO (Either ParseFailure (RDF TList))
loadInputGraph name n =
  TIO.readFile (fpath name n "ttl") >>=
  return . parseString (TurtleParser mtestBaseUri (mkDocUrl testBaseUri name n)) >>= return . handleLoad

loadInputGraph1 :: String -> String -> IO (Either ParseFailure (RDF TList))
loadInputGraph1 dir fname =
  TIO.readFile (printf "%s/%s.ttl" dir fname :: String) >>=
  return . parseString (TurtleParser mtestBaseUri (mkDocUrl1 testBaseUri fname)) >>= return . handleLoad

handleLoad :: Either ParseFailure (RDF TList) -> Either ParseFailure (RDF TList)
handleLoad res =
  case res of
    l@(Left _)  -> l
    (Right gr)  -> Right $ mkRdf (map normalize (triplesOf gr)) (baseUrl gr) (prefixMappings gr)

normalize :: Triple -> Triple
normalize t = let s' = normalizeN $ subjectOf t
                  p' = normalizeN $ predicateOf t
                  o' = normalizeN $ objectOf t
              in  triple s' p' o'
normalizeN :: Node -> Node
normalizeN (BNodeGen i) = BNode (T.pack $ "_:genid" ++ show i)
normalizeN n            = n

loadExpectedGraph :: String -> Int -> IO (Either ParseFailure (RDF TList))
loadExpectedGraph name n = loadExpectedGraph1 (fpath name n "out")
loadExpectedGraph1 :: String -> IO (Either ParseFailure (RDF TList))
loadExpectedGraph1 fname =
  liftM (parseString (TurtleParser mtestBaseUri (mkDocUrl1 testBaseUri fname))) (TIO.readFile fname)

assertLoadSuccess, assertLoadFailure :: String -> IO (Either ParseFailure (RDF TList)) -> TU.Assertion
assertLoadSuccess idStr exprGr = do
  g <- exprGr
  case g of
    Left (ParseFailure err) -> TU.assertFailure $ idStr  ++ err
    Right _ -> return ()

assertLoadFailure idStr exprGr = do
  g <- exprGr
  case g of
    Left _ -> return ()
    Right _ -> TU.assertFailure $ "Bad test " ++ idStr ++ " loaded successfully."

assertEquivalent :: Rdf a => String -> IO (Either ParseFailure (RDF a)) -> IO (Either ParseFailure (RDF a)) -> TU.Assertion
assertEquivalent testname r1 r2 = do
  gr1 <- r1
  gr2 <- r2
  case equivalent gr1 gr2 of
    Nothing    -> TU.assert True
    (Just msg) -> fail $ "Graph " ++ testname ++ " not equivalent to expected:\n" ++ msg

mkDocUrl :: String -> String -> Int -> Maybe T.Text
mkDocUrl baseDocUrl fname testNum = Just $ T.pack $ printf "%s%s-%02d.ttl" baseDocUrl fname testNum

mkDocUrl1 :: String -> String -> Maybe T.Text
mkDocUrl1 baseDocUrl fname        = Just $ T.pack $ printf "%s%s.ttl" baseDocUrl fname
