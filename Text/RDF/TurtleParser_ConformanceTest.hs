module Main where


import Text.RDF.Core
import Text.RDF.TurtleParser
import qualified Text.RDF.NTriplesParser as NT
import Text.RDF.TriplesGraph
import Text.RDF.GraphTestUtils

import Text.Printf

import System.IO

import Control.Monad

import qualified Test.HUnit as T

import qualified Data.ByteString.Lazy.Char8 as B

import Debug.Trace(trace)

main :: IO ()
main = putStrLn "Running TurtleParser_ConformanceTest..." >> runAllCTests >>= putStrLn . show


_debug = trace

-- A list of other tests to run, each entry of which is (directory, fname_without_ext).
otherTestFiles = [("data/ttl", "example1"),
                  ("data/ttl", "example2"),
                  ("data/ttl", "example3"),
                  ("data/ttl", "example5"),
                  ("data/ttl", "example6"),
                  ("data/ttl", "fawlty1")
                 ]

-- The Base URI to be used for all conformance tests:
testBaseUri :: String
testBaseUri  = "http://www.w3.org/2001/sw/DataAccess/df1/tests/"

mtestBaseUri :: Maybe BaseUrl
mtestBaseUri = Just $ BaseUrl $ B.pack testBaseUri

fpath :: String -> Int -> String -> String
fpath name i ext = printf "data/ttl/conformance/%s-%02d.%s" name i ext :: String

runAllCTests :: IO (T.Counts, Int)
runAllCTests = allTests >>= return . T.TestList >>= runTest
  where runTest       = T.runTestText (T.putTextToHandle stdout True)
        allGoodTests  = mapM checkGoodConformanceTest [0..30]
        allBadTests   = mapM checkBadConformanceTest [0..14]
        allOtherTests = mapM (uncurry checkGoodOtherTest) otherTestFiles
        allTests      = do ts1 <- allGoodTests
                           ts2 <- allBadTests
                           ts3 <- allOtherTests
                           return (ts1 ++ ts2 ++ ts3)

checkGoodConformanceTest :: Int -> IO T.Test
checkGoodConformanceTest i =
  do
    expGr <- loadExpectedGraph "test" i
    inGr  <- loadInputGraph    "test" i
    doGoodConformanceTest expGr inGr (printf "%02d" i :: String)
checkGoodOtherTest :: String -> String -> IO T.Test
checkGoodOtherTest dir fname =
  do 
    expGr <- loadExpectedGraph1 (printf "%s/%s.out" dir fname :: String)
    inGr  <- loadInputGraph1 dir fname
    doGoodConformanceTest expGr inGr fname

doGoodConformanceTest   :: Either ParseFailure TriplesGraph -> 
                           Either ParseFailure TriplesGraph -> 
                           String -> IO T.Test
doGoodConformanceTest expGr inGr testname =
  do
    t1 <-  return (return expGr >>= assertLoadSuccess (printf "expected (%s): " testname))
    t2 <-  return (return inGr  >>= assertLoadSuccess (printf "   input (%s): " testname))
    t3 <-  return $ assertEquivalent testname expGr inGr
    return $ T.TestList $ map T.TestCase [t1, t2, t3]

checkBadConformanceTest :: Int -> IO T.Test
checkBadConformanceTest i =
  do
    t <- return (loadInputGraph "bad" i >>= assertLoadFailure (show i))
    return $ T.TestCase t

-- Determines if graphs are equivalent, returning Nothing if so or else a diagnostic message.
-- First graph is expected graph, second graph is actual.
equivalent :: Graph gr => Either ParseFailure gr -> Either ParseFailure gr -> Maybe String
equivalent (Left _) _                = Nothing
equivalent _        (Left _)         = Nothing
equivalent (Right gr1) (Right gr2)   = {- _debug (show (length gr1ts, length gr2ts)) -} (test $! zip gr1ts gr2ts)
  where
    gr1ts = uordered $ triplesOf $ gr1
    gr2ts = uordered $ triplesOf $ gr2
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
    equalNodes (BNode fs1) (BNodeGen i) = B.reverse (value fs1) == s2b ("_:genid" ++ show i)
    equalNodes n1          n2           = n1 == n2

-- Returns a graph for a good ttl test that is intended to pass, and normalizes
-- triples into a format so that they can be compared with the expected output triples.
loadInputGraph :: String -> Int -> IO (Either ParseFailure TriplesGraph)
loadInputGraph name n =
  B.readFile (fpath name n "ttl") >>=
    return . parseString mtestBaseUri (mkDocUrl testBaseUri name n) >>= return . handleLoad
loadInputGraph1 :: String -> String -> IO (Either ParseFailure TriplesGraph)
loadInputGraph1 dir fname =
  B.readFile (printf "%s/%s.ttl" dir fname :: String) >>=
    return . parseString mtestBaseUri (mkDocUrl1 testBaseUri fname) >>= return . handleLoad

handleLoad :: Either ParseFailure TriplesGraph -> Either ParseFailure TriplesGraph
handleLoad res =
  case res of
    l@(Left _)  -> l
    (Right gr)  -> Right $ mkGraph (map normalize (triplesOf gr)) (baseUrl gr) (prefixMappings gr)

normalize :: Triple -> Triple
normalize t = let s' = normalizeN $ subjectOf t
                  p' = normalizeN $ predicateOf t
                  o' = normalizeN $ objectOf t
              in  triple s' p' o'
normalizeN :: Node -> Node
normalizeN (BNodeGen i) = BNode $ mkFastString (s2b $ "_:genid" ++ show i)
normalizeN n            = n

loadExpectedGraph :: String -> Int -> IO (Either ParseFailure TriplesGraph)
loadExpectedGraph name n = loadExpectedGraph1 (fpath name n "out")
loadExpectedGraph1 :: String -> IO (Either ParseFailure TriplesGraph)
loadExpectedGraph1 filename = B.readFile filename >>= return . NT.parseString 

assertLoadSuccess, assertLoadFailure :: String -> Either ParseFailure TriplesGraph -> T.Assertion
assertLoadSuccess idStr (Left (ParseFailure err)) = T.assertFailure $ idStr  ++ err
assertLoadSuccess _      (Right _) = return ()
assertLoadFailure _       (Left _)   = return ()
assertLoadFailure idStr _          = T.assertFailure $ "Bad test " ++ idStr ++ " loaded successfully."

assertEquivalent :: Graph gr => String -> Either ParseFailure gr -> Either ParseFailure gr -> T.Assertion
assertEquivalent testname r1 r2 =
  case equiv of
    Nothing    -> T.assert True
    (Just msg) -> fail $ "Graph " ++ testname ++ " not equivalent to expected:\n" ++ msg
  where equiv = equivalent r1 r2

_test :: Bool -> Int -> IO ()
_test testGood testNum = B.readFile fpath >>= f
  where
    fname = printf "%s-%02d.ttl" name testNum :: String
    fpath = "data/ttl/conformance/" ++ fname
    name = if testGood then "test" else "bad" :: String
    docUrl = mkDocUrl testBaseUri name testNum
    f :: B.ByteString -> IO ()
    f s = let result = parseString mtestBaseUri docUrl s
          in case result of
               (Left err) -> putStrLn $ "ERROR:" ++ show err
               (Right gr) -> mapM_ (putStrLn . show) (triplesOf (gr :: TriplesGraph))

mkDocUrl :: String -> String -> Int -> String
mkDocUrl baseDocUrl fname testNum = printf "%s%s-%02d.ttl" baseDocUrl fname testNum

mkDocUrl1 :: String -> String -> String
mkDocUrl1 baseDocUrl fname        = printf "%s%s.ttl" baseDocUrl fname

doTest :: Bool -> Int -> IO (T.Counts, Int)
doTest True  testNum = checkGoodConformanceTest testNum  >>= T.runTestText (T.putTextToHandle stdout True)
doTest False testNum = checkBadConformanceTest  testNum  >>= T.runTestText (T.putTextToHandle stdout True)
