import qualified Test.HUnit as T

import Text.Printf
import RDF
import TurtleParser
import qualified NTriplesParser as NT
import TriplesGraph
import Text.ParserCombinators.Parsec
import System.IO
import Control.Monad
import GraphTestUtils
import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as B

main = runAllCTests >>= putStrLn . show

-- The Base URI to be used for all conformance tests:
testBaseUri  = "http://www.w3.org/2001/sw/DataAccess/df1/tests/"
mtestBaseUri = Just $ BaseUrl $ B.pack testBaseUri

fpath :: String -> Int -> String -> String    
fpath name i ext = printf "data/ttl/conformance/%s-%02d.%s" name i ext :: String

--runAllCTests :: IO T.Counts
runAllCTests = allTests >>= return . T.TestList >>= runTest
  where runTest  = T.runTestText (T.putTextToHandle stdout True)
        allGoodTests = mapM checkGoodConformanceTest [14..14] -- 0..30
        allBadTests  = mapM checkBadConformanceTest [0..14]
        allTests = allGoodTests >>= \ts -> allBadTests >>= \ts' -> return (ts ++ [])

checkGoodConformanceTest :: Int -> IO T.Test
checkGoodConformanceTest i = 
  do
    expGr <- expected "test" i
    inGr  <- input    "test" i 
    t1 <-  return (return expGr >>= assertLoadSuccess (printf "expected%02d:" i :: String))
    t2 <-  return (return inGr  >>= assertLoadSuccess (printf "   input%02d:" i :: String))
    t3 <-  return $ assertEquivalent i expGr inGr
    return $ T.TestList $ map T.TestCase [t1, t2, t3]
  --assertEqual errMsg (ordered $ triplesOf
  where 
    errMsg = printf "Triples mismatch for test %02d." i :: String

checkBadConformanceTest :: Int -> IO T.Test
checkBadConformanceTest i =
  do
    t <- return (input "bad" i >>= assertLoadFailure (show i))
    return $ T.TestCase t

-- Determines if graphs are equivalent, returning Nothing if so or else a diagnostic message.
equivalent :: Graph gr => Either ParseFailure gr -> Either ParseFailure gr -> Maybe String
equivalent (Left _) _        = Nothing
equivalent _        (Left _) = Nothing
equivalent (Right gr1) (Right gr2) = test $ zip t1s t2s
  where
    test []           = Nothing
    test ((t1,t2):ts) = 
      case compare t1 t2 of
        Nothing -> test ts
        err     -> err
    compare t1 t2 = if t1 == t2 then Nothing else Just ("[" ++ show t1 ++ "]\n[" ++ show t2 ++ "]")
    t1s = ordered $! triplesOf gr1
    t2s = ordered $! triplesOf gr2

input :: String -> Int -> IO (Either ParseFailure TriplesGraph)
input name n = readFile (fpath name n "ttl") >>=  return . parseString mtestBaseUri (testBaseUri ++ name)

expected :: String -> Int -> IO (Either ParseFailure TriplesGraph)
expected name n = readFile (fpath name n "out") >>= return . NT.parseString

assertLoadSuccess, assertLoadFailure :: String -> Either ParseFailure TriplesGraph -> T.Assertion
assertLoadSuccess idStr (Left err) = T.assertFailure $ idStr  ++ show err
assertLoadSuccess _      (Right _) = return ()
assertLoadFailure idStr (Left err) = return ()
assertLoadFailure idStr _          = T.assertFailure $ "Bad test " ++ idStr ++ " loaded successfully."

assertEquivalent :: Graph gr => Int -> Either ParseFailure gr -> Either ParseFailure gr -> T.Assertion
assertEquivalent i r1 r2 = 
  case equiv of
    Nothing    -> T.assert True
    (Just msg) -> fail $ "Graph " ++ show i ++ " not equivalent to expected:\n" ++ msg
  where equiv = equivalent r1 r2

test :: Bool -> Int -> IO ()
test testGood testNum = readFile fpath >>= f
  where
    fname = printf "%s-%02d.ttl" name testNum :: String
    fpath = "data/ttl/conformance/" ++ fname
    name = if testGood then "test" else "bad" :: String
    f s = case parseString mtestBaseUri (testBaseUri ++ fname) s of
            (Left err) -> putStrLn $ "ERROR:" ++ show err
            --(Right gr) -> putStrLn $ "Loaded " ++ show (length (triplesOf (gr::AvlGraph))) ++ " triples"
            (Right gr) -> mapM_ (putStrLn . show) (triplesOf (gr :: TriplesGraph))
