import qualified Test.HUnit as T

import Text.Printf
import RDF
import TurtleParser
import qualified NTriplesParser as NT
import TriplesGraph
import Text.ParserCombinators.Parsec
import System.IO
import Control.Monad

main = runAllCTests >>= putStrLn . show

-- The Base URI to be used for all conformance tests:
testBaseUri  = "http://www.w3.org/2001/sw/DataAccess/df1/tests/"
mtestBaseUri = Just $ BaseUrl testBaseUri

fpath :: String -> Int -> String -> String    
fpath name i ext = printf "data/ttl/conformance/%s-%02d.%s" name i ext :: String

--runAllCTests :: IO T.Counts
runAllCTests = allTests >>= return . T.TestList >>= runTest
  where runTest  = T.runTestText (T.putTextToHandle stdout True)
        allGoodTests = mapM checkGoodConformanceTest [0..30]
        allBadTests  = mapM checkBadConformanceTest [0..14]
        allTests = allGoodTests >>= \ts -> allBadTests >>= \ts' -> return (ts ++ ts')

checkGoodConformanceTest :: Int -> IO T.Test
checkGoodConformanceTest i = 
  do 
    t1 <-  return (expected "test" i >>= assertLoadSuccess (printf "expected%02d:" i :: String))
    t2 <-  return (input    "test" i >>= assertLoadSuccess (printf "   input%02d:" i :: String))
    return $ T.TestList $ map T.TestCase [t1, t2]
  --assertEqual errMsg (ordered $ triplesOf
  where 
    errMsg = printf "Triples mismatch for test %02d." i :: String

checkBadConformanceTest :: Int -> IO T.Test
checkBadConformanceTest i =
  do
    t <- return (input "bad" i >>= assertLoadFailure (show i))
    return $ T.TestCase t

equivalent :: Graph gr => gr -> gr -> Bool
equivalent gr1 gr2 = False

input :: String -> Int -> IO (Either ParseFailure TriplesGraph)
input name n = readFile (fpath name n "ttl") >>=  return . parseString mtestBaseUri testBaseUri

expected :: String -> Int -> IO (Either ParseFailure TriplesGraph)
expected name n = readFile (fpath name n "out") >>= return . NT.parseString

assertLoadSuccess, assertLoadFailure :: String -> Either ParseFailure TriplesGraph -> T.Assertion
assertLoadSuccess idStr (Left err) = T.assertFailure $ idStr  ++ show err
assertLoadSuccess _      (Right _) = return ()
assertLoadFailure idStr (Left err) = return ()
assertLoadFailure idStr _          = T.assertFailure $ "Bad test " ++ idStr ++ " loaded successfully."

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
