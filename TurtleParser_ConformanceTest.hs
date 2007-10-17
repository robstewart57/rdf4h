import qualified Test.HUnit as T

import Text.Printf
import RDF
import TurtleParser
import qualified NTriplesParser as NT
import TriplesGraph
import Text.ParserCombinators.Parsec
import System.IO

--runAllCTests :: IO T.Counts
runAllCTests = allTests >>= return . T.TestList >>= runTest
  where runTest  = T.runTestText (T.putTextToHandle stdout True)
        allTests = mapM checkConformanceTest [1..30]

checkConformanceTest :: Int -> IO T.Test
checkConformanceTest i = 
  do 
    t1 <-  return (expected >>= assertLoaded (printf "expected%02d:" i :: String))
    t2 <-  return (input    >>= assertLoaded (printf "   input%02d:" i :: String))
    return $ T.TestList $ map T.TestCase [t1, t2]
  --assertEqual errMsg (ordered $ triplesOf
  where 
    errMsg = printf "Triples mismatch for test %02d." i :: String
    input :: IO (Either ParseFailure TriplesGraph)
    input = readFile (fpath i "ttl") >>= 
              return . runParser t_turtleDoc 0 "" >>= 
              return . (post_process Nothing) >>=
              return . handleResult
    expected :: IO (Either ParseFailure TriplesGraph)
    expected = readFile (fpath i "out") >>=
                 return . NT.parseString

assertLoaded :: String -> Either ParseFailure TriplesGraph -> T.Assertion
assertLoaded idStr (Left err) = T.assertFailure $ idStr  ++ show err
assertLoaded _      (Right _) = return ()

fpath :: Int -> String -> String    
fpath i ext = printf "data/ttl/conformance/test-%02d.%s" i ext

--t :: Int -> IO T.Counts
--t i = T.runTestTT $ T.TestCase $ checkConformanceTest i
--extractTs :: Either ParseFailure AvlGraph -> Triples
--extractTs (Left err) = 