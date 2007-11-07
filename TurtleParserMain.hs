import System.Environment
import System.Exit
import TurtleParser
import RDF
import MGraph


main :: IO ()
main = 
  getArgs >>= \args -> 
    if length args /= 3
       then usage >> exitFailure
       else let baseUrl  = Just $ BaseUrl $ s2b $ args !! 0
                docUrl   = args !! 1
                filepath = args !! 2
            in parseFile baseUrl docUrl filepath >>= \res ->
              case (res) of
                Left err -> print $ show err
                Right gr -> mapM_ (putStrLn . show) (triplesOf (gr :: MGraph))

usage :: IO ()
usage = getProgName >>= \name -> putStrLn $ name ++ " baseUrl docUrl filepath"