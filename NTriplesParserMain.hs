import Text.RDF.Core
import Text.RDF.TriplesGraph
import Text.RDF.NTriplesParser

import System.Environment

-- |A simple main that just dumps the triples to stdout. Will add more later.
main :: IO ()
main = 
  getArgs >>= \t -> (parseFile $ head t) >>= \res ->
    case (res) of
      Left err -> print $ show err
      Right gr -> mapM_ (putStrLn . show) (triplesOf (gr :: TriplesGraph))
