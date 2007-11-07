import System.Environment
import NTriplesParser
import RDF
import TriplesGraph

-- |A simple main that just dumps the triples to stdout. Will add more later.
main = 
  getArgs >>= \t -> (parseFile $ head t) >>= \res ->
    case (res) of
      Left err -> print $ show err
      Right gr -> mapM_ (putStrLn . show) (triplesOf (gr :: TriplesGraph))
