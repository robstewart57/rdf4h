import System.Environment
import NTriples
import RDF

-- |A dumb main that just dumps the triples to stdout. Will add more later.
main = 
  getArgs >>= \t -> (parseFile $ head t) >>= \res ->
    case (res) of
      Left err -> print $ show err
      Right xs -> mapM_ (putStrLn . show) (triplesOf xs)
