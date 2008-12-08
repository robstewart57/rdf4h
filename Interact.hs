module Interact where

import Data.ByteString.Lazy.Char8(ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

import Text.RDF.Core
import Text.RDF.Utils
import Text.RDF.TriplesGraph(TriplesGraph)
import Text.RDF.MGraph(MGraph)
import Text.RDF.TriplesGraph
import Text.RDF.MGraph
import Text.RDF.NTriplesParser
import Text.RDF.TurtleParser
import Text.RDF.NTriplesSerializer
import Text.RDF.TurtleSerializer

-- |Load a Turtle file from the filesystem using the optional base URL 
-- (used to resolve relative URI fragments) and optional document URI
-- (used to resolve <> in the document). This function calls 'error' 
-- with an error message if unable to load the file, so it's only suitable
-- for interactive experimentation and should not otherwise be used.
loadTurtleFile :: forall gr. (Graph gr) => Maybe String -> Maybe String -> String -> IO gr
loadTurtleFile baseUrl docUri = _loadTurtle parseFile (mkTurtleParser baseUrl docUri)

-- |Load a Turtle file from a URL just like 'loadTurtleFile' does from the local
-- filesystem. See that function for explanation of args, etc.
loadTurtleURL  :: forall gr. (Graph gr) => Maybe String -> Maybe String -> String -> IO gr
loadTurtleURL baseUrl docUri  = _loadTurtle parseURL (mkTurtleParser baseUrl docUri)


mkTurtleParser :: Maybe String -> Maybe String -> TurtleParser
mkTurtleParser b d = TurtleParser (strToBaseUrl b) d

_loadTurtle :: forall p gr. (RdfParser p, Graph gr) => 
                  (p -> String -> IO (Either ParseFailure gr)) -> 
                  p -> String -> IO gr
_loadTurtle parseFunc parser location =
  do res <- parseFile parser location :: IO (Either ParseFailure gr)
     either (error . show) return res

strToBaseUrl :: Maybe String -> Maybe BaseUrl
strToBaseUrl = fmap (BaseUrl . B.pack)
