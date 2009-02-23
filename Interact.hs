-- |This module re-exports most of the other modules of this library and also
-- defines some convenience methods for interactive experimentation, such as
-- simplified functions for parsing and serializing RDF, etc.
--
-- All the load functions can be used with any 'Graph' implementation, so you
-- must declare a type in order to help the type system disambiguate the 
-- @Graph gr@ constraint in those function's types.
-- 
-- Many of the simplified functions in this module call 'error' when there is 
-- a failure. This is so that you don't have to deal with 'Maybe' or 'Either'
-- return values while interacting. These functions are thus only intended
-- to be used when interactively exploring via ghci, and should not otherwise
-- be used.

module Interact where

import Data.ByteString.Lazy.Char8(ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

import Text.RDF.RDF4H.Core
import Text.RDF.RDF4H.Utils
import Text.RDF.RDF4H.TriplesGraph(TriplesGraph)
import Text.RDF.RDF4H.MGraph(MGraph)
import Text.RDF.RDF4H.TriplesGraph
import Text.RDF.RDF4H.MGraph
import Text.RDF.RDF4H.NTriplesParser
import Text.RDF.RDF4H.TurtleParser
import Text.RDF.RDF4H.NTriplesSerializer
import Text.RDF.RDF4H.TurtleSerializer

-- |Load a Turtle file from the filesystem using the optional base URL 
-- (used to resolve relative URI fragments) and optional document URI
-- (used to resolve <> in the document). 
-- 
-- This function calls 'error' with an error message if unable to load the file.
loadTurtleFile :: forall gr. (Graph gr) => Maybe String -> Maybe String -> String -> IO gr
loadTurtleFile baseUrl docUri = _load parseFile (mkTurtleParser baseUrl docUri)

-- |Load a Turtle file from a URL just like 'loadTurtleFile' does from the local
-- filesystem. See that function for explanation of args, etc.
loadTurtleURL  :: forall gr. (Graph gr) => Maybe String -> Maybe String -> String -> IO gr
loadTurtleURL baseUrl docUri  = _load parseURL (mkTurtleParser baseUrl docUri)

-- |Parse a Turtle document from the given 'ByteString' using the given @baseUrl@ and 
-- @docUri@, which have the same semantics as in the loadTurtle* functions.
parseTurtleString :: forall gr. (Graph gr) => Maybe String -> Maybe String -> ByteString -> gr
parseTurtleString baseUrl docUri = _parse parseString (mkTurtleParser baseUrl docUri)

mkTurtleParser :: Maybe String -> Maybe String -> TurtleParser
mkTurtleParser b d = TurtleParser ((BaseUrl . B.pack) `fmap` b) (B.pack `fmap` d)

-- |Load an NTriples file from the filesystem.
-- 
-- This function calls 'error' with an error message if unable to load the file.
loadNTriplesFile :: forall gr. (Graph gr) => String -> IO gr
loadNTriplesFile = _load parseFile NTriplesParser

-- |Load an NTriples file from a URL just like 'loadNTriplesFile' does from the local
-- filesystem. See that function for more info.
loadNTriplesURL :: forall gr. (Graph gr) => String -> IO gr
loadNTriplesURL  = _load parseURL  NTriplesParser

-- |Parse an NTriples document from the given 'ByteString', as 'loadNTriplesFile' does
-- from a file.
parseNTriplesString :: forall gr. (Graph gr) => ByteString -> gr
parseNTriplesString = _parse parseString NTriplesParser

-- Load a graph using the given parseFunc, parser, and the location (filesystem path
-- or HTTP URL), calling error with the 'ParseFailure' message if unable to load
-- or parse for any reason.
_load :: forall p gr. (RdfParser p, Graph gr) => 
            (p -> String -> IO (Either ParseFailure gr)) -> 
            p -> String -> IO gr
_load parseFunc parser location = parseFunc parser location >>= _handle

-- Use the given parseFunc and parser to parse the given 'ByteString', calling error
-- with the 'ParseFailure' message if unable to load or parse for any reason.
_parse :: forall p gr. (RdfParser p, Graph gr) => 
                       (p -> ByteString -> Either ParseFailure gr) -> 
                       p -> ByteString -> gr
_parse parseFunc parser rdfBs = either (error . show) id $ parseFunc parser rdfBs

-- Handle the result of an IO parse by returning the graph if parse was successful
-- and calling 'error' with the 'ParseFailure' error message if unsuccessful.
_handle :: forall gr. (Graph gr) => Either ParseFailure gr -> IO gr
_handle = either (error . show) return
