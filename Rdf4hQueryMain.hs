module Main where

import System.Console.GetOpt

main :: IO ()
main = putStrLn "Not Implemented Yet"

data Flag
 = Help | Version | Verbose
 | InputFormat String | InputBaseUri String
 deriving (Show)

header :: String
header =
  "\nrdf4h_query: utility for querying RDF data\n\n"                         ++
  "\nUsage: rdf4h_query [OPTION...] INPUT-URI [INPUT-BASE-URI]\n\n"          ++
  "  INPUT-URI       a filename, URI or '-' for standard input (stdin).\n"   ++
  "  INPUT-BASE-URI  the input/parser base URI or '-' for none.\n"           ++
  "    Default is INPUT-URI\n"                                               ++
  "    Equivalent to -I INPUT-BASE-URI, --input-base-uri INPUT-BASE-URI\n\n"

options :: [OptDescr Flag]
options =
 [ Option ['h']  ["help"]                           (NoArg Help)   "Display this help, then exit"
 , Option ['v']  ["verbose"]                     (NoArg Verbose)   "Display extra information messages to stderr"
 , Option ['V']  ["version"]                     (NoArg Version)   "Show version number\n\n"

 , Option ['i']  ["input"]        (ReqArg InputFormat  "FORMAT") $ "Set input format/parser to one of:\n" ++
                                                                   "  turtle      Turtle (default)\n" ++
                                                                   "  ntriples    N-Triples"
 , Option ['I']  ["input-base-uri"]  (ReqArg InputBaseUri "URI") $ "Set the input/parser base URI. '-' for none.\n" ++
                                                                   "  Default is INPUT-BASE-URI argument value.\n\n"

 ]
-- The current version of the executable, which for the moment is the same as
-- the version for the library as a whole, as given in rdf4h.cabal.
version :: String
version = "0.6.0"
