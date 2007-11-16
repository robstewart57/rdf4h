module Main where

import Text.RDF.Core
import Text.RDF.TriplesGraph
import qualified Text.RDF.NTriplesParser     as NP
import qualified Text.RDF.NTriplesSerializer as NS
import qualified Text.RDF.TurtleParser       as TP

import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as B

import System.Environment
import System.IO
import System.Exit
import System.Console.GetOpt

import Control.Monad

import Data.List
import Data.Char(isLetter)
import Text.Printf(hPrintf)

-- TODO: doesn't obey the output format yet, because there is no serializer for turtle yet.

main :: IO ()
main = 
  do (opts, args) <- (getArgs >>= compilerOpts)
     when (elem Help opts)
       (putStrLn (usageInfo header options) >> exitWith ExitSuccess)
     when (elem Version opts)
       (putStrLn version >> exitWith ExitSuccess)
     when (null args)
       (ioError (userError ("\n\n" ++ "INPUT-URI required\n\n" ++ usageInfo header options)))
     let quiet         = elem Quiet opts
         inputUri      = head args
         inputFormat   = getWithDefault (InputFormat "turtle") opts
         outputFormat  = getWithDefault (OutputFormat "ntriples") opts
         inputBaseUri  = getInputBaseUri inputUri args opts
         outputBaseUri = getWithDefault (OutputBaseUri inputBaseUri) opts
     unless (outputFormat == "ntriples")
       (hPrintf stderr ("'" ++ outputFormat ++ "' is not a valid output format. Supported output formats are: ntriples\n") >> exitWith (ExitFailure 1))
     when (not quiet)
       (hPrintf stderr "      INPUT-URI:  %s\n\n" inputUri     >>
        hPrintf stderr "   INPUT-FORMAT:  %s\n"   inputFormat  >>
        hPrintf stderr " INPUT-BASE-URI:  %s\n\n" inputBaseUri >>
        hPrintf stderr "  OUTPUT-FORMAT:  %s\n"   outputFormat >>
        hPrintf stderr "OUTPUT-BASE-URI:  %s\n\n" outputBaseUri)
     let mInputUri  = if inputBaseUri == "-" then Nothing else Just (BaseUrl $ s2b inputBaseUri)
         docUri     = inputUri
     case (inputFormat, isUri $ s2b inputUri) of
       -- we use TriplesGraph in all cases, since it preserves the ordering of triples
       ("turtle",    True) -> TP.parseURL mInputUri docUri inputUri
                                >>= \(res :: Either ParseFailure TriplesGraph) -> write res
       ("turtle",   False) -> (if inputUri /= "-" 
                                  then TP.parseFile mInputUri docUri inputUri 
                                  else getContents >>= return . TP.parseString mInputUri docUri)
                                >>= \(res :: Either ParseFailure TriplesGraph) -> write res
       ("ntriples",  True) -> NP.parseURL inputUri
                                >>= \(res :: Either ParseFailure TriplesGraph) -> write res
       ("ntriples", False) -> (if inputUri /= "-"
                                  then NP.parseFile inputUri
                                  else getContents >>= return . NP.parseString)
                                >>= \(res :: Either ParseFailure TriplesGraph) -> write res
       (str     ,   _    ) -> putStrLn ("Invalid format: " ++ str) >> exitFailure

write :: Graph gr => Either ParseFailure gr -> IO ()
write res =
  case res of
    (Left err) -> putStrLn (show err) >> exitWith (ExitFailure 1)
    (Right gr) -> NS.writeTriples stdout (triplesOf gr)

-- Get the input base URI from the argument list or flags, using the 
-- first string arg as the default if not found in string args (as
-- the second item in the list) or in the flags as an explicitly 
-- selected flag. If the user submitted both a 2nd commandline arg
-- after the INPUT-URI and used the -I/--input-base-uri arg, then
-- the -I/--input-base-uri value is used and the 2nd commandline
-- arg is silently discarded.
getInputBaseUri :: String -> [String] -> [Flag] -> String
getInputBaseUri inputUri args flags =
  case (null $ tail args) of
    True  -> getWithDefault (InputBaseUri inputUri) flags
    False -> getWithDefault (InputBaseUri (head $ tail args)) flags

-- Determine if the bytestring represents a URI, which is currently 
-- decided solely by checking for a colon in the string.
isUri :: ByteString -> Bool
isUri str = not (B.null post) && B.all isLetter pre
  where (pre, post) = B.break (== ':') str

-- Extract from the list of flags a flag of the same type as the first
-- flag argument, returning its string value; if there is no such flag,
-- return the string value of the first argument.
getWithDefault :: Flag -> [Flag] -> String
getWithDefault def args = 
  case find (== def) args of
    Nothing  -> strValue def
    Just val -> strValue val

-- Convert the flag to a string, which is only valid for flags that have
-- a string argument.
strValue :: Flag -> String
strValue (InputFormat s)   = s
strValue (InputBaseUri s)  = s
strValue (OutputFormat s)  = s
strValue (OutputBaseUri s) = s
strValue flag              = error $ "No string value for flag: " ++ show flag

-- The commandline arguments we accept. None are required.
data Flag 
 = Help | Quiet | Version
 | InputFormat String | InputBaseUri String
 | OutputFormat String | OutputBaseUri String
 deriving (Show)

-- Two flags are equal if they are of the same type, regardless of value: a
-- strange definition, but we never care about values when finding or comparing
-- them.
instance Eq Flag where
  Help            == Help            = True
  Quiet           == Quiet           = True
  Version         == Version         = True
  InputFormat _   == InputFormat _   = True
  InputBaseUri _  == InputBaseUri _  = True
  OutputFormat _  == OutputFormat _  = True
  OutputBaseUri _ == OutputBaseUri _ = True
  _               == _               = False

-- The top part of the usage output.
header :: String
header = 
  "\nrdf4h: an RDF parser and serializer\n\n"                                ++
  "\nUsage: rdf4h [OPTION...] INPUT-URI [INPUT-BASE-URI]\n\n"                ++
  "  INPUT-URI       a filename, URI or '-' for standard input (stdin).\n"   ++
  "  INPUT-BASE-URI  the input/parser base URI or '-' for none.\n"           ++
  "    Default is INPUT-URI\n"                                               ++
  "    Equivalent to -I INPUT-BASE-URI, --input-base-uri INPUT-BASE-URI\n\n"

-- The current version of the library as a whole; should always match what is in
-- the rdf4h.cabal file.
version :: String
version = "0.3"

options :: [OptDescr Flag]
options =
 [ Option ['h']  ["help"]                           (NoArg Help)   "Display this help, then exit"
 , Option ['q']  ["quiet"]                         (NoArg Quiet)   "No extra information messages to stderr"
 , Option ['v']  ["version"]                     (NoArg Version)   "Show version number\n\n"

 , Option ['i']  ["input"]        (ReqArg InputFormat  "FORMAT") $ "Set input format/parser to one of:\n" ++
                                                                   "  turtle      Turtle (default)\n" ++
                                                                   "  ntriples    N-Triples"
 , Option ['I']  ["input-base-uri"]  (ReqArg InputBaseUri "URI") $ "Set the input/parser base URI. '-' for none.\n" ++
                                                                   "  Default is INPUT-BASE-URI argument value.\n\n"

 , Option ['o']  ["output"]       (ReqArg OutputFormat "FORMAT") $ "Set output format/serializer to one of:\n" ++
                                                                   "  ntriples    N-Triples (default)\n" -- ++
                                                                   --"  turtle      Turtle"
 , Option ['O'] ["output-base-uri"] (ReqArg OutputBaseUri "URI") $ "Set the output format/serializer base URI. '-' for none.\n" ++
                                                                   "  Default is input/parser base URI."
 ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = 
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError (userError ("\n\n" ++ concat errs ++ usageInfo header options))

