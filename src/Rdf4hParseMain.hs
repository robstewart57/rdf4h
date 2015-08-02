{-# LANGUAGE OverloadedStrings,
             RankNTypes,
             ScopedTypeVariables,
             MultiParamTypeClasses #-}

module Main where

import Data.RDF.Types
import Data.RDF.TriplesGraph
import Text.RDF.RDF4H.NTriplesParser
import Text.RDF.RDF4H.NTriplesSerializer
import Text.RDF.RDF4H.TurtleParser
import Text.RDF.RDF4H.TurtleSerializer

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import System.Environment
import System.IO
import System.Exit
import System.Console.GetOpt

import Control.Monad

import Data.List
import qualified Data.Map as Map
import Data.Char(isLetter)
import Text.Printf(hPrintf)

-- TODO: cleanup and refactor main and elsewhere in this module

main :: IO ()
main =
  do (opts, args) <- getArgs >>= compilerOpts
     when (Help `elem` opts)
      (putStrLn (usageInfo header options) >> exitSuccess)
     when (null args)
      (ioError
        (userError
           ("\n\n" ++ "INPUT-URI required\n\n" ++ usageInfo header options)))
     let debug = Debug `elem` opts
         inputUri = head args
         inputFormat = getWithDefault (InputFormat "turtle") opts
         outputFormat = getWithDefault (OutputFormat "ntriples") opts
         inputBaseUri = getInputBaseUri inputUri args opts
         outputBaseUri = getWithDefault (OutputBaseUri inputBaseUri) opts
     unless (outputFormat == "ntriples" || outputFormat == "turtle")
       (hPrintf stderr
          ("'" ++
             outputFormat ++
               "' is not a valid output format. Supported output formats are: ntriples, turtle\n")
          >> exitWith (ExitFailure 1))
     when debug
       (hPrintf stderr "      INPUT-URI:  %s\n\n" inputUri >>
          hPrintf stderr "   INPUT-FORMAT:  %s\n" inputFormat
          >> hPrintf stderr " INPUT-BASE-URI:  %s\n\n" inputBaseUri
          >> hPrintf stderr "  OUTPUT-FORMAT:  %s\n" outputFormat
          >> hPrintf stderr "OUTPUT-BASE-URI:  %s\n\n" outputBaseUri)
     let mInputUri
           = if inputBaseUri == "-" then Nothing else
               Just (BaseUrl (T.pack inputBaseUri))
         docUri = Just $ T.pack inputUri
         emptyPms = PrefixMappings Map.empty
     case (inputFormat, isUri $ T.pack inputUri) of
         ("turtle", True) -> parseURL (TurtleParser mInputUri docUri)
                               inputUri
                               >>=
                               \ (res :: Either ParseFailure TriplesGraph) ->
                                 write outputFormat docUri emptyPms res
         ("turtle", False) -> (if inputUri /= "-" then
                                 parseFile (TurtleParser mInputUri docUri) inputUri else
                                 liftM (parseString (TurtleParser mInputUri docUri)) TIO.getContents)
                                >>=
                                \ (res :: Either ParseFailure TriplesGraph) ->
                                  write outputFormat docUri emptyPms res
         ("ntriples", True) -> parseURL NTriplesParser inputUri >>=
                                 \ (res :: Either ParseFailure TriplesGraph) ->
                                   write outputFormat Nothing emptyPms res
         ("ntriples", False) -> (if inputUri /= "-" then
                                   parseFile NTriplesParser inputUri else
                                   liftM (parseString NTriplesParser) TIO.getContents)
                                  >>=
                                  \ (res :: Either ParseFailure TriplesGraph) ->
                                    write outputFormat Nothing emptyPms res
         (str, _) -> putStrLn ("Invalid format: " ++ str) >> exitFailure

write :: forall rdf. (RDF rdf) => String -> Maybe T.Text -> PrefixMappings -> Either ParseFailure rdf -> IO ()
write format docUri pms res =
  case res of
    (Left (ParseFailure msg)) -> putStrLn msg >> exitWith (ExitFailure 1)
    (Right rdf)               -> doWriteRdf rdf
  where doWriteRdf rdf = case format of 
                           "turtle"   -> writeRdf (TurtleSerializer docUri pms) rdf
                           "ntriples" -> writeRdf NTriplesSerializer rdf
                           unknown    -> error $ "Unknown output format: " ++ unknown

-- Get the input base URI from the argument list or flags, using the
-- first string arg as the default if not found in string args (as
-- the second item in the list) or in the flags as an explicitly
-- selected flag. If the user submitted both a 2nd commandline arg
-- after the INPUT-URI and used the -I/--input-base-uri arg, then
-- the -I/--input-base-uri value is used and the 2nd commandline
-- arg is silently discarded.
getInputBaseUri :: String -> [String] -> [Flag] -> String
getInputBaseUri inputUri args flags =
  if null $ tail args then
  getWithDefault (InputBaseUri inputUri) flags else
  getWithDefault (InputBaseUri (head $ tail args)) flags

-- Determine if the bytestring represents a URI, which is currently
-- decided solely by checking for a colon in the string.
isUri :: T.Text -> Bool
isUri str = not (T.null post) && T.all isLetter pre
  where (pre, post) = T.break (== ':') str

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
 = Help | Debug
 | InputFormat String | InputBaseUri String
 | OutputFormat String | OutputBaseUri String
 deriving (Show)

-- Two flags are equal if they are of the same type, regardless of value: a
-- strange definition, but we never care about values when finding or comparing
-- them.
instance Eq Flag where
  Help            == Help            = True
  Debug           == Debug           = True
  InputFormat _   == InputFormat _   = True
  InputBaseUri _  == InputBaseUri _  = True
  OutputFormat _  == OutputFormat _  = True
  OutputBaseUri _ == OutputBaseUri _ = True
  _               == _               = False


-- The top part of the usage output.
header :: String
header =
  "\nrdf4h_parse: an RDF parser and serializer\n\n"                          ++
  "\nUsage: rdf4h_parse [OPTION...] INPUT-URI [INPUT-BASE-URI]\n\n"          ++
  "  INPUT-URI       a filename, URI or '-' for standard input (stdin).\n"   ++
  "  INPUT-BASE-URI  the input/parser base URI or '-' for none.\n"           ++
  "    Default is INPUT-URI\n"                                               ++
  "    Equivalent to -I INPUT-BASE-URI, --input-base-uri INPUT-BASE-URI\n\n"

options :: [OptDescr Flag]
options =
 [ Option "h"  ["help"]                           (NoArg Help)   "Display this help, then exit"
 , Option "d"  ["debug"]                         (NoArg Debug)   "Print debug info (like INPUT-BASE-URI used, etc.)"
 , Option "i"  ["input"]        (ReqArg InputFormat  "FORMAT") $ "Set input format/parser to one of:\n" ++
                                                                   "  turtle      Turtle (default)\n" ++
                                                                   "  ntriples    N-Triples"
 , Option "I"  ["input-base-uri"]  (ReqArg InputBaseUri "URI") $ "Set the input/parser base URI. '-' for none.\n" ++
                                                                   "  Default is INPUT-BASE-URI argument value.\n\n"

 , Option "o"  ["output"]       (ReqArg OutputFormat "FORMAT") $ "Set output format/serializer to one of:\n" ++
                                                                   "  ntriples    N-Triples (default)\n" ++
                                                                   "  turtle      Turtle"
 , Option "O" ["output-base-uri"] (ReqArg OutputBaseUri "URI") $ "Set the output format/serializer base URI. '-' for none.\n" ++
                                                                   "  Default is input/parser base URI."
 ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError (userError ("\n\n" ++ concat errs ++ usageInfo header options))

