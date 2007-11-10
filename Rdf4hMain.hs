module Main where

import Text.RDF.Core
import qualified Text.RDF.TriplesGraph   as NG
import qualified Text.RDF.NTriplesParser as NP
import qualified Text.RDF.MGraph         as TG
import qualified Text.RDF.TurtleParser   as TP

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
     when (not quiet)
       (hPrintf stderr "      INPUT-URI:  %s\n\n" inputUri     >>
        hPrintf stderr "   INPUT-FORMAT:  %s\n"   inputFormat  >>
        hPrintf stderr " INPUT-BASE-URI:  %s\n\n" inputBaseUri >>
        hPrintf stderr "  OUTPUT-FORMAT:  %s\n"   outputFormat >>
        hPrintf stderr "OUTPUT-BASE-URI:  %s\n\n" outputBaseUri)
     let mInputUri  = if inputBaseUri == "-" then Nothing else Just (BaseUrl $ s2b inputBaseUri)
         docUri     = inputUri
     case (inputFormat, isUri $ s2b inputUri) of
       ("turtle",    True) -> TP.parseURL mInputUri docUri inputUri
                                >>= \(res :: Either ParseFailure TG.MGraph) -> write res
       ("turtle",   False) -> TP.parseFile mInputUri docUri inputUri
                                >>= \(res :: Either ParseFailure TG.MGraph) -> write res
       ("ntriples",  True) -> NP.parseURL inputUri
                                >>= \(res :: Either ParseFailure NG.TriplesGraph) -> write res
       ("ntriples", False) -> NP.parseFile inputUri
                                >>= \(res :: Either ParseFailure NG.TriplesGraph) -> write res
       (str     ,   _    ) -> putStrLn ("Invalid format: " ++ str) >> exitFailure
     --putStrLn $ show (opts, args)

write :: Graph gr => Either ParseFailure gr -> IO ()
write res =
  case res of
    (Left err) -> putStrLn (show err) >> exitWith (ExitFailure 1)
    (Right gr) -> mapM_ (putStrLn . show) (triplesOf gr)

getInputBaseUri :: String -> [String] -> [Flag] -> String
getInputBaseUri inputUri args flags =
  case (null $ tail args) of
    True  -> getWithDefault (InputBaseUri inputUri) flags
    False -> getWithDefault (InputBaseUri (head $ tail args)) flags

isUri :: ByteString -> Bool
isUri str = not (B.null post) && B.all isLetter pre
  where (pre, post) = B.break (== ':') str

getWithDefault :: Flag -> [Flag] -> String
getWithDefault def args = 
  case find (== def) args of
    Nothing  -> strValue def
    Just val -> strValue val

strValue :: Flag -> String
strValue (InputFormat s)   = s
strValue (InputBaseUri s)  = s
strValue (OutputFormat s)  = s
strValue (OutputBaseUri s) = s
strValue flag              = error $ "No string value for flag: " ++ show flag

--  getArgs >>= \t -> (TP.parseFile Nothing "" $ head t) >>= \res ->
--    case (res) of
--      Left err -> print $ show err
--      Right gr -> mapM_ (putStrLn . show) (triplesOf (gr :: TG.TriplesGraph))

data Flag 
 = Help | Quiet | Version
 | InputFormat String | InputBaseUri String
 | OutputFormat String | OutputBaseUri String
 deriving (Show)

instance Eq Flag where
  Help            == Help            = True
  Quiet           == Quiet           = True
  Version         == Version         = True
  InputFormat _   == InputFormat _   = True
  InputBaseUri _  == InputBaseUri _  = True
  OutputFormat _  == OutputFormat _  = True
  OutputBaseUri _ == OutputBaseUri _ = True
  _               == _               = False

header :: String
header = 
  "\nrdf4h: an RDF parser and serializer\n\n"                                ++
  "\nUsage: rdf4h [OPTION...] INPUT-URI [INPUT-BASE-URI]\n\n"                ++
  "  INPUT-URI       a filename, URI or '-' for standard input (stdin).\n"   ++
  "  INPUT-BASE-URI  the input/parser base URI or '-' for none.\n"           ++
  "    Default is INPUT-URI\n"                                               ++
  "    Equivalent to -I INPUT-BASE-URI, --input-base-uri INPUT-BASE-URI\n\n"

version :: String
version = "0.4"

options :: [OptDescr Flag]
options =
 [ Option ['h']  ["help"]                           (NoArg Help)   "Display this help, then exit"
 , Option ['q']  ["quiet"]                         (NoArg Quiet)   "No extra information messages to stderr"
 , Option ['v']  ["version"]                     (NoArg Version)   "Show version number"

 , Option ['i']  ["input"]        (ReqArg InputFormat  "FORMAT") $ "Set input format/parser to one of:\n" ++
                                                                   "  turtle      Turtle (default)\n" ++
                                                                   "  ntriples    N-Triples"
 , Option ['I']  ["input-base-uri"]  (ReqArg InputBaseUri "URI") $ "Set the input/parser base URI. '-' for none.\n" ++
                                                                   "  Default is INPUT-BASE-URI argument value.\n\n"

 , Option ['o']  ["output"]       (ReqArg OutputFormat "FORMAT") $ "Set output format/serializer to one of:\n" ++
                                                                   "  ntriples    N-Triples (default)\n" ++
                                                                   "  turtle      Turtle"
 , Option ['O'] ["output-base-uri"] (ReqArg OutputBaseUri "URI") $ "Set the output format/serializer base URI. '-' for none.\n" ++
                                                                   "  Default is input/parser base URI."
 ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = 
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError (userError ("\n\n" ++ concat errs ++ usageInfo header options))

