module Text.RDF.TurtleSerializer where

import Text.RDF.Core
import Text.RDF.Namespace

import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as B

import Text.PrettyPrint.HughesPJ

import System.IO

writeGraph :: Graph gr => Handle -> gr -> IO ()
writeGraph h gr = writeHeader h bUrl pms >> writeTriples h bUrl pms ts
  where
    bUrl = baseUrl gr
    pms  = prefixMappings gr
    ts   = triplesOf gr

writeHeader :: Handle -> Maybe BaseUrl -> PrefixMappings -> IO ()
writeHeader h bUrl pms = writeBase h bUrl >> writePrefixes h pms

writeBase :: Handle -> Maybe BaseUrl -> IO ()
writeBase h Nothing               = return ()
writeBase h (Just (BaseUrl bUrl)) = hPutStr h "@base " >> B.hPutStrLn h bUrl

writePrefixes :: Handle -> PrefixMappings -> IO ()
writePrefixes h pms = mapM_ (writePrefix h) (toPMList pms)

writePrefix :: Handle -> (ByteString, ByteString) -> IO ()
writePrefix h (pre, uri) =
  hPutStr h "@prefix " >> B.hPutStr h pre >> hPutChar h ':' >>
  B.hPutStrLn h uri

writeTriples :: Handle -> Maybe BaseUrl -> PrefixMappings -> Triples -> IO ()
writeTriples _ _    _   []     = return ()
writeTriples h bUrl pms (t:ts) = return ()

--writeSubjectGroup :: Handle -> Maybe BaseUrl -> PrefixMappings -> Triples -> IO ()
--writeSubjectGroup


subj1 = unode $ s2b "http://example.com/subj"
pred1 = unode $ s2b "http://example.com/pred"
obj1  = typedL (s2b "hello, world") (mkFastString $ makeUri xsd $ s2b "")
--  writeGraph, writeTriples, writeTriple,
--  writeNode, writeLValue, writeLiteralString