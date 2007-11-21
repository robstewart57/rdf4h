module Text.RDF.TurtleSerializer where

import Text.RDF.Core
import Text.RDF.Namespace
import Text.RDF.Utils

import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as B

import Data.Map(Map)
import qualified Data.Map as Map

import Control.Monad

import Text.PrettyPrint.HughesPJ

import System.IO

splitUri :: ByteString -> (ByteString, ByteString)
splitUri = B.breakEnd (\c -> c == '/' || c == ':' || c == '#')

writeGraph :: Graph gr => Handle -> gr -> IO ()
writeGraph h gr =
  writeHeader h bUrl pms >> writeTriples h bUrl pms ts >> hPutChar h '\n'
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
writePrefixes h pms = mapM_ (writePrefix h) (toPMList pms) >> hPutChar h '\n'

writePrefix :: Handle -> (ByteString, ByteString) -> IO ()
writePrefix h (pre, uri) =
  hPutStr h "@prefix " >> B.hPutStr h pre >> hPutChar h ':' >>
  B.hPutStrLn h uri

writeTriples :: Handle -> Maybe BaseUrl -> PrefixMappings -> Triples -> IO ()
writeTriples _ _    _                    []     = return ()
writeTriples h bUrl (PrefixMappings pms) (t:ts) =
  let (subj, pred, obj) = (subjectOf t, predicateOf t, objectOf t)
      revPms            = Map.fromList $ map (\(k,v) -> (v,k)) $ Map.toList pms
  in writeNode h subj revPms >> hPutChar h ' ' >>
     writeNode h pred revPms >> hPutChar h ' ' >>
     writeNode h obj  revPms >> hPutStrLn h " ."

writeNode :: Handle -> Node -> Map ByteString ByteString -> IO ()
writeNode h node prefixes =
  case node of
    (UNode fs)  -> writeUNodeUri h (B.reverse $ value fs) prefixes
    (BNode gId) -> hPutStrRev h (value gId)
    (BNodeGen i)-> putStr "_:genid" >> hPutStr h (show i)
    (LNode n)   -> writeLValue h n

writeUNodeUri :: Handle -> ByteString -> Map ByteString ByteString -> IO ()
writeUNodeUri h uri prefixes =
  case Map.lookup urlInit prefixes of
    Nothing    -> hPutChar h '<' >> B.hPutStr h uri >> hPutChar h '>'
    (Just pre) -> B.hPutStr h pre >> hPutChar h ':' >> B.hPutStr h localName
  where
    (urlInit, localName) = splitUri uri

writeLValue :: Handle -> LValue -> IO ()
writeLValue h lv =
  case lv of
    (PlainL lit)       -> writeLiteralString h lit
    (PlainLL lit lang) -> writeLiteralString h lit >>
                            hPutStr h "@\"" >>
                            B.hPutStr h lang >>
                            hPutStr h "\""
    (TypedL lit dtype) -> writeLiteralString h lit >>
                            hPutStr h "^^\"" >>
                            hPutStrRev h (value dtype) >>
                            hPutStr h "\""

writeLiteralString:: Handle -> ByteString -> IO ()
writeLiteralString h bs =
  do hPutChar h '"'
     B.foldl' writeChar (return True) bs
     hPutChar h '"'
  where
    writeChar :: IO (Bool) -> Char -> IO (Bool)
    writeChar b c =
      case c of
        '\n' ->  b >>= \b' -> when b' (hPutChar h '\\' >> hPutChar h 'n')  >> return True
        '\t' ->  b >>= \b' -> when b' (hPutChar h '\\' >> hPutChar h 't')  >> return True
        '\r' ->  b >>= \b' -> when b' (hPutChar h '\\' >> hPutChar h 'r')  >> return True
        '"'  ->  b >>= \b' -> when b' (hPutChar h '\\' >> hPutChar h '"')  >> return True
        '\\' ->  b >>= \b' -> when b' (hPutChar h '\\' >> hPutChar h '\\') >> return True
        _    ->  b >>= \b' -> when b' (hPutChar  h c)                      >> return True
--writeSubjGroup :: Handle -> Maybe BaseUrl -> PrefixMappings -> Triples -> IO ()
--writeSubjGroup h bUrl pms ts = mapM_ (writeSubjPredGroup h bUrl pms) (groupBy equalPred ts)

--writeSubjPredGroup :: Handle -> Maybe BaseUrl -> PrefixMappings -> Triples -> IO ()
--writeSubjPredGroup h bUrl pms ts = undefined

subj1 = unode $ s2b "http://example.com/subj"
pred1 = unode $ s2b "http://example.com/pred"
obj1  = typedL (s2b "hello, world") (mkFastString $ makeUri xsd $ s2b "")
--  writeGraph, writeTriples, writeTriple,
--  writeNode, writeLValue, writeLiteralString