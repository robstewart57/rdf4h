module Text.RDF.TurtleSerializer where

import Text.RDF.Core
import Text.RDF.Namespace
import Text.RDF.Utils

import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as B

import Data.Map(Map)
import qualified Data.Map as Map

import Data.List

import Control.Monad

import Text.PrettyPrint.HughesPJ()

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
writeBase _ Nothing               =
  return ()
writeBase h (Just (BaseUrl bUrl)) =
  hPutStr h "@base " >> hPutChar h '<' >> B.hPutStr h bUrl >> hPutStr h "> ." >> hPutChar h '\n'

writePrefixes :: Handle -> PrefixMappings -> IO ()
writePrefixes h pms = mapM_ (writePrefix h) (toPMList pms) >> hPutChar h '\n'

writePrefix :: Handle -> (ByteString, ByteString) -> IO ()
writePrefix h (pre, uri) =
  hPutStr h "@prefix " >> B.hPutStr h pre >> hPutStr h ": " >>
  hPutChar h '<' >> B.hPutStr h uri >> hPutStr h "> ." >> hPutChar h '\n'

writeTriples :: Handle -> Maybe BaseUrl -> PrefixMappings -> Triples -> IO ()
writeTriples h bUrl (PrefixMappings pms) ts =
  mapM_ (writeSubjGroup h bUrl revPms) (groupBy equalSubjects ts)
  where
    revPms = Map.fromList $ map (\(k,v) -> (v,k)) $ Map.toList pms

-- Write a group of triples that all have the same subject, with the subject only
-- being output once, and comma or semi-colon used as appropriate.
writeSubjGroup :: Handle -> Maybe BaseUrl -> Map ByteString ByteString -> Triples -> IO ()
writeSubjGroup _ _    _   []     = return ()
writeSubjGroup h bUrl pms ts@(t:_) =
  writeNode h (subjectOf t) pms >> hPutChar h ' ' >>
  mapM_ (writePredGroup h bUrl pms) (groupBy equalPredicates ts)

-- Write a group of triples that all have the same subject and the same predicate,
-- assuming the subject has already been output and only the predicate objects
-- need to written.
writePredGroup :: Handle -> Maybe BaseUrl -> Map ByteString ByteString -> Triples -> IO ()
writePredGroup _ _ _   []     = return ()
writePredGroup h _ pms (t:ts) =
  writeNode h (predicateOf t) pms >> hPutChar h ' ' >> writeNode h (objectOf t) pms >>
  mapM_ (\t -> hPutStr h ", " >> writeNode h (objectOf t) pms) ts >>
  hPutStrLn h " ."

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

--subj1 = unode $ s2b "http://example.com/subj"
--pred1 = unode $ s2b "http://example.com/pred"
--obj1  = typedL (s2b "hello, world") (mkFastString $ makeUri xsd $ s2b "")
--  writeGraph, writeTriples, writeTriple,
--  writeNode, writeLValue, writeLiteralString