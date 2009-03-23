-- |A serializer for RDF as N-Triples
-- <http://www.w3.org/TR/rdf-testcases/#ntriples>.

module Text.RDF.RDF4H.NTriplesSerializer(
  writeGraph, writeTriples, writeTriple,
  writeNode, writeLValue, writeLiteralString,
  NTriplesSerializer(NTriplesSerializer)
) where

import Text.RDF.RDF4H.Core
import Text.RDF.RDF4H.Utils

import Data.ByteString.Lazy.Char8(ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy as BL

import System.IO
import Control.Monad


data NTriplesSerializer = NTriplesSerializer

instance RdfSerializer NTriplesSerializer where
  hWriteG _     = writeGraph
  writeG  _     = writeGraph stdout
  hWriteH _ _ _ = return ()
  writeH  _   _ = return ()
  hWriteTs _    = writeTriples
  writeTs  _    = writeTriples stdout
  hWriteT  _    = writeTriple
  writeT   _    = writeTriple stdout
  hWriteN  _    = writeNode
  writeN   _    = writeNode stdout

writeGraph :: Graph gr => Handle -> gr -> IO ()
writeGraph h = writeTriples h . triplesOf

writeTriples :: Handle -> Triples -> IO ()
writeTriples h = mapM_ (writeTriple h)

writeTriple :: Handle -> Triple -> IO ()
writeTriple h (Triple s p o) =
  writeNode h s >> hPutChar h ' ' >>
  writeNode h p >> hPutChar h ' ' >>
  writeNode h o >> hPutStrLn h " ."

writeNode :: Handle -> Node -> IO ()
writeNode h node =
  case node of
    (UNode fs)  -> hPutChar h '<' >>
                     hPutStrRev h (value fs) >>
                     hPutChar h '>'
    (BNode gId) -> hPutStrRev h (value gId)
    (BNodeGen i)-> putStr "_:genid" >> hPutStr h (show i)
    (LNode n)   -> writeLValue h n

writeLValue :: Handle -> LValue -> IO ()
writeLValue h lv =
  case lv of
    (PlainL lit)       -> writeLiteralString h lit
    (PlainLL lit lang) -> writeLiteralString h lit >>
                            hPutStr h "@" >>
                            BL.hPutStr h lang
    (TypedL lit dtype) -> writeLiteralString h lit >>
                            hPutStr h "^^<" >>
                            hPutStrRev h (value dtype) >>
                            hPutStr h ">"

-- TODO: this is REALLY slow.
writeLiteralString:: Handle -> ByteString -> IO ()
writeLiteralString h bs =
  do hPutChar h '"'
     B.foldl' writeChar (return ()) bs
     hPutChar h '"'
  where
    -- the seq is necessary in writeChar to ensure all chars
    -- are written. without it, only the last is written.
    writeChar :: IO () -> Char -> IO ()
    writeChar b c = b >>= \b' -> b' `seq`
      case c of
        '\n' ->  hPutChar h '\\' >> hPutChar h 'n'  >> return ()
        '\t' ->  hPutChar h '\\' >> hPutChar h 't'  >> return ()
        '\r' ->  hPutChar h '\\' >> hPutChar h 'r'  >> return ()
        '"'  ->  hPutChar h '\\' >> hPutChar h '"'  >> return ()
        '\\' ->  hPutChar h '\\' >> hPutChar h '\\' >> return ()
        _    ->  hPutChar  h c                      >> return ()

_bs1, _bs2 :: ByteString
_bs1 = B.pack "\nthis \ris a \\U00015678long\t\nliteral\\uABCD\n"
_bs2 = B.pack "\nan \\U00015678 escape\n"

_w :: IO ()
_w = writeLiteralString stdout _bs1
