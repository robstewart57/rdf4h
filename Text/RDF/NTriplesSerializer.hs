module Text.RDF.NTriplesSerializer where

import Text.RDF.Core
import Text.RDF.Utils

import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as B

import System.IO
import Control.Monad

writeNTriples :: Handle -> Triples -> IO ()
writeNTriples h = mapM_ (writeNTriple h)

writeNTriple :: Handle -> Triple -> IO ()
writeNTriple h (Triple s p o) =
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
    (BNodeGen i)-> hPutStr h (show i)
    (LNode n)   -> writeLValue h n

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
     --case B.any (\c -> c == '\n' || c == '"') bs of -- TODO: see if this is faster than just always folding
     --  False -> B.hPutStr h bs >> return True
     --  True  -> B.foldl' writeChar (return True) bs
     B.foldl' writeChar (return True) bs
     hPutChar h '"'
  where
    writeChar b c = 
      case c of 
        '\n' ->  b >>= \b' -> when b' (hPutChar h '\\' >> hPutChar h 'n') >> return True
        '\t' ->  b >>= \b' -> when b' (hPutChar h '\\' >> hPutChar h 't') >> return True
        '"'  ->  b >>= \b' -> when b' (hPutChar h '\\' >> hPutChar h '"') >> return True
        _    ->  b >>= \b' -> when b' (hPutChar  h c)                     >> return True

