-- |A serializer for RDF as N-Triples
-- <http://www.w3.org/TR/rdf-testcases/#ntriples>.

module Text.RDF.RDF4H.NTriplesSerializer(
  NTriplesSerializer(NTriplesSerializer)
) where

import Control.Monad (void)
import Data.RDF.Types
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.IO

data NTriplesSerializer = NTriplesSerializer

instance RdfSerializer NTriplesSerializer where
  hWriteRdf _     = _writeRdf
  writeRdf  _     = _writeRdf stdout
  hWriteH   _ _ _ = return ()
  writeH    _   _ = return ()
  hWriteTs  _     = _writeTriples
  writeTs   _     = _writeTriples stdout
  hWriteT   _     = _writeTriple
  writeT    _     = _writeTriple stdout
  hWriteN   _     = _writeNode
  writeN    _     = _writeNode stdout

_writeRdf :: RDF rdf => Handle -> rdf -> IO ()
_writeRdf h = _writeTriples h . triplesOf

_writeTriples :: Handle -> Triples -> IO ()
_writeTriples h = mapM_ (_writeTriple h)

_writeTriple :: Handle -> Triple -> IO ()
_writeTriple h (Triple s p o) =
  _writeNode h s >> hPutChar h ' ' >>
  _writeNode h p >> hPutChar h ' ' >>
  _writeNode h o >> hPutStrLn h " ."

_writeNode :: Handle -> Node -> IO ()
_writeNode h node =
  case node of
    (UNode bs)  -> hPutChar h '<' >>
                     T.hPutStr h bs >>
                     hPutChar h '>'
    (BNode gId) -> T.hPutStr h gId
    (BNodeGen i)-> putStr "_:genid" >> hPutStr h (show i)
    (LNode n)   -> _writeLValue h n

_writeLValue :: Handle -> LValue -> IO ()
_writeLValue h lv =
  case lv of
    (PlainL lit)       -> _writeLiteralString h lit
    (PlainLL lit lang) -> _writeLiteralString h lit >>
                            hPutStr h "@" >>
                            T.hPutStr h lang
    (TypedL lit dtype) -> _writeLiteralString h lit >>
                            hPutStr h "^^<" >>
                            T.hPutStr h dtype >>
                            hPutStr h ">"

-- TODO: this is REALLY slow.
_writeLiteralString:: Handle -> T.Text -> IO ()
_writeLiteralString h bs =
  do hPutChar h '"'
     T.foldl' writeChar (return ()) bs
     hPutChar h '"'
  where
    -- the seq is necessary in writeChar to ensure all chars
    -- are written. without it, only the last is written.
    writeChar :: IO () -> Char -> IO ()
    writeChar b c = b >>= \b' -> b' `seq`
      case c of
        '\n' ->  void (hPutChar h '\\' >> hPutChar h 'n')
        '\t' ->  void (hPutChar h '\\' >> hPutChar h 't')
        '\r' ->  void (hPutChar h '\\' >> hPutChar h 'r')
        '"'  ->  void (hPutChar h '\\' >> hPutChar h '"')
        '\\' ->  void (hPutChar h '\\' >> hPutChar h '\\')
        _    ->  void (hPutChar h c)

_bs1, _bs2 :: T.Text
_bs1 = T.pack "\nthis \ris a \\U00015678long\t\nliteral\\uABCD\n"
_bs2 = T.pack "\nan \\U00015678 escape\n"

_w :: IO ()
_w = _writeLiteralString stdout _bs1
