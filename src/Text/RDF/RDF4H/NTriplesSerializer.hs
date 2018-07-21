{-# LANGUAGE OverloadedStrings #-}

-- |A serializer for RDF as N-Triples
-- <http://www.w3.org/TR/rdf-testcases/#ntriples>.

module Text.RDF.RDF4H.NTriplesSerializer
  ( NTriplesSerializer(NTriplesSerializer)
  ) where

import Data.RDF.Types
import Data.RDF.Query (expandTriples)
import qualified Data.Text as T
import qualified Data.Text.IO as T
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

_writeRdf :: Rdf a => Handle -> RDF a -> IO ()
_writeRdf h = _writeTriples h . expandTriples

_writeTriples :: Handle -> Triples -> IO ()
_writeTriples h = mapM_ (_writeTriple h)

_writeTriple :: Handle -> Triple -> IO ()
_writeTriple h (Triple s p o) =
  _writeNode h s >> hPutChar h ' ' >>
  _writeNode h p >> hPutChar h ' ' >>
  _writeNode h o >> hPutStrLn h " ."

_writeNode :: Handle -> Node -> IO ()
_writeNode h node = case node of
  (UNode s)  -> hPutChar h '<' >> T.hPutStr h s >> hPutChar h '>'
  (BNode gId) -> T.hPutStr h "_:" >> T.hPutStr h gId
  (BNodeGen i)-> T.hPutStr h "_:genid" >> hPutStr h (show i)
  (LNode n)   -> _writeLValue h n

_writeLValue :: Handle -> LValue -> IO ()
_writeLValue h lv = case lv of
  (PlainL lit)       -> _writeLiteralString h lit
  (PlainLL lit lang) -> _writeLiteralString h lit >>
                          hPutChar h '@' >>
                          T.hPutStr h lang
  (TypedL lit dtype) -> _writeLiteralString h lit >>
                          hPutStr h "^^<" >>
                          T.hPutStr h dtype >>
                          hPutChar h '>'

_writeLiteralString:: Handle -> T.Text -> IO ()
_writeLiteralString h ls = do
  hPutChar h '"'
  T.hPutStr h $ T.concatMap escapeChar ls
  hPutChar h '"'
  where escapeChar '\n' = "\\n"
        escapeChar '\t' = "\\t"
        escapeChar '\r' = "\\r"
        escapeChar '"'  = "\\\""
        escapeChar '\\' = "\\\\"
        escapeChar c    = T.singleton c
