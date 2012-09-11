
module Data.RDF.Utils (
  s2b, b2s, hPutStrRev, canonicalize
) where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8(ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Map(Map)
import qualified Data.Map as Map
import System.IO

-- |A convenience function for converting from a bytestring to a string.
{-# INLINE b2s #-}
b2s :: ByteString -> String
b2s = B.unpack

-- |A convenience function for converting from a string to a bytestring.
{-# INLINE s2b #-}
s2b :: String -> ByteString
s2b = B.pack

-- |Write to the handle the reversed value of the bytestring, with no newline.
{-# INLINE hPutStrRev #-}
hPutStrRev :: Handle -> ByteString -> IO ()
hPutStrRev h bs = BL.hPutStr h (B.reverse bs)

-- |Canonicalize the given 'ByteString' value using the 'FastString'
-- as the datatype URI.
{-# NOINLINE canonicalize #-}
canonicalize :: ByteString -> ByteString -> ByteString
canonicalize typeFs litValue =
  case Map.lookup typeFs canonicalizerTable of
    Nothing   ->  litValue
    Just fn   ->  fn litValue

-- A table of mappings from a FastString URI (reversed as
-- they are) to a function that canonicalizes a ByteString
-- assumed to be of that type.
{-# NOINLINE canonicalizerTable #-}
canonicalizerTable :: Map ByteString (ByteString -> ByteString)
canonicalizerTable =
  Map.fromList [(integerUri, _integerStr), (doubleUri, _doubleStr),
                (decimalUri, _decimalStr)]
  where
    integerUri = mkFsUri "http://www.w3.org/2001/XMLSchema#integer"
    decimalUri = mkFsUri "http://www.w3.org/2001/XMLSchema#decimal"
    doubleUri  = mkFsUri "http://www.w3.org/2001/XMLSchema#double"
    mkFsUri :: String -> ByteString
    mkFsUri uri = s2b $! uri

_integerStr, _decimalStr, _doubleStr :: ByteString -> ByteString
_integerStr = B.dropWhile (== '0')

-- exponent: [eE] ('-' | '+')? [0-9]+
-- ('-' | '+') ? ( [0-9]+ '.' [0-9]* exponent | '.' ([0-9])+ exponent | ([0-9])+ exponent )
_doubleStr s = B.pack $ show (read $ B.unpack s :: Double)

-- ('-' | '+')? ( [0-9]+ '.' [0-9]* | '.' ([0-9])+ | ([0-9])+ )
_decimalStr s =     -- haskell double parser doesn't handle '1.'..,
  case B.last s of   -- so we add a zero if that's the case and then parse
    '.' -> f (s `B.snoc` '0')
    _   -> f s
  where f s' = B.pack $ show (read $ B.unpack s' :: Double)
