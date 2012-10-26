
module Data.RDF.Utils (
  t2s, s2t, hPutStrRev, canonicalize
) where

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import Data.Map(Map)
import qualified Data.Map as Map
import System.IO

-- |A convenience function for converting from a bytestring to a string.
{-# INLINE t2s #-}
t2s :: T.Text -> String
t2s = T.unpack

-- |A convenience function for converting from a string to a bytestring.
{-# INLINE s2t #-}
s2t :: String -> T.Text
s2t = T.pack

-- |Write to the handle the reversed value of the bytestring, with no newline.
{-# INLINE hPutStrRev #-}
hPutStrRev :: Handle -> T.Text -> IO ()
hPutStrRev h bs = B.hPutStr h ((encodeUtf8 . T.reverse) bs)

-- |Canonicalize the given 'T.Text' value using the 'T.Text'
-- as the datatype URI.
{-# NOINLINE canonicalize #-}
canonicalize :: T.Text -> T.Text -> T.Text
canonicalize typeTxt litValue =
  case Map.lookup typeTxt canonicalizerTable of
    Nothing   ->  litValue
    Just fn   ->  fn litValue

-- A table of mappings from a 'T.Text' URI (reversed as
-- they are) to a function that canonicalizes a T.Text
-- assumed to be of that type.
{-# NOINLINE canonicalizerTable #-}
canonicalizerTable :: Map T.Text (T.Text -> T.Text)
canonicalizerTable =
  Map.fromList [(integerUri, _integerStr), (doubleUri, _doubleStr),
                (decimalUri, _decimalStr)]
  where
    integerUri = mkFsUri "http://www.w3.org/2001/XMLSchema#integer"
    decimalUri = mkFsUri "http://www.w3.org/2001/XMLSchema#decimal"
    doubleUri  = mkFsUri "http://www.w3.org/2001/XMLSchema#double"
    mkFsUri :: String -> T.Text
    mkFsUri uri = s2t $! uri

_integerStr, _decimalStr, _doubleStr :: T.Text -> T.Text
_integerStr = T.dropWhile (== '0')

-- exponent: [eE] ('-' | '+')? [0-9]+
-- ('-' | '+') ? ( [0-9]+ '.' [0-9]* exponent | '.' ([0-9])+ exponent | ([0-9])+ exponent )
_doubleStr s = T.pack $ show (read $ T.unpack s :: Double)

-- ('-' | '+')? ( [0-9]+ '.' [0-9]* | '.' ([0-9])+ | ([0-9])+ )
_decimalStr s =     -- haskell double parser doesn't handle '1.'..,
  case T.last s of   -- so we add a zero if that's the case and then parse
    '.' -> f (s `T.snoc` '0')
    _   -> f s
  where f s' = T.pack $ show (read $ T.unpack s' :: Double)
