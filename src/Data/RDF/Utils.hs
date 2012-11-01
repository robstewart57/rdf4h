
module Data.RDF.Utils (
  t2s, s2t, hPutStrRev
) where

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B
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

