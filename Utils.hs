module Utils(FastString(uniq,value), 
             mkFastString, equalFS, compareFS,s2b,b2s
) where

import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as B

import System.IO.Unsafe(unsafePerformIO)
import Control.Monad
import Data.HashTable(HashTable)
import qualified Data.HashTable as HT
import Data.Int(Int32, Int64)
import Data.Char(ord)
import Data.Bits(shiftR)
import Data.IORef

-- |A wrapper around a bytestring that contains a uniq int associated with the bytestring,
-- together with the value of the bytestring. 
-- 
-- FastString values are created using 'mkFastString', which will reuse values
-- wherever possible. The 'uniq' value is an identifier for a given ByteString,
-- and provides fast equality testing due to the invariant that two equal
-- bytestrings have equal uniq values.
data FastString = FS {
      uniq   :: {-# UNPACK #-} !Int,
      value  :: {-# UNPACK #-} !ByteString
}

instance Show FastString where
  show = B.unpack . B.reverse . value

instance Eq FastString where
  (==) = equalFS

{-# INLINE equalFS #-}
equalFS :: FastString -> FastString -> Bool
equalFS !fs1 !fs2 = uniq fs1 == uniq fs2

instance Ord FastString where
  compare !fs1 !fs2 = compareFS fs1 fs2

{-# INLINE compareFS #-}
compareFS :: FastString -> FastString -> Ordering
compareFS fs1 fs2 =
  case uniq fs1 == uniq fs2 of
    True    ->  EQ
    False   ->  compare (value fs1) (value fs2)

-- |A convenience function for converting from a bytestring to a string.
{-# INLINE b2s #-}
b2s :: ByteString -> String
b2s = B.unpack

-- |A convenience function for converting from a string to a bytestring.
{-# INLINE s2b #-}
s2b :: String -> ByteString
s2b = B.pack

{-# NOINLINE fsCounter #-}
fsCounter :: IORef Int
fsCounter = unsafePerformIO $ newIORef 0

{-# NOINLINE fsTable #-}
fsTable :: HashTable ByteString FastString
fsTable = unsafePerformIO $ HT.new (==) _hashByteString1

-- This hashfunction is the djb2 hashFunction.
{-# INLINE _hashByteString1 #-}
_hashByteString1 :: ByteString -> Int32
_hashByteString1 = fromIntegral . B.foldl' djb2 (5381 :: Int)
  where djb2     ::  Int -> Char -> Int
        djb2 h c  =  (h * 33) + ord c

-- This hashfunction is adapted for bytestring but otherwise copied from Data.HashTable.
-- The above much simpler function is faster and doesn't seem to have problems 
-- with too many collisions, so we're using that one for now.
{-# INLINE _hashByteString2 #-}
_hashByteString2 :: ByteString -> Int32
_hashByteString2 = B.foldl' f 0
  where f m c = fromIntegral (ord c) + _mulHi m _golden

{-# INLINE _golden #-}
_golden :: Int32
_golden =  -1640531527

{-# INLINE _mulHi #-}
-- hi 32 bits of a x-bit * 32 bit -> 64-bit multiply
_mulHi :: Int32 -> Int32 -> Int32
_mulHi a b = fromIntegral (r `shiftR` 32)
  where r :: Int64
        r = fromIntegral a * fromIntegral b :: Int64

{-# INLINE mkFastString #-}
mkFastString :: ByteString -> IO FastString
mkFastString = mkFastString' fsTable

{-# INLINE mkFastString' #-}
mkFastString' :: HashTable ByteString FastString -> ByteString -> IO FastString
mkFastString' !fst !str =
  do res <- HT.lookup fst str
     case res of
       Nothing   -> do fs <- newFastString str
                       HT.insert fst str fs
                       return $! fs
       (Just fs)  -> return $! fs

{-# INLINE newFastString #-}
newFastString ::  ByteString -> IO FastString
newFastString !str = 
  do curr <- readIORef fsCounter
     modifyIORef fsCounter (+1)
     return $! (FS curr ((1 :: Int) `seq` B.reverse str))

_b1, _b2 :: ByteString
_b1 = B.pack "asdf"
_b2 = B.pack "zxcv"