module Utils(FastString(uniq,value), mkFastString, equalFS, compareFS) where

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
-- together with the value of the bytestring. ByteString has O(1) length, so we no longer
-- include the length explicitly.
data FastString = FS {
      uniq   :: {-# UNPACK #-} !Int,
      value  :: {-# UNPACK #-} !ByteString
}

instance Show FastString where
  show !fs = B.unpack $! B.reverse $! value fs

instance Eq FastString where
  (==) !fs1 !fs2 = equalFS fs1 fs2

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
    False   ->  compareBS (value fs1) (value fs2)

{-# INLINE compareBS #-}
compareBS :: ByteString -> ByteString -> Ordering
compareBS !b1 !b2 = compare b1 b2

{-# NOINLINE fsCounter #-}
fsCounter :: IORef Int
fsCounter = unsafePerformIO $ newIORef 0

-- The hashfunction is adapted for bytestring but otherwise copied from Data.HashTable.
{-# NOINLINE fsTable #-}
fsTable :: HashTable ByteString FastString
fsTable = unsafePerformIO $ HT.new (==) hashByteString 

{-# INLINE hashByteString #-}
hashByteString :: ByteString -> Int32
hashByteString !str = B.foldl' f 0 str

{-# INLINE f #-}
f :: Int32 -> Char -> Int32
f !m !c = fromIntegral (ord c + 1) * golden + mulHi m golden

{-# INLINE golden #-}
golden :: Int32
golden =  -1640531527

{-# INLINE mulHi #-}
-- hi 32 bits of a x-bit * 32 bit -> 64-bit multiply
mulHi :: Int32 -> Int32 -> Int32
mulHi !a !b = fromIntegral (r `shiftR` 32)
  where r :: Int64
        r = fromIntegral a * fromIntegral b :: Int64

{-# INLINE mkFastString #-}
mkFastString :: ByteString -> IO FastString
mkFastString !str = mkFastString' fsTable str

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
     return $! (FS curr (B.reverse str))

_b1, _b2 :: ByteString
_b1 = B.pack "asdf"
_b2 = B.pack "zxcv"