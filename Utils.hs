module Utils(FastString(rev,uniq,len,value), mkFastString, mkFastStringR, equalFS) where

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
-- together with the length of the bytestring and the value of the bytestring
data FastString = FS {
      rev    :: {-# UNPACK #-} !Bool,
      uniq   :: {-# UNPACK #-} !Int,
      len    :: {-# UNPACK #-} !Int,
      value  :: {-# UNPACK #-} !ByteString
}

instance Show FastString where
  show fs = B.unpack $ f $ value fs
    where f = if rev fs then B.reverse else id

instance Eq FastString where
  (==) !fs1 !fs2 = equalFS fs1 fs2

equalFS :: FastString -> FastString -> Bool
equalFS !fs1 ! fs2 = uniq fs1 == uniq fs2

instance Ord FastString where
  compare !fs1 !fs2 =
    case (rev fs1, rev fs2) of
      (True,  True)  -> compareFS u1 v1 u2 v2
      (False, False) -> compareFS u1 v1 u2 v2
      (True, False)  -> compareFS u1 v1 u2 (B.reverse v2)
      (False, True)  -> compareFS u1 (B.reverse v1) u2 v2
    where
      u1 = uniq fs1
      u2 = uniq fs2
      v1 = value fs1
      v2 = value fs2

compareFS :: Int -> ByteString -> Int -> ByteString -> Ordering
compareFS !u1 !v1 !u2 !v2 =
  case u1 == u2 of
    True    ->  EQ
    False   ->  compareBS v1 v2

compareBS :: ByteString -> ByteString -> Ordering
compareBS !b1 !b2 = compare b1 b2

fsCounter :: IORef Int
fsCounter = unsafePerformIO $ newIORef 0

-- The hashfunction is adapted for bytestring but otherwise copied from Data.HashTable.
fsTable :: HashTable ByteString FastString
fsTable = unsafePerformIO $ HT.new (==) hashByteString 

hashByteString :: ByteString -> Int32
hashByteString !str = B.foldl f 0 str

f :: Int32 -> Char -> Int32
f !m !c = fromIntegral (ord c + 1) * golden + mulHi m golden

golden :: Int32
golden =  -1640531527

-- hi 32 bits of a x-bit * 32 bit -> 64-bit multiply
mulHi :: Int32 -> Int32 -> Int32
mulHi a b = fromIntegral (r `shiftR` 32)
  where r :: Int64
        r = fromIntegral a * fromIntegral b :: Int64

mkFastString :: ByteString -> IO FastString
mkFastString !str = mkFastString' False fsTable str

mkFastStringR :: ByteString -> IO FastString
mkFastStringR !str = mkFastString' True fsTable str

mkFastString' :: Bool -> HashTable ByteString FastString -> ByteString -> IO FastString
mkFastString' !rev !fst !str =
  do res <- HT.lookup fst str
     case res of
       Nothing   -> do fs <- newFastString rev str
                       HT.insert fst str fs
                       return fs
       (Just fs)  -> return fs

newFastString :: Bool -> ByteString -> IO FastString
newFastString !rev !str = 
  do curr <- readIORef fsCounter
     modifyIORef fsCounter (+1)
     return (FS rev curr (B.length str) val)
  where val = if rev then B.reverse str else str

_b1, _b2 :: ByteString
_b1 = B.pack "asdf"
_b2 = B.pack "zxcv"