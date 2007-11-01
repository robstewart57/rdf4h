module Utils where

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
  show fs@(FS rev _ _ _) = B.unpack $ f $ value fs
    where f = if rev then B.reverse else id


instance Eq FastString where
  (FS u1 _ _ _) == (FS u2 _ _ _) = u1 == u2

instance Ord FastString where
  compare (FS True u1 _ v1)  (FS True u2 _ v2)  = compareFS u1 v1 u2 v2
  compare (FS False u1 _ v1) (FS False u2 _ v2) = compareFS u1 v1 u2 v2
  compare (FS True u1 _ v1)  (FS False u2 _ v2) = compareFS u1 v1 u2 (B.reverse v2)
  compare (FS False u1 _ v1) (FS True u2 _ v2)  = compareFS u1 (B.reverse v1) u2 v2

compareFS u1 v1 u2 v2 =
  case u1 == u2 of
    True    ->  EQ
    False   ->  compare v1 v2

fsCounter :: IORef Int
fsCounter = unsafePerformIO $ newIORef 0

-- The hashfunction is adapted for bytestring but otherwise copied from Data.HashTable.
fsTable :: IO (HashTable ByteString FastString)
fsTable = HT.new (==) hashByteString 

hashByteString :: ByteString -> Int32
hashByteString str = B.foldl f 0 str
  where f m c = fromIntegral (ord c + 1) * golden + mulHi m golden
        golden =  -1640531527 :: Int32
        -- hi 32 bits of a x-bit * 32 bit -> 64-bit multiply
        mulHi :: Int32 -> Int32 -> Int32
        mulHi a b = fromIntegral (r `shiftR` 32)
          where r :: Int64
                r = fromIntegral a * fromIntegral b :: Int64

mkFastString :: ByteString -> IO FastString
mkFastString str = mkFastString' False fsTable str

mkFastStringR :: ByteString -> IO FastString
mkFastStringR str = mkFastString' True fsTable str

mkFastString' :: Bool -> IO (HashTable ByteString FastString) -> ByteString -> IO FastString
mkFastString' rev fst str =
  do ht <- fst
     res <- HT.lookup ht str
     case res of
       Nothing   -> do fs <- newFastString rev str
                       HT.insert ht str fs
                       return fs
       (Just fs)  -> return fs

newFastString :: Bool -> ByteString -> IO FastString
newFastString rev str = 
  do curr <- readIORef fsCounter
     modifyIORef fsCounter (+1)
     return (FS rev curr (B.length str) val)
  where val = if rev then B.reverse str else str

b1 = B.pack "asdf"
b2 = B.pack "zxcv"