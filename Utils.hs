{-#  GHC_OPTIONS -fno-cse  #-}
module Utils(FastString(uniq,value), 
             mkFastString, equalFS, compareFS,s2b,b2s
) where

import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as B

import Text.Printf(printf)
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad
import Data.HashTable(HashTable)
import qualified Data.HashTable as HT
import Data.Int(Int32, Int64)
import Data.Char(ord)
import Data.Bits(shiftR)
import Data.IORef

-- |'FastString' is a bytestring-based string type that provides constant-time equality
-- testing.
--
-- A 'FastString' value consists of a unique identifier and a (strict) 'ByteString' value.
-- The unique identifier is used for constant-time equality testing, and all other operations
-- are provided by the 'ByteString' value itself.
--
-- 'FastString' values are created by the 'mkFastString' function, which maintains a table
-- of all created values, and reuses old values whenever possible. The 'ByteString' is
-- maintained internally in reverse order of the string passed to 'mkFastString'; this
-- is to provide faster comparison testing for unequal values, since it is very common in
-- RDF to have URIs that are equal apart from the last few characters (localname).
data FastString = FS {
      uniq   :: {-# UNPACK #-} !Int,
      value  :: {-# UNPACK #-} !ByteString
}

instance Show FastString where
  show fs = printf "(%d, %s)" (uniq fs) (B.unpack $ B.reverse $ value fs)

-- |Two 'FastString' values are equal iff they have the same unique identifer.
instance Eq FastString where
  (==) = equalFS

{-# INLINE equalFS #-}
equalFS :: FastString -> FastString -> Bool
equalFS !fs1 !fs2 = uniq fs1 == uniq fs2

-- |Two 'FastString' values are equal if they have the same unique identifier, 
-- and are otherwise ordered using the natural ordering of 'ByteString' in the 
-- internal (reversed) representation.
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

-- |Return a 'FastString' value for the given 'ByteString', reusing a 'FastString'
-- if one has been created for equal bytestrings, or creating a new one if necessary.
-- The 'FastString' values created maintain the invariant that two values have the
-- same unique identifier (accessible via 'uniq') iff their respective bytestring
-- values are equal. 
--
-- The unique identifier is only for the given session, and equal 'ByteString' values
-- will generally not be assigned the same identifier under different processes and
-- different executions.
{-# NOINLINE mkFastString #-}
mkFastString :: ByteString -> FastString
mkFastString !bs = unsafePerformIO $ mkFastString' fsTable bs

{-# INLINE mkFastString' #-}
mkFastString' :: HashTable ByteString FastString -> ByteString -> IO FastString
mkFastString' fst str =
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

{-
registerCanonicalizer :: ByteString -> (ByteString -> ByteString) -> (FastString, ByteString -> ByteString)
registerCanonicalizer typeUri canonicalizer =
    do typeUriFs <- mkFastString typeUri
       HT.insert canonicalizersT typeUriFs canonicalizer
       return $! (typeUriFs, canonicalizer)
-}     

{-
canonicalize :: ByteString -> ByteString -> ByteString
canonicalize typeUri value = 
  unsafePerformIO $
    do typeUriFs <- mkFastString typeUri
       mCanonFn <- HT.lookup canonicalizersT typeUriFs
       case mCanonFn of
         Nothing   -> return value
         (Just fn) -> return $ fn value
-}