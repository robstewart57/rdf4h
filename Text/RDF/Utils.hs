{-#  GHC_OPTIONS -fno-cse  #-}
module Text.RDF.Utils (
  FastString(uniq, value), 
  mkFastString, equalFS, compareFS, s2b, b2s,
  canonicalize
) where

import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as B

import Data.Map(Map)
import qualified Data.Map as Map

import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

import Control.Monad

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
  show = B.unpack . B.reverse . value

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
mkFastString !bs = 
  unsafePerformIO $ 
  do m <- readIORef fsMap
     let mFs = Map.lookup bs m
     case mFs of
       Just fs  -> return fs
       Nothing  -> newFastString bs >>= \fs -> 
                     writeIORef fsMap (Map.insert bs fs m) >> 
                     return fs

-- |Canonicalize the given 'ByteString' value using the 'FastString'
-- as the datatype URI. 
{-# NOINLINE canonicalize #-}
canonicalize :: FastString -> ByteString -> ByteString
canonicalize typeFs litValue =
  case Map.lookup typeFs canonicalizerTable of
    Nothing   ->  litValue
    Just fn   ->  fn litValue

{-# INLINE newFastString #-}
newFastString ::  ByteString -> IO FastString
newFastString !str = 
  do curr <- readIORef fsCounter
     modifyIORef fsCounter (+1)
     return $! (FS curr ((1 :: Int) `seq` B.reverse str))

{-# NOINLINE fsCounter #-}
fsCounter :: IORef Int
fsCounter = unsafePerformIO $ newIORef 0

{-# NOINLINE fsMap #-}
fsMap :: IORef (Map ByteString FastString)
fsMap = unsafePerformIO $ newIORef Map.empty

-- A table of mappings from a FastString URI (reversed as
-- they are) to a function that canonicalizes a ByteString
-- assumed to be of that type.
{-# NOINLINE canonicalizerTable #-}
canonicalizerTable :: Map FastString (ByteString -> ByteString)
canonicalizerTable = 
  Map.fromList [(integerUri, _integerStr), (doubleUri, _doubleStr),
                (decimalUri, _decimalStr)]
  where 
    integerUri = mkFsUri "http://www.w3.org/2001/XMLSchema#integer"
    decimalUri = mkFsUri "http://www.w3.org/2001/XMLSchema#decimal"
    doubleUri  = mkFsUri "http://www.w3.org/2001/XMLSchema#double"
    mkFsUri :: String -> FastString
    mkFsUri uri = mkFastString . s2b $! uri

_integerStr, _decimalStr, _doubleStr :: ByteString -> ByteString
_integerStr !s = B.dropWhile (== '0') s

-- exponent: [eE] ('-' | '+')? [0-9]+
-- ('-' | '+') ? ( [0-9]+ '.' [0-9]* exponent | '.' ([0-9])+ exponent | ([0-9])+ exponent )
_doubleStr !s = B.pack $ show $ (read $ B.unpack s :: Double)

-- ('-' | '+')? ( [0-9]+ '.' [0-9]* | '.' ([0-9])+ | ([0-9])+ )
_decimalStr !s =     -- haskell double parser doesn't handle '1.'.., 
  case B.last s of   -- so we add a zero if that's the case and then parse
    '.' -> f (s `B.snoc` '0')
    _   -> f s
  where f s' = B.pack $ show (read $ B.unpack s' :: Double)
