{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-
  Files Xmlbf and Xeno have been taken from:
  https://gitlab.com/k0001/xmlbf

  Which is licensed under Apache License 2.0.

  Read the comments in the Xmlbf.hs file for the reason why.
-}

module Text.RDF.RDF4H.XmlParser.Xeno
 ( fromXenoNode
 , fromRawXml
 ) where

import qualified Data.Bifunctor as Bif
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
#if !MIN_VERSION_base(4,13,0)
import Data.Monoid ((<>))
#else
#endif
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Encoding as T
import Data.Traversable (for)
import qualified HTMLEntities.Decoder
import qualified Xeno.DOM as Xeno
import qualified Text.RDF.RDF4H.XmlParser.Xmlbf as Xmlbf

--------------------------------------------------------------------------------
-- Xeno support

-- | Convert a 'Xeno.Node' from "Xeno.DOM" into an 'Element' from "Xmlbf".
fromXenoNode
  :: Xeno.Node -- ^ A 'Xeno.Node' from "Xeno.DOM".
  -> Either String Xmlbf.Node -- ^ A 'Xmlbf.Node' from "Xmlbf".
fromXenoNode x = do
  n <- decodeUtf8 (Xeno.name x)
  as <- for (Xeno.attributes x) $ \(k,v) -> do
     (,) <$> decodeUtf8 k <*> unescapeXmlUtf8 v
  cs <- for (Xeno.contents x) $ \case
     Xeno.Element n1 -> fromXenoNode n1
     Xeno.Text bs -> Xmlbf.text' =<< unescapeXmlUtf8Lazy bs
     Xeno.CData bs -> Xmlbf.text' =<< decodeUtf8Lazy bs
  Xmlbf.element' n (HM.fromList as) cs

-- | Parses a given UTF8-encoded raw XML fragment into @a@, using the @xeno@
-- Haskell library, so all of @xeno@'s parsing quirks apply.
--
-- You can provide the output of this function as input to "Xmlbf"'s
-- 'Xmlbf.parse'.
--
-- The given XML can contain more zero or more text or element nodes.
--
-- Surrounding whitespace is not stripped.
fromRawXml
  :: B.ByteString                 -- ^ Raw XML fragment.
  -> Either String [Xmlbf.Node]   -- ^ 'Xmlbf.Node's from "Xmlbf"
fromRawXml = \bs -> case Xeno.parse ("<x>" <> dropBomUtf8 bs <> "</x>") of
  Left e -> Left ("Malformed XML: " ++ show e)
  Right n -> fromXenoNode n >>= \case
    Xmlbf.Element "x" _ cs -> Right cs
    _ -> Left "Unknown result from fromXenoNode. Please report this as a bug."

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Miscellaneous

decodeUtf8 :: B.ByteString -> Either String T.Text
{-# INLINE decodeUtf8 #-}
decodeUtf8 bs = Bif.first show (T.decodeUtf8' bs)

decodeUtf8Lazy :: B.ByteString -> Either String TL.Text
{-# INLINE decodeUtf8Lazy #-}
decodeUtf8Lazy bs = fmap TL.fromStrict (decodeUtf8 bs)

unescapeXmlUtf8 :: B.ByteString -> Either String T.Text
{-# INLINE unescapeXmlUtf8 #-}
unescapeXmlUtf8 bs = fmap TL.toStrict (unescapeXmlUtf8Lazy bs)

unescapeXmlUtf8Lazy :: B.ByteString -> Either String TL.Text
{-# INLINE unescapeXmlUtf8Lazy #-}
unescapeXmlUtf8Lazy bs = do
   t <- decodeUtf8 bs
   pure (TB.toLazyText (HTMLEntities.Decoder.htmlEncodedText t))

dropBomUtf8 :: B.ByteString -> B.ByteString
{-# INLINE dropBomUtf8 #-}
dropBomUtf8 bs | B.isPrefixOf "\xEF\xBB\xBF" bs = B.drop 3 bs
               | otherwise = bs
