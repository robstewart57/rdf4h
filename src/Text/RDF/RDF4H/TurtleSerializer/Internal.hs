{-# LANGUAGE OverloadedStrings #-}

module Text.RDF.RDF4H.TurtleSerializer.Internal
  ( findMapping,
    writeUNodeUri,
  )
where

import Data.List (elemIndex)
import qualified Data.Map as Map
import Data.Monoid (Any (..), getAny)
import Data.RDF.Namespace hiding (rdf)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO

-- | Converts an aliased URI (e.g., 'rdf:subject') to a tuple whose first element
--  is the full (non-aliased) URI and whose second element is the target/path
--  portion (the part after the colon in the aliased URI).
findMapping ::
  -- | The 'PrefixMappings' to be searched for the prefix that may be a part of the URI.
  PrefixMappings ->
  -- | The URI.
  T.Text ->
  Maybe (T.Text, T.Text)
findMapping (PrefixMappings pms) aliasedURI = do
  (prefix, target) <- splitAliasedURI aliasedURI
  uri <- Map.lookup prefix pms
  pure (uri, target)

-- | Writes the given 'UNode' to the given 'Handle'.
writeUNodeUri ::
  -- | The Handle to write to
  Handle ->
  -- | The text from a UNode
  T.Text ->
  -- | The 'PrefixMappings' which should contain a mapping for any prefix found in the URI.
  PrefixMappings ->
  IO ()
writeUNodeUri h uri _ =
  if (isQName uri)
    then T.hPutStr h uri
    else hPutChar h '<' >> T.hPutStr h uri >> hPutChar h '>'

isQName :: T.Text -> Bool
isQName = not . isFullURI
  where
    isFullURI :: T.Text -> Bool
    isFullURI =
      getAny
        . foldMap
          (Any .)
          [ ("http://" `T.isPrefixOf`),
            ("https://" `T.isPrefixOf`),
            ("file://" `T.isPrefixOf`)
          ]

-- | Given an aliased URI (e.g., 'rdf:subject') return a tuple whose first
--  element is the alias ('rdf') and whose second part is the path or fragment
--  ('subject').
splitAliasedURI ::
  -- | Aliased URI.
  T.Text ->
  Maybe (T.Text, T.Text)
splitAliasedURI uri = do
  let uriStr = T.unpack uri
  i <- elemIndex ':' uriStr
  let (prefix, target) = splitAt i uriStr
  pure (T.pack prefix, T.pack $ tail target)
