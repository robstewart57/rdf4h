module Text.RDF.RDF4H.TurtleSerializer.Internal
  ( findMapping
  , writeUNodeUri
  )
where

import           Data.List (elemIndex)
import qualified Data.Map as Map
import           Data.RDF.Namespace hiding (rdf)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.IO

-- Expects a map from uri to prefix, and returns the (prefix, uri_expansion)
-- from the mappings such that uri_expansion is a prefix of uri, or Nothing if
-- there is no such mapping. This function does a linear-time search over the
-- map, but the prefix mappings should always be very small, so it's okay for now.
findMapping :: PrefixMappings -> T.Text -> Maybe (T.Text, T.Text)
findMapping (PrefixMappings pms) aliasedURI = do
  (prefix, target) <- splitAliasedURI aliasedURI
  uri <- Map.lookup prefix pms
  pure (uri, target)

writeUNodeUri :: Handle -> T.Text -> PrefixMappings -> IO ()
writeUNodeUri h uri pms =
  case mapping of
    Nothing -> hPutChar h '<' >> T.hPutStr h uri >> hPutChar h '>'
    (Just (pre, localName)) -> T.hPutStr h pre >> T.hPutStr h localName
  where
    mapping = findMapping pms uri

-- |Given an aliased URI (e.g., 'rdf:subject') return a tuple whose first
-- element is the aliased ('rdf') and whose second part is the path or fragment
-- ('subject').
splitAliasedURI :: T.Text -> Maybe (T.Text, T.Text)
splitAliasedURI uri = do
  let uriStr = T.unpack uri
  i <- elemIndex ':' uriStr
  let (prefix, target) = splitAt i uriStr
  pure (T.pack prefix, T.pack $ tail target)
