module Text.RDF.RDF4H.ParserUtils(
  _parseURL, justTriples,
  resolveQName, mkAbsoluteUrl, absolutizeUrl, isAbsoluteUri

) where

import Data.RDF.Types

import Network.URI
import Network.HTTP
import Data.Char(intToDigit)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- | A convenience function for terminating a parse with a parse failure, using
-- the given error message as the message for the failure.
errResult :: RDF rdf => String -> Either ParseFailure rdf
errResult msg = Left (ParseFailure msg)

-- | Keep the (Just t) triples (eliminating the Nothing comments), and unbox the
-- triples, leaving a list of triples.
justTriples :: [Maybe Triple] -> [Triple]
justTriples = map (fromMaybe (error "ParserUtils.justTriples")) .
              filter (/= Nothing)

-- | pars contents at URL in to and 'RDF'
_parseURL :: RDF rdf => (T.Text -> Either ParseFailure rdf)  -> String -> IO (Either ParseFailure rdf)
_parseURL parseFunc url =
  maybe
    (return (Left (ParseFailure $ "Unable to parse URL: " ++ url)))
    doParse
    (parseURI url)
  where
    showRspCode (a, b, c) = map intToDigit [a, b, c]
    httpError resp = showRspCode (rspCode resp) ++ " " ++ rspReason resp
    doParse url' =
      simpleHTTP (request url') >>= \resp ->
        case resp of
          (Left e)    -> return (errResult $ "couldn't retrieve from URL: " ++ show url' ++ " [" ++ show e ++ "]")
          (Right res) -> case rspCode res of
                           (2, 0, 0) -> return $ parseFunc (decodeUtf8 (rspBody res))
                           _         -> return (errResult $ "couldn't retrieve from URL: " ++ httpError res)

-- | construct HTTP GET request.
request :: URI -> HTTPRequest B.ByteString
request uri = Request { rqURI = uri,
                        rqMethod = GET,
                        rqHeaders = [Header HdrConnection "close"],
                        rqBody = B.empty }

-- Resolve a prefix using the given prefix mappings and base URL. If the prefix is
-- empty, then the base URL will be used if there is a base URL and if the map
-- does not contain an entry for the empty prefix.
resolveQName :: Maybe BaseUrl -> T.Text -> PrefixMappings -> T.Text
resolveQName mbaseUrl prefix (PrefixMappings pms') =
  case (mbaseUrl, T.null prefix) of
    (Just (BaseUrl base), True)  ->  Map.findWithDefault base T.empty pms'
    (Nothing,             True)  ->  err1
    (_,                   _   )  ->  Map.findWithDefault err2 prefix pms'
  where
    err1 = error  "Cannot resolve empty QName prefix to a Base URL."
    err2 = error ("Cannot resolve QName prefix: " ++ T.unpack prefix)

-- Resolve a URL fragment found on the right side of a prefix mapping by converting it to an absolute URL if possible.
absolutizeUrl :: Maybe BaseUrl -> Maybe T.Text -> T.Text -> T.Text
absolutizeUrl mbUrl mdUrl urlFrag =
  if isAbsoluteUri urlFrag then urlFrag else
    (case (mbUrl, mdUrl) of
         (Nothing, Nothing) -> urlFrag
         (Just (BaseUrl bUrl), Nothing) -> bUrl `T.append` urlFrag
         (Nothing, Just dUrl) -> if isHash urlFrag then
                                     dUrl `T.append` urlFrag else urlFrag
         (Just (BaseUrl bUrl), Just dUrl) -> (if isHash urlFrag then dUrl
                                                  else bUrl)
                                                 `T.append` urlFrag)
  where
    isHash bs' = T.length bs' == 1 && T.head bs' == '#'

{-# INLINE isAbsoluteUri #-}
isAbsoluteUri :: T.Text -> Bool
isAbsoluteUri = T.isInfixOf (T.pack [':'])

{-# INLINE mkAbsoluteUrl #-}
-- Make an absolute URL by returning as is if already an absolute URL and otherwise
-- appending the URL to the given base URL.
mkAbsoluteUrl :: T.Text -> T.Text -> T.Text
mkAbsoluteUrl base url =
  if isAbsoluteUri url then url else base `T.append` url
