module Text.RDF.RDF4H.ParserUtils(
  _parseURL, justTriples
) where

import Data.RDF.Types

import Network.URI
import Network.HTTP
import Data.Char (intToDigit)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text.Lazy as T
-- import qualified Data.Map as Map
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

-- | Parse contents at URL into an 'RDF'.
_parseURL :: RDF rdf => (T.Text -> Either ParseFailure rdf) -> String -> IO (Either ParseFailure rdf)
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

-- | Construct HTTP GET request.
request :: URI -> HTTPRequest B.ByteString
request uri = Request { rqURI = uri,
                        rqMethod = GET,
                        rqHeaders = [Header HdrConnection "close"],
                        rqBody = B.empty }
