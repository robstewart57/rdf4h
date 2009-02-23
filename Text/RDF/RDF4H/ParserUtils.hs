module Text.RDF.RDF4H.ParserUtils(_parseURL, justTriples) where

import Text.RDF.RDF4H.Core

import Network.URI
import Network.HTTP
import Network.HTTP.Headers

import Data.Char(intToDigit)
import Data.ByteString.Lazy.Char8(ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

-- A convenience function for terminating a parse with a parse failure, using
-- the given error message as the message for the failure.
errResult :: Graph gr => String -> Either ParseFailure gr
errResult msg = Left (ParseFailure msg)

-- Keep the (Just t) triples (eliminating the Nothing comments), and unbox the
-- triples, leaving a list of triples.
justTriples :: [Maybe(Triple)] -> [Triple]
justTriples = map (maybe (error "ParserUtils.justTriples") id) .
              filter (/= Nothing)

_parseURL :: Graph gr => (ByteString -> Either ParseFailure gr)  -> String -> IO (Either ParseFailure gr)
_parseURL parseFunc url =
  return (parseURI url) >>=
    maybe (return (Left (ParseFailure $ "Unable to parse URL: " ++ url))) p
  where
    showRspCode (a, b, c) = map intToDigit [a, b, c]
    httpError resp = showRspCode (rspCode resp) ++ " " ++ rspReason resp
    p url =
      simpleHTTP (request url) >>= \resp ->
        case resp of
          (Left e)    -> return (errResult $ "couldn't retrieve from URL: " ++ show url ++ " [" ++ show e ++ "]")
          (Right res) -> case rspCode res of
                           (2, 0, 0) -> return $ parseFunc (rspBody res)
                           _         -> return (errResult $ "couldn't retrieve from URL: " ++ httpError res)

request :: URI -> HTTPRequest ByteString
request uri = Request { rqURI = uri,
                        rqMethod = GET,
                        rqHeaders = [Header HdrConnection "close"],
                        rqBody = B.empty }
