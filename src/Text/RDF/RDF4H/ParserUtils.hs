{-# LANGUAGE ScopedTypeVariables #-}

module Text.RDF.RDF4H.ParserUtils(
  parseFromURL,
  Parser(..)
) where

import Data.RDF.Types

import Control.Exception.Lifted
import Network.HTTP.Conduit
import Data.Text.Encoding (decodeUtf8)
import Data.Semigroup ((<>))
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T

data Parser = Parsec | Attoparsec

-- | A convenience function for terminating a parse with a parse failure, using
-- the given error message as the message for the failure.
errResult :: String -> Either ParseFailure (RDF rdfImpl)
errResult msg = Left (ParseFailure msg)

parseFromURL :: (Rdf rdfImpl) => (T.Text -> Either ParseFailure (RDF rdfImpl)) -> String -> IO (Either ParseFailure (RDF rdfImpl))
parseFromURL parseFunc url = do
  result <- Control.Exception.Lifted.try $ simpleHttp url
  case result of
    Left (ex::HttpException) ->
      case ex of
        (HttpExceptionRequest _req content) ->
          case content of
            ConnectionTimeout ->
              return $ errResult "Connection timed out"
            _ -> return $ errResult ("HttpExceptionRequest content: " <> show content)
        (InvalidUrlException{}) ->
          return $ errResult "Invalid URL exception"
    Right bs -> do
      let s = decodeUtf8 $ BS.toStrict bs
      return (parseFunc s)
