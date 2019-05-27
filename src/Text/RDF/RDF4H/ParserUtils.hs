{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.RDF.RDF4H.ParserUtils
  ( Parser(..)
  , parseFromURL
  , rdfTypeNode, rdfNilNode, rdfFirstNode, rdfRestNode, rdfListIndex
  , rdfSubjectNode, rdfPredicateNode, rdfObjectNode, rdfStatementNode, rdfXmlLiteral
  , xsdIntUri, xsdDoubleUri, xsdDecimalUri, xsdBooleanUri
  ) where

import Data.RDF.Types
import Data.RDF.Namespace

import Control.Exception.Lifted
import Network.HTTP.Conduit
import Data.Text.Encoding (decodeUtf8)
import Data.Semigroup ((<>))
import qualified Data.ByteString.Lazy as BS
import           Data.Text (Text)
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
    Left (err :: HttpException) ->
      case err of
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

rdfTypeNode, rdfNilNode, rdfFirstNode, rdfRestNode :: Node
rdfTypeNode  = UNode $ mkUri rdf "type"
rdfNilNode   = UNode $ mkUri rdf "nil"
rdfFirstNode = UNode $ mkUri rdf "first"
rdfRestNode  = UNode $ mkUri rdf "rest"

rdfSubjectNode, rdfPredicateNode, rdfObjectNode, rdfStatementNode :: Node
rdfSubjectNode   = UNode $ mkUri rdf "subject"
rdfPredicateNode = UNode $ mkUri rdf "predicate"
rdfObjectNode    = UNode $ mkUri rdf "object"
rdfStatementNode = UNode $ mkUri rdf "Statement"

rdfListIndex, rdfXmlLiteral :: Text
rdfListIndex = mkUri rdf "_"
rdfXmlLiteral = mkUri rdf "XMLLiteral"

xsdIntUri, xsdDoubleUri, xsdDecimalUri, xsdBooleanUri :: Text
xsdIntUri     = mkUri xsd "integer"
xsdDoubleUri  = mkUri xsd "double"
xsdDecimalUri = mkUri xsd "decimal"
xsdBooleanUri = mkUri xsd "boolean"
