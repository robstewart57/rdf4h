{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.RDF.RDF4H.ParserUtils
  ( Parser(..)
  , parseFromURL
  -- RDF
  , rdfTypeNode, rdfNilNode, rdfFirstNode, rdfRestNode
  , rdfSubjectNode, rdfPredicateNode, rdfObjectNode, rdfStatementNode
  , rdfTag, rdfID, rdfAbout, rdfParseType, rdfResource, rdfNodeID, rdfDatatype
  , rdfType, rdfLi, rdfListIndex
  , rdfDescription, rdfXmlLiteral
  , rdfAboutEach, rdfAboutEachPrefix, rdfBagID
  -- XML
  , xmlLang
  -- XSD
  , xsdIntUri, xsdDoubleUri, xsdDecimalUri, xsdBooleanUri
  -- for GHC 8.0 compatibility
#if MIN_VERSION_base(4,10,0)
#else
  , fromRight
#endif
  ) where

import Data.RDF.Types
import Data.RDF.Namespace

import Control.Exception.Lifted
import Network.HTTP.Conduit
import Data.Text.Encoding (decodeUtf8)
#if MIN_VERSION_base(4,9,0)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#else
#endif
#else
#endif
import qualified Data.ByteString.Lazy as BS
import           Data.Text (Text)
import qualified Data.Text as T

#if MIN_VERSION_base(4,10,0)
#else
fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b
#endif

data Parser = Parsec | Attoparsec

-- | A convenience function for terminating a parse with a parse failure, using
-- the given error message as the message for the failure.
errResult :: String -> Either ParseFailure (RDF rdfImpl)
errResult msg = Left (ParseFailure msg)

parseFromURL :: (T.Text -> Either ParseFailure (RDF rdfImpl)) -> String -> IO (Either ParseFailure (RDF rdfImpl))
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

-- Core terms
rdfTag, rdfID, rdfAbout, rdfParseType, rdfResource, rdfNodeID, rdfDatatype :: Text
rdfTag = mkUri rdf "RDF"
rdfID = mkUri rdf "ID"
rdfAbout = mkUri rdf "about"
rdfParseType = mkUri rdf "parseType"
rdfResource = mkUri rdf "resource"
rdfNodeID = mkUri rdf "nodeID"
rdfDatatype = mkUri rdf "datatype"

rdfType, rdfLi, rdfListIndex :: Text
rdfType = mkUri rdf "type"
rdfLi = mkUri rdf "li"
rdfListIndex = mkUri rdf "_"

rdfXmlLiteral, rdfDescription :: Text
rdfXmlLiteral = mkUri rdf "XMLLiteral"
rdfDescription = mkUri rdf "Description"

-- Old terms
rdfAboutEach, rdfAboutEachPrefix, rdfBagID :: Text
rdfAboutEach = mkUri rdf "aboutEach"
rdfAboutEachPrefix = mkUri rdf "aboutEachPrefix"
rdfBagID = mkUri rdf "bagID"

xmlLang :: Text
xmlLang = mkUri xml "lang"

xsdIntUri, xsdDoubleUri, xsdDecimalUri, xsdBooleanUri :: Text
xsdIntUri     = mkUri xsd "integer"
xsdDoubleUri  = mkUri xsd "double"
xsdDecimalUri = mkUri xsd "decimal"
xsdBooleanUri = mkUri xsd "boolean"
