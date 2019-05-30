{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Text.RDF.RDF4H.XmlParser.Identifiers
  (
  -- Validation
    validateID
  , resolveQName, resolveQName'
  , parseQName
  ) where


import           Data.Functor ((<$))
import           Control.Applicative (liftA2, Alternative(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Attoparsec.Text (Parser, (<?>))
import qualified Data.Attoparsec.Text as P

import           Data.RDF.Namespace

-- IRI processing
validateID :: Text -> Either String Text
validateID t = t <$ parseId t

parseId :: Text -> Either String Text
parseId = P.parseOnly $ pNCName <* (P.endOfInput <?> "Unexpected characters at the end")

resolveQName :: PrefixMappings -> Text -> Either String Text
resolveQName pm qn = parseQName qn >>= resolveQName' pm

resolveQName' :: PrefixMappings -> (Maybe Text, Text) -> Either String Text
resolveQName' (PrefixMappings pm) (Nothing, name) =
  case Map.lookup mempty pm of
    Nothing  -> Left $ mconcat ["Cannot resolve QName \"", T.unpack name, "\": no default namespace defined."]
    Just iri -> Right $ iri <> name
resolveQName' (PrefixMappings pm) (Just prefix, name) =
  case Map.lookup prefix pm of
    Nothing  -> Left $ mconcat ["Cannot resolve QName: prefix \"", T.unpack prefix, "\" not defined"]
    Just iri -> Right $ iri <> name

parseQName :: Text -> Either String (Maybe Text, Text)
parseQName = P.parseOnly $ pQName <* (P.endOfInput <?> "Unexpected characters at the end of a QName")

-- https://www.w3.org/TR/xml-names/#ns-qualnames
pQName :: Parser (Maybe Text, Text)
pQName = pPrefixedName <|> pUnprefixedNamed
  where pUnprefixedNamed = (empty,) <$> pLocalPart

pPrefixedName :: Parser (Maybe Text, Text)
pPrefixedName = do
  prefix <- pLocalPart <* P.char ':'
  localPart <- pLocalPart
  pure (Just prefix, localPart)

pLocalPart :: Parser Text
pLocalPart = pNCName

-- http://www.w3.org/TR/REC-xml-names/#NT-NCName
pNCName :: Parser Text
pNCName = liftA2 T.cons pNameStartChar pNameRest
  where
    pNameStartChar = P.satisfy isValidFirstCharId
    pNameRest = P.takeWhile isValidRestCharId
    isValidFirstCharId c =
         ('A' <= c && c <= 'Z') || c == '_' || ('a' <= c && c <= 'z')
      || ('\xC0' <= c && c <= '\xD6') || ('\xD8' <= c && c <= '\xF6')
      || ('\xF8' <= c && c <= '\x2FF') || ('\x370' <= c && c <= '\x37D')
      || ('\x37F' <= c && c <= '\x1FFF') || ('\x200C' <= c && c <= '\x200D')
      || ('\x2070' <= c && c <= '\x218F') || ('\x2C00' <= c && c <= '\x2FEF')
      || ('\x3001' <= c && c <= '\xD7FF') || ('\xF900' <= c && c <= '\xFDCF')
      || ('\xFDF0' <= c && c <= '\xFFFD') || ('\x10000' <= c && c <= '\xEFFFF')
    isValidRestCharId c = isValidFirstCharId c
      || c == '-' || c == '.' || ('0' <= c && c <= '9')
      || ('\x0300' <= c && c <= '\x036F') || ('\x203F' <= c && c <= '\x2040')
