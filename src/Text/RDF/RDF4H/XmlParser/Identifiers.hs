{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Text.RDF.RDF4H.XmlParser.Identifiers
  ( -- rdf:ID validation
    checkRdfId
    -- Qualified names
  , resolveQName, resolveQName'
  , parseQName
  ) where


#if !MIN_VERSION_base(4,13,0)
import           Data.Functor ((<$))
#else
#endif
import           Control.Applicative (liftA2, Alternative(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Attoparsec.Text (Parser, (<?>))
import qualified Data.Attoparsec.Text as P
#if MIN_VERSION_base(4,9,0)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#else
#endif
#else
#endif
import           Data.RDF.Namespace

--------------------------------------------------------------------------------
-- rdf:ID

-- |Validate the value of @rdf:ID@.
--
-- See: https://www.w3.org/TR/rdf-syntax-grammar/#rdf-id
checkRdfId
  :: Text
  -- ^ Value of a @rdf:ID@ attribute to validate.
  -> Either String Text
checkRdfId t = t <$ parseId t

parseId :: Text -> Either String Text
parseId = P.parseOnly $ pNCName <* (P.endOfInput <?> "Unexpected characters at the end")

--------------------------------------------------------------------------------
-- Qualified names

-- |Parse and resolve a qualified name.
--
-- See: https://www.w3.org/TR/xml-names/#ns-qualnames
resolveQName
  :: PrefixMappings
  -- ^ Namespace mapping to resolve q qualified name.
  -> Text
  -- ^ Raw qualified name to process.
  -> Either String Text
resolveQName pm qn = parseQName qn >>= resolveQName' pm

-- |Resolve a qualified name.
resolveQName'
  :: PrefixMappings
  -- ^ Namespace mapping to resolve q qualified name.
  -> (Maybe Text, Text)
  -- ^ (namespace, local name)
  -> Either String Text
resolveQName' (PrefixMappings pm) (Nothing, name) =
  case Map.lookup mempty pm of
    Nothing  -> Left $ mconcat ["Cannot resolve QName \"", T.unpack name, "\": no default namespace defined."]
    Just iri -> Right $ iri <> name
resolveQName' (PrefixMappings pm) (Just prefix, name) =
  case Map.lookup prefix pm of
    Nothing  -> Left $ mconcat ["Cannot resolve QName: prefix \"", T.unpack prefix, "\" not defined"]
    Just iri -> Right $ iri <> name

-- |Parse a qualified name.
--
-- See: https://www.w3.org/TR/xml-names/#ns-qualnames
parseQName :: Text -> Either String (Maybe Text, Text)
parseQName = P.parseOnly $ pQName <* (P.endOfInput <?> "Unexpected characters at the end of a QName")

-- https://www.w3.org/TR/xml-names/#ns-qualnames
-- https://www.w3.org/TR/xml-names/#NT-QName
pQName :: Parser (Maybe Text, Text)
pQName = pPrefixedName <|> pUnprefixedNamed
  where pUnprefixedNamed = (empty,) <$> pLocalPart

-- https://www.w3.org/TR/xml-names/#NT-PrefixedName
pPrefixedName :: Parser (Maybe Text, Text)
pPrefixedName = do
  prefix <- pLocalPart <* P.char ':'
  localPart <- pLocalPart
  pure (Just prefix, localPart)

-- https://www.w3.org/TR/xml-names/#NT-LocalPart
pLocalPart :: Parser Text
pLocalPart = pNCName

-- http://www.w3.org/TR/REC-xml-names/#NT-NCName
pNCName :: Parser Text
pNCName = liftA2 T.cons pNameStartChar pNameRest
  where
    pNameStartChar = P.satisfy isValidFirstCharId
    pNameRest = P.takeWhile isValidRestCharId
    isValidFirstCharId c
      =  ('A' <= c && c <= 'Z') || c == '_' || ('a' <= c && c <= 'z')
      || ('\xC0' <= c && c <= '\xD6') || ('\xD8' <= c && c <= '\xF6')
      || ('\xF8' <= c && c <= '\x2FF') || ('\x370' <= c && c <= '\x37D')
      || ('\x37F' <= c && c <= '\x1FFF') || ('\x200C' <= c && c <= '\x200D')
      || ('\x2070' <= c && c <= '\x218F') || ('\x2C00' <= c && c <= '\x2FEF')
      || ('\x3001' <= c && c <= '\xD7FF') || ('\xF900' <= c && c <= '\xFDCF')
      || ('\xFDF0' <= c && c <= '\xFFFD') || ('\x10000' <= c && c <= '\xEFFFF')
    isValidRestCharId c = isValidFirstCharId c
      || c == '-' || c == '.' || ('0' <= c && c <= '9')
      || ('\x0300' <= c && c <= '\x036F') || ('\x203F' <= c && c <= '\x2040')
