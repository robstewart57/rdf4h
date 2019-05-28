module Text.RDF.RDF4H.XmlParser.Utils
  ( validateID
  ) where


import           Data.Functor ((<$))
import           Control.Applicative (liftA2)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Attoparsec.Text (Parser, (<?>))
import qualified Data.Attoparsec.Text as P


validateID :: Text -> Either String Text
validateID t = t <$ parseId t

parseId :: Text -> Either String Text
parseId = P.parseOnly $ idParser <* (P.endOfInput <?> "Unexpected characters at the end")

-- http://www.w3.org/TR/REC-xml-names/#NT-NCName
idParser :: Parser Text
idParser = liftA2 T.cons pNameStartChar pNameRest
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
