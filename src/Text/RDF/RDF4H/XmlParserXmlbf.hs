{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- |An parser for the RDF/XML format
-- <http://www.w3.org/TR/REC-rdf-syntax/>.

module Text.RDF.RDF4H.XmlParserXmlbf (
  XmlParser'(XmlParser')
  , xmlEg
) where

import qualified Control.Applicative as Applicative
import Control.Exception
import Control.Monad
import Data.RDF.Types
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Xmlbf
import qualified Xmlbf.Xeno as Xeno
  
data XmlParser' = XmlParser' (Maybe BaseUrl) (Maybe Text)

instance RdfParser XmlParser' where
  parseString (XmlParser' bUrl dUrl)  = parseXmlRDF bUrl dUrl

-- |Global state for the parser
data GParseState = GParseState
  { stateGenId :: Int
  } deriving (Show)

-- |Local state for the parser (dependant on the parent xml elements)
data LParseState = LParseState { stateBaseUrl :: BaseUrl
                               , stateLang :: Maybe String
                               , stateSubject :: Subject
                               }
  deriving(Show)

data ParserException = ParserException String
                     deriving (Show)
instance Exception ParserException

-- |Parse a xml Text to an RDF representation
parseXmlRDF :: (Rdf a)
            => Maybe BaseUrl     -- ^ The base URL for the RDF if required
            -> Maybe Text        -- ^ DocUrl: The request URL for the RDF if available
            -> Text              -- ^ The contents to parse
            -> Either ParseFailure (RDF a) -- ^ The RDF representation of the triples or ParseFailure
parseXmlRDF bUrl dUrl xmlStr =
  case Xeno.nodes (T.encodeUtf8 xmlEg) of
    Left xmlParseError -> Left (ParseFailure xmlParseError)
    Right nodes ->
      case runParser rdfParser nodes of
        Left rdfParseError -> Left (ParseFailure rdfParseError)
        Right rdf -> Right rdf

rdfParser :: Rdf a => Parser (RDF a)
rdfParser = do
  rdf <- rdfDescription
  void pEndOfInput
  return rdf

-- Text "\n"
newline :: Parser ()
newline = void $ pure (text "\n")

{-
[ ("xmlns:si","https://www.w3schools.com/rdf/")
, ("xmlns:rdf","http://www.w3.org/1999/02/22-rdf-syntax-ns#")
]
-}
prefixes :: Parser (HM.HashMap Text Text)
prefixes = pAttrs

rdfDescription :: Rdf a => Parser (RDF a)
rdfDescription = do
  newline
  pfixes <- pElement "rdf:RDF" prefixes
  children <- pChildren
  error (show (head children))
  -- return Data.RDF.Types.empty

{-
[ Text "\n"
, Element
    "rdf:RDF"
    [ ("xmlns:si","https://www.w3schools.com/rdf/")
    , ("xmlns:rdf","http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    ]
    [ Text "\n"
    , Element
        "rdf:Description"
        [ ("rdf:about","https://www.w3schools.com") ]
        [ Text "\n"
        , Element
            "si:title"
            []
            [ Text "W3Schools" ]
        , Text "\n"
        , Element
             "si:author"
             []
             [ Text "Jan Egil Refsnes" ]
        , Text "\n"
        ]
    ,Text "\n"
    ]
,Text "\n"
]
-}
xmlEg = T.pack $ unlines
  [ "<?xml version=\"1.0\"?>"
  , "<rdf:RDF"
  , "xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\""
  , "xmlns:si=\"https://www.w3schools.com/rdf/\">"
  , "<rdf:Description rdf:about=\"https://www.w3schools.com\">"
  , "<si:title>W3Schools</si:title>"
  , "<si:author>Jan Egil Refsnes</si:author>"
  , "</rdf:Description>"
  , "</rdf:RDF>"
  ]


-- parser combinators missing in Xmlbf
between :: Parser a -> Parser b -> Parser c -> Parser c
between open close thing  = open *> thing <* close

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill thing z = many thing <* z

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) a b = a Applicative.<|> b

some :: Parser a -> Parser [a]
some = Applicative.some

many :: Parser a -> Parser [a]
many = Applicative.many

-- pElem :: Text -> Parser Text
-- oneOf :: Parser [a] -> Parser a
-- noneOf :: Parser [a] -> Parser a
