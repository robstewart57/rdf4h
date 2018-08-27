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
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Maybe
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
    Right nodes -> -- error (show nodes)
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
newline = void $ pText' "\n"
-- newline = pure (text "\n") *> pure ()

{-
[ ("xmlns:si","https://www.w3schools.com/rdf/")
, ("xmlns:rdf","http://www.w3.org/1999/02/22-rdf-syntax-ns#")
]
-}
prefixes :: Parser [(Text,Text)]
prefixes = do
  xs <- HashMap.toList <$> pAttrs
  pure (map (\(k,v) -> (fromJust (T.stripPrefix "xmlns:" k),v)) xs)

rdfTriple :: Parser Triple
rdfTriple = do
  newline
  -- t <- pText
  -- error (show t)
  -- void newline
  subj <- unode <$> pAttr "rdf:about"
  -- x <- pName
  -- error (show x)
  (predText,obj) <- pElement' predObj
  -- x <- pElement "rdf:Description"
  pure $ triple subj (unode predText) obj

predObj :: Parser (Data.RDF.Types.Node)
predObj = do
  t <- pText
  pure $ lnode (plainL t)
--   t <- pName
--   error (show t)
  -- newline
  -- x <- pName
  -- error (show x)

rdfDescription' :: Parser (PrefixMappings,Maybe BaseUrl,Triples)
rdfDescription' = do
  pfixes <- prefixes
  (_,triple) <- pElement' rdfTriple
  -- error (show e)
  pure (PrefixMappings (Map.fromList pfixes), Nothing, [triple])

rdfDescription :: Rdf a => Parser (RDF a)
rdfDescription = do
  (pfixes,bUrl,triples) <- pElement "rdf:RDF" rdfDescription'
  -- error (show $ pfixes)
  many newline
  pure $ mkRdf triples bUrl pfixes

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
