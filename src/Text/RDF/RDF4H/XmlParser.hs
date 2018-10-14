{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- |An parser for the RDF/XML format
-- <http://www.w3.org/TR/REC-rdf-syntax/>.

module Text.RDF.RDF4H.XmlParser
  -- (
  -- XmlParser'(XmlParser')
  -- , xmlEg
  -- )
where

import Text.RDF.RDF4H.ParserUtils (parseFromURL)

import qualified Control.Applicative as Applicative
import Control.Exception
import Control.Monad
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Maybe
import Data.RDF.Types
import Data.RDF.Graph.TList
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import Xmlbf
import qualified Xmlbf.Xeno as Xeno
  
data XmlParser = XmlParser (Maybe BaseUrl) (Maybe Text)

instance RdfParser XmlParser where
  parseString (XmlParser bUrl dUrl)  = parseXmlRDF bUrl dUrl
  parseFile   (XmlParser bUrl dUrl)  = parseFile' bUrl dUrl
  parseURL    (XmlParser bUrl dUrl)  = parseURL'  bUrl dUrl

parseFile' ::
     (Rdf a)
  => Maybe BaseUrl
  -> Maybe Text
  -> String
  -> IO (Either ParseFailure (RDF a))
parseFile' bUrl dUrl fpath =
   TIO.readFile fpath >>=  return . parseXmlRDF bUrl dUrl

parseURL' ::
     (Rdf a)
  => Maybe BaseUrl -- ^ The optional base URI of the document.
  -> Maybe Text -- ^ The document URI (i.e., the URI of the document itself); if Nothing, use location URI.
  -> String -- ^ The location URI from which to retrieve the XML document.
  -> IO (Either ParseFailure (RDF a)) -- ^ The parse result, which is either a @ParseFailure@ or the RDF
                                      --   corresponding to the XML document.
parseURL' bUrl docUrl = parseFromURL (parseXmlRDF bUrl docUrl)

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

testXeno :: Text -> Either String [Xmlbf.Node]
testXeno = Xeno.nodes . T.encodeUtf8

-- |Parse a xml Text to an RDF representation
parseXmlRDF :: (Rdf a)
            => Maybe BaseUrl     -- ^ The base URL for the RDF if required
            -> Maybe Text        -- ^ DocUrl: The request URL for the RDF if available
            -> Text              -- ^ The contents to parse
            -> Either ParseFailure (RDF a) -- ^ The RDF representation of the triples or ParseFailure
parseXmlRDF bUrl dUrl xmlStr =
  case Xeno.nodes (T.encodeUtf8 xmlStr) of
    Left xmlParseError -> Left (ParseFailure xmlParseError)
    Right nodes -> -- error (show nodes)
      case runParser rdfParser nodes of
        Left rdfParseError -> Left (ParseFailure rdfParseError)
        Right rdf -> Right rdf
-- TODO: use bUrl and dUrl

rdfParser :: Rdf a => Parser (RDF a)
rdfParser = do
  rdf <- rdfDescription
  void pEndOfInput
  return rdf

-- Text "\n"
-- TODO: check that all that follows from \n is zero or more ' ' characters.
newline :: Parser ()
-- newline = void $ (pText' "\n")
newline = do
  t <- pText
  if (T.take 1 (TL.toStrict t) == T.pack "\n")
  then pure ()
  else pFail "not a newline text node"

newlines :: Parser ()
newlines = void (many newline) 

{-
[ ("xmlns:si","https://www.w3schools.com/rdf/")
, ("xmlns:rdf","http://www.w3.org/1999/02/22-rdf-syntax-ns#")
]
-}
prefixes :: Parser [(Text,Text)]
prefixes = do
  xs <- HashMap.toList <$> pAttrs
  pure (map (\(k,v) -> (fromJust (T.stripPrefix "xmlns:" k),v)) xs)

rdfTriplesP :: Parser [Triple]
rdfTriplesP = do
  newlines
  pAnyElement $ do
    -- n <- pName
    subj <- unode <$> pAttr "rdf:about"
    predObjs <- many predObjP
    pure $ map (\(p,o) ->
                  triple subj p o
               ) predObjs
    -- (predicate,object) <- predObj
    -- pure $ triple subj predicate object
    
  -- subj <- unode <$> pAttr "rdf:about"
  -- x <- pName
  -- error (show x)
  -- (predText,obj) <- pElement' predObj
  -- pure $ triple subj (unode predText) obj

predObjP :: Parser (Data.RDF.Types.Node,Data.RDF.Types.Node)
predObjP = do
  void $ many newline
  pAnyElement $ do
    p <- unode <$> pName
    o <- objP
    pure (p,o)
    
  -- t <- pText
  -- pure $ lnode (plainL (TL.toStrict t))

-- TODO: unodes, and all different kinds of plain text nodes
objP :: Parser Data.RDF.Types.Node
objP = do
  -- typed literal
  theType <- pAttr "rdf:datatype"
  theText <- pText
  pure (lnode (typedL (TL.toStrict theText) theType))
  <|>
  -- plain literal
  ((lnode . plainL . TL.toStrict) <$> pText)



rdfDescription' :: Parser (PrefixMappings,Maybe BaseUrl,Triples)
rdfDescription' = do
  pfixes <- prefixes
  (_,triples) <- pElement' rdfTriplesP
  pure (PrefixMappings (Map.fromList pfixes), Nothing, triples)

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

test1 :: Bool
test1 = triplesOf got == expected
  where
    Right (got::RDF TList) = parseXmlRDF Nothing Nothing xmlEg
    expected =
      [ Triple
         (UNode "https://www.w3schools.com")
         (UNode "si:title")
         (LNode (PlainL "W3Schools"))
      , Triple
         (UNode "https://www.w3schools.com")
         (UNode "si:author")
         (LNode (PlainL "Jan Egil Refsnes"))
      ]
    
-- missing in Xmlbf

-- | @'pElement'' p@ runs a 'Parser' @p@ inside a element node and
-- returns a pair with the name of the parsed element and result of
-- @p@. This fails if such element does not exist at the current
-- position.
--
-- Leading whitespace is ignored. If you need to preserve that whitespace for
-- some reason, capture it using 'pText' before using 'pElement''.
--
-- Consumes the element from the parser state.
pElement' :: Parser a -> Parser (T.Text,a)
pElement' p = do
  res <- p
  name <- pName
  return (name,res)

pText' :: TL.Text -> Parser TL.Text
pText' t = do
  let pTextFail = pFail ("Missing text node " ++ show t)
  (do t' <- pText
      if t == t' then pure t
      else pTextFail
   <|> pTextFail)


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


---------------------------
-- Example trees from xeno

-- data/xml/example07.rdf
{-
[ Text "\n"
, Element "rdf:RDF"
    [ ("xmlns:dc","http://purl.org/dc/elements/1.1/")
    , ("xmlns:ex","http://example.org/stuff/1.0/")
    , ("xmlns:rdf","http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    ]
    [ Text "\n  "
    , Element "rdf:Description"
        [ ("rdf:about","http://www.w3.org/TR/rdf-syntax-grammar")
        , ("dc:title","RDF/XML Syntax Specification (Revised)")
        ]
        [ Text "\n    "
        , Element "ex:editor"
            []
            [ Text "\n      "
            , Element "rdf:Description"
                [ ("ex:fullName","Dave Beckett")
                ]
                [ Text "\n        "
                , Element "ex:homePage"
                    [ ("rdf:resource","http://purl.org/net/dajobe/")
                    ]
                    []
                , Text "\n      "
                ]
            , Text "\n    "
            ]
        , Text "\n  "
        ]
    , Text "\n"
    ]
, Text "\n"
]
-}
