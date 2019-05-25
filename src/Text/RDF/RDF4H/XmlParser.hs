{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |An parser for the RDF/XML format
-- <http://www.w3.org/TR/REC-rdf-syntax/>.

module Text.RDF.RDF4H.XmlParser
  ( XmlParser(..)
  , xmlEg
  , example11
  , example12
  ) where

import Text.RDF.RDF4H.ParserUtils hiding (Parser)
--import Data.RDF.IRI
import Data.RDF.Types hiding (empty)
--import Data.RDF.Graph.TList

--import Debug.Trace
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.State.Strict
import Data.Semigroup ((<>))
import qualified Data.Map as Map
--import Data.Maybe
import Data.Bifunctor
--import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
--import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import Xmlbf hiding (Node, Parser)
--import qualified Xmlbf (Node)
import qualified Xmlbf.Xeno as Xeno

data XmlParser = XmlParser (Maybe BaseUrl) (Maybe Text)

instance RdfParser XmlParser where
  parseString (XmlParser bUrl dUrl) = parseXmlRDF bUrl dUrl
  parseFile   (XmlParser bUrl dUrl) = parseFile'  bUrl dUrl
  parseURL    (XmlParser bUrl dUrl) = parseURL'   bUrl dUrl

parseFile' ::
     (Rdf a)
  => Maybe BaseUrl
  -> Maybe Text
  -> String
  -> IO (Either ParseFailure (RDF a))
parseFile' bUrl dUrl fpath = parseXmlRDF bUrl dUrl <$> TIO.readFile fpath

parseURL' ::
     (Rdf a)
  => Maybe BaseUrl -- ^ The optional base URI of the document.
  -> Maybe Text -- ^ The document URI (i.e., the URI of the document itself); if Nothing, use location URI.
  -> String -- ^ The location URI from which to retrieve the XML document.
  -> IO (Either ParseFailure (RDF a)) -- ^ The parse result, which is either a @ParseFailure@ or the RDF
                                      --   corresponding to the XML document.
parseURL' bUrl docUrl = parseFromURL (parseXmlRDF bUrl docUrl)

-- -- |Global state for the parser
-- data GParseState = GParseState
--   { stateGenId :: Int
--   } deriving (Show)


type Parser = ParserT (State ParseState)

-- |Local state for the parser (dependant on the parent xml elements)
data ParseState = ParseState
  { stateBaseUri :: Maybe BaseUrl
  , stateLang :: Maybe Text
  , stateSubject :: Maybe Subject
  , stateGenId :: Int
  } deriving(Show)

data ParserException = ParserException String
                     deriving (Show)
instance Exception ParserException

-- testXeno :: Text -> Either String [Xmlbf.Node]
-- testXeno = Xeno.fromRawXml . T.encodeUtf8

-- |Parse a xml Text to an RDF representation
parseXmlRDF :: (Rdf a)
            => Maybe BaseUrl     -- ^ The base URL for the RDF if required
            -> Maybe Text        -- ^ DocUrl: The request URL for the RDF if available
            -> Text              -- ^ The contents to parse
            -> Either ParseFailure (RDF a) -- ^ The RDF representation of the triples or ParseFailure
parseXmlRDF bUrl dUrl = parseRdf . parseXml
  where
    parseXml = Xeno.fromRawXml . T.encodeUtf8
    parseRdf = first ParseFailure . join . second parseRdf'
    parseRdf' ns = evalState (runParserT (rdfParser bUrl dUrl) ns) initState
    initState = ParseState bUrl empty empty 0
-- TODO: use bUrl and dUrl

rdfParser :: Rdf a => Maybe BaseUrl -> Maybe Text -> Parser (RDF a)
rdfParser _bUrl _dUrl = do
  rdf <- pRdf
  pWs
  pEndOfInput
  return rdf

{- NOTE:
  remember to use `showTree` in the fork of xmlbf when pEndOfInput needs
  debugging.
-}

pRdf :: Rdf a => Parser (RDF a)
pRdf = pElement "rdf:RDF" $ do
  bUri <- optional pBaseUri
  setBaseUri bUri
  pm <- pPrefixMappings
  -- [TODO] Ensure no attributes
  triples <- pNodeElementList
  pure $ mkRdf triples Nothing pm

pPrefixMappings :: Parser PrefixMappings
pPrefixMappings = PrefixMappings <$> pm
  where
    pm = Map.fromList . HM.foldlWithKey' getPrefixes mempty <$> pAttrs
    getPrefixes ps k v = maybe ps (\k' -> (k', v):ps) (T.stripPrefix "xmlns:" k)

pBaseUri :: Parser BaseUrl
pBaseUri = BaseUrl <$> pAttr "xml:base"

pNodeElementList :: Parser Triples
pNodeElementList = pWs *> (mconcat <$> many pNodeElement)

-- |White spaces parser
pWs :: Parser ()
pWs = maybe True (T.all ws . TL.toStrict) <$> optional pText >>= guard
  where
    -- See: https://www.w3.org/TR/2000/REC-xml-20001006#NT-S
    ws c = c == '\x20' || c == '\x09' || c == '\x0d' || c == '\x0a'

pNodeElement :: Parser Triples
pNodeElement = pAnyElement $ do
  (s, ts1) <- pSubject
  ts2 <- pPropertyAttr
  ts3 <- pPropertyEltList
  setSubject (Just s)
  pure $ mconcat [ts1, ts2, ts3]

pSubject :: Parser (Node, Triples)
pSubject = do
  s <- pUnodeId <|> pBnode <|> pUnode <|> pBnodeGen
  uri <- pName
  pLang >>= setLang
  mtype <- optional (pType1 s uri)
  ts <- pPropertyAttrs s
  pure (s, (maybe ts (:ts) mtype))
  where
    pUnodeId = do
      nid <- pIdAttr
      -- [FIXME] undefined
      mkUNodeID nid >>= maybe undefined pure
    pBnode = do
      bn <- pNodeIdAttr
      let s = BNode bn
      setSubject (Just s)
      pure s
    -- Default subject: a new blank node
    pUnode = do
      s <- unode <$> pAboutAttr
      setSubject (Just s)
      pure s
    pBnodeGen = do
      s <- newBNode
      setSubject (Just s)
      pure s
    pType1 n uri =
      if uri /= "rdf:Description"
        then pure $ Triple n rdfTypeNode (unode uri)
        else empty

pPropertyAttrs :: Node -> Parser Triples
pPropertyAttrs s = do
  attrs <- pAttrs
  HM.elems <$> HM.traverseWithKey f attrs
  where
    -- [TODO] resolve IRIs
    f attr value = if attr == "rdf:type"
      then pure $ Triple s rdfTypeNode (unode value)
      else do
        lang <- currentLang
        pure $ let mkLiteral = maybe plainL (flip plainLL) lang
               in Triple s (unode attr) (lnode (mkLiteral value))

pLang :: Parser (Maybe Text)
pLang = optional (pAttr "xml:lang")

pPropertyEltList :: Parser Triples
pPropertyEltList = pWs *> fmap mconcat (many (pPropertyElt <* pWs))

pPropertyElt :: Parser Triples
pPropertyElt = pAnyElement $ do
   p <- unode <$> pName
   -- [TODO] check URI
   pParseTypeLiteralPropertyElt p
     -- <|> pLiteralPropertyElt p
     -- <|> pParseTypeLiteralPropertyElt p
     <|> pParseTypeResourcePropertyElt p
     <|> pParseTypeCollectionPropertyElt p
     <|> pParseTypeOtherPropertyElt p
     <|> pResourcePropertyElt p
     <|> pLiteralPropertyElt p
     <|> pEmptyPropertyElt p

pResourcePropertyElt :: Node -> Parser Triples
pResourcePropertyElt p = do
  -- [TODO] idAttr
  -- [TODO] rdf:ID
  pWs
  s <- currentSubject
  ts <- pNodeElement
  o <- currentSubject
  pWs
  let mt = flip Triple p <$> s <*> o
  pure $ maybe ts (:ts) mt

pLiteralPropertyElt :: Node -> Parser Triples
pLiteralPropertyElt p = do
  -- [TODO] idAttr
  dt <- optional (pAttr "rdf:datatype")
  t <- pText
  s <- currentSubject
  lang <- liftA2 (<|>) pLang currentLang
  let t' = TL.toStrict t
  let literal = maybe (plainL t') id $ (typedL t' <$> dt) <|> (plainLL t' <$> lang)
  pure $ maybe mempty (\s' -> [Triple s' p (lnode literal)]) s

pParseTypeLiteralPropertyElt :: Node -> Parser Triples
pParseTypeLiteralPropertyElt _p = do
  -- [TODO] idAttr
  pt <- pAttr "rdf:parseType"
  guard (pt == "Literal")
  pFail "TODO" -- [TODO]

pParseTypeResourcePropertyElt :: Node -> Parser Triples
pParseTypeResourcePropertyElt p = do
  -- [TODO] idAttr
  pt <- pAttr "rdf:parseType"
  guard (pt == "Resource")
  s <- currentSubject
  o <- newBNode
  let ts = maybe mempty (\s' -> [Triple s' p o]) s
  -- setSubject (Just o)
  -- pPropertyEltList [TODO]
  pure ts

pParseTypeCollectionPropertyElt :: Node -> Parser Triples
pParseTypeCollectionPropertyElt _p = do
  -- [TODO] idAttr
  pt <- pAttr "rdf:parseType"
  guard (pt == "Collection")
  -- pNodeElementList
  -- pWs *> (mconcat <$> many pNodeElement)
  pFail "TODO" -- [TODO]

pParseTypeOtherPropertyElt :: Node -> Parser Triples
pParseTypeOtherPropertyElt _p = do
  -- [TODO] idAttr
  pt <- pAttr "rdf:parseType"
  guard (pt /= "Resource" && pt /= "Literal" && pt /= "Collection")
  pFail "TODO" -- [TODO]

pEmptyPropertyElt :: Node -> Parser Triples
pEmptyPropertyElt p = do
  -- [TODO] idAttr, rdf:ID
  s <- currentSubject
  case s of
    Nothing -> pure mempty
    Just s' -> do
      o <- pResourceAttr' <|> pNodeIdAttr' <|> newBNode
      ts <- pPropertyAttrs o
      pure (Triple s' p o : ts)
  where
    pResourceAttr' = unode <$> pResourceAttr
    pNodeIdAttr' = BNode <$> pNodeIdAttr

pIdAttr :: Parser Text
pIdAttr = pAttr "rdf:ID" -- [TODO] Check

pNodeIdAttr :: Parser Text
pNodeIdAttr = pAttr "rdf:nodeID" -- [TODO] Check

pAboutAttr :: Parser Text
pAboutAttr = pAttr "rdf:about" >>= checkIRI "rdf:about"

pResourceAttr :: Parser Text
pResourceAttr = pAttr "rdf:resource" >>= checkIRI "rdf:resource"

pDatatypeAttr :: Parser Text
pDatatypeAttr = pAttr "rdf:datatype" >>= checkIRI "rdf:datatype"

-- [TODO]
pPropertyAttr :: Parser Triples
pPropertyAttr = do
  attrs <- HM.filterWithKey (\iri _ -> iri /= "rdf:type") <$> pAttrs
  s <- currentSubject
  lang <- currentLang
  let mkLiteral = lnode . maybe plainL (flip plainLL) lang
  pure $ maybe
    mempty
    (\s' -> HM.elems $ HM.mapWithKey (mkTriple s' mkLiteral) attrs)
    s
  where
    mkTriple s mkLiteral iri value = Triple s (unode iri) (mkLiteral value)

checkIRI :: String -> Text -> Parser Text
checkIRI msg = maybe (pFail ("Malformed IRI: " <> msg)) pure . uriValidate

newBNode :: Parser Node
newBNode = do
  modify $ \st -> st { stateGenId = stateGenId st + 1 }
  st <- get
  pure $ BNodeGen (stateGenId st)

currentBaseUri :: Parser (Maybe BaseUrl)
currentBaseUri = stateBaseUri <$> get

setBaseUri :: (Maybe BaseUrl) -> Parser ()
setBaseUri u = modify (\st -> st { stateBaseUri = u })

mkUNodeID :: Text -> Parser (Maybe Node)
mkUNodeID t = currentBaseUri >>= \case
  Nothing          -> pure Nothing
  Just (BaseUrl u) -> pure . Just . unode $ mconcat [u, "#", t]

currentSubject :: Parser (Maybe Subject)
currentSubject = stateSubject <$> get

setSubject :: (Maybe Subject) -> Parser ()
setSubject s = modify (\st -> st { stateSubject = s })

currentLang :: Parser (Maybe Text)
currentLang = stateLang <$> get

setLang :: (Maybe Text) -> Parser ()
setLang lang = modify (\st -> st { stateLang = lang })

example11 :: Text
example11 = T.pack $ unlines
  [ "<?xml version=\"1.0\"?>"
  , "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\""
  , "        xmlns:dc=\"http://purl.org/dc/elements/1.1/\""
  , "         xmlns:ex=\"http://example.org/stuff/1.0/\">"
  , "  <rdf:Description rdf:about=\"http://www.w3.org/TR/rdf-syntax-grammar\""
  , "   dc:title=\"RDF/XML Syntax Specification (Revised)\">"
  , "  <ex:editor rdf:nodeID=\"abc\"/>"
  , "  </rdf:Description>"
  , "  <rdf:Description rdf:nodeID=\"abc\""
  , "                  ex:fullName=\"Dave Beckett\">"
  , "<ex:homePage rdf:resource=\"http://purl.org/net/dajobe/\"/>"
  , "</rdf:Description>"
  , "</rdf:RDF>"
  ]

example12 :: Text
example12 = T.pack $ unlines
  [ "<?xml version=\"1.0\"?>"
  , "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\""
  , "         xmlns:dc=\"http://purl.org/dc/elements/1.1/\""
  , "         xmlns:ex=\"http://example.org/stuff/1.0/\">"
  , "  <rdf:Description rdf:about=\"http://www.w3.org/TR/rdf-syntax-grammar\""
  , "   dc:title=\"RDF/XML Syntax Specification (Revised)\">"
  , "    <ex:editor rdf:parseType=\"Resource\">"
  , "      <ex:fullName>Dave Beckett</ex:fullName>"
  , "      <ex:homePage rdf:resource=\"http://purl.org/net/dajobe/\"/>"
  , "    </ex:editor>"
  , "  </rdf:Description>"
  , "</rdf:RDF>"
  ]

xmlEg :: Text
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
-- pElement' :: Parser a -> Parser (Text, a)
-- pElement' = liftA2 (,) pName

-- pText' :: TL.Text -> Parser TL.Text
-- pText' t = do
--   let pTextFail = pFail ("Missing text node " <> show t)
--   do t' <- pText
--      if t == t' then pure t
--      else pTextFail
--    <|> pTextFail


-- parser combinators missing in Xmlbf
-- between :: Parser a -> Parser b -> Parser c -> Parser c
-- between open close thing  = open *> thing <* close
--
-- manyTill :: Parser a -> Parser end -> Parser [a]
-- manyTill thing z = many thing <* z

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


{- rdf-tests/rdf-xml/amp-in-url/test001.rdf

[ Text "\n\n"
, Element "rdf:RDF"
  [("xmlns:rdf","http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  ]
  [ Text "\n\n  "
  , Element "rdf:Description"
    [("rdf:about","http://example/q?abc=1&def=2")
    ]
    [ Text "\n    "
    , Element "rdf:value"
      []
      [Text "xxx"]
    , Text "\n  "
    ]
  , Text "\n\n"
  ]
, Text "\n"
]
-}

{- "rdf-tests/rdf-xml/rdfms-rdf-names-use/error-001.rdf"

[ Text "\n\n\n"
, Element "rdf:RDF"
  [("xmlns:rdf","http://www.w3.org/1999/02/22-rdf-syntax-ns#")]
  [ Text "\n  "
  , Element "rdf:RDF"
    []
    []
  , Text "\n"
  ]
, Text "\n"
]

-}



{- "rdf-tests/rdf-xml/rdfms-rdf-names-use/error-011.rdf"
Description is forbidden as a property element name.

[ Text "\n\n\n"
, Element "rdf:RDF"
  [("xmlns:rdf","http://www.w3.org/1999/02/22-rdf-syntax-ns#")]
  [ Text "\n  "
  , Element "rdf:Description"
    [("rdf:about","http://example.org/node1")]
    [ Text "\n    "
    , Element "rdf:Description"
      [("rdf:resource","http://example.org/node2")]
      []
    , Text "\n  "
    ]
 , Text "\n"
 ]
, Text "\n"
]

-}

{- "rdf-tests/rdf-xml/rdf-charmod-literals/test001.rdf"

[ Text "\n\n\n"
, Element "rdf:RDF"
  [("xmlns:eg","http://example.org/"),("xmlns:rdf","http://www.w3.org/1999/02/22-rdf-syntax-ns#")]
  [ Text "\n   \n\n   "
  , Element "rdf:Description"
    [("rdf:about","http://www.w3.org/TR/2002/WD-charmod-20020220")]
    [ Text "\n\n   \n      "
    , Element "eg:Creator"
      [("eg:named","D\252rst")]
      []
    , Text "\n      \n\n   "
    ]
  , Text "\n"
  ]
, Text "\n"
]

-}

{- rdf-tests/rdf-xml/rdf-charmod-uris/test001.rdf

[ Text "\r\n\r\n\r\n"
, Element "rdf:RDF"
  [("xmlns:eg","http://example.org/#"),("xmlns:rdf","http://www.w3.org/1999/02/22-rdf-syntax-ns#")]
  [ Text "\r\n\r\n  \r\n\r\n   "
  , Element "rdf:Description"
    [("rdf:about","http://example.org/#Andr\233")]
    [ Text "\r\n      "
    , Element "eg:owes"
      []
      [Text "2000"]
    , Text "\r\n   "
    ]
  , Text "\r\n"
  ]
, Text "\r\n"
]

-}

{- "rdf-tests/rdf-xml/rdf-charmod-uris/test002.rdf"

[ Text "\r\n\r\n"
, Element "rdf:RDF"
  [("xmlns:eg","http://example.org/#"),("xmlns:rdf","http://www.w3.org/1999/02/22-rdf-syntax-ns#")]
  [ Text "\r\n \r\n  \r\n\r\n   "
  , Element "rdf:Description"
    [("rdf:about","http://example.org/#Andr%C3%A9")]
    [ Text "\r\n      "
    , Element "eg:owes"
      []
      [Text "2000"]
    , Text "\r\n   "
    ]
  , Text "\r\n"
  ]
, Text " \r\n"
]

-}
