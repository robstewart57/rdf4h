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

import Debug.Trace
import qualified Control.Applicative as Applicative
import Control.Exception
import Control.Monad
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.Maybe
import Data.RDF.IRI
import Data.RDF.Types
import Data.RDF.Graph.TList
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import Xmlbf hiding (Node)
import qualified Xmlbf (Node)
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

-- -- |Global state for the parser
-- data GParseState = GParseState
--   { stateGenId :: Int
--   } deriving (Show)

-- |Local state for the parser (dependant on the parent xml elements)
data ParseState = ParseState { stateBaseUrl :: Maybe BaseUrl
                             , stateLang :: Maybe Text
                             , stateSubject :: Subject
                             , stateGenId :: Int
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
      case runParser (rdfParser bUrl dUrl) nodes of
        Left rdfParseError -> Left (ParseFailure rdfParseError)
        Right rdf -> Right rdf
-- TODO: use bUrl and dUrl

rdfParser :: Rdf a => Maybe BaseUrl -> Maybe Text -> Parser (RDF a)
rdfParser bUrl dUrl = do
  let initState = ParseState bUrl Nothing undefined 0
  rdf <- rdfDescription initState
  newlines
  -- tree <- showTree
  -- error (show tree)
  void pEndOfInput
  return rdf

-- Text "\n"
-- TODO: check that all that follows from \n is zero or more ' ' characters.
newline :: Parser ()
newline = do
  t <- pText
  if not $ anyUsefulChars (TL.toStrict t)
  then pure ()
  else pFail "not a newline text node"
  where
    anyUsefulChars t =
      if T.length t == 0
      then False
      else
       let c = T.head t
       in if (c /= '\n' && c /= '\r' && c /= ' ')
          then True
          else anyUsefulChars (T.tail t)

newlines :: Parser ()
newlines = void (many newline) 

pNodeNot :: Text -> Parser ()
pNodeNot t = do
  n <- pName
  if (n /= t)
  then pure ()
  else pFail ("forbidden element name: " ++ show t)

{-
[ ("xmlns:si","https://www.w3schools.com/rdf/")
, ("xmlns:rdf","http://www.w3.org/1999/02/22-rdf-syntax-ns#")
]
-}
prefixes :: Parser [(Text,Text)]
prefixes = do
  xs <- HashMap.toList <$> pAttrs
  pure (map (\(k,v) -> (fromJust (T.stripPrefix "xmlns:" k),v)) xs)

oneAttr :: Parser (Text,Text)
oneAttr = do
  xs <- pAttrs
  case length (HashMap.toList xs) of
    1 -> pure $ head (HashMap.toList xs)
    _ -> pFail "not one attr"

rdfTriplesP :: ParseState -> Parser (Triples,ParseState)
rdfTriplesP st = do
  newlines
  pElement "rdf:Description" $ do
    newlines
    ((subj, reifiedTriples), st') <- subjP st
    (ts) <- concat <$> many (predObjP st')
    newlines
    -- tree <- showTree
    -- error (show tree)
    void pEndOfInput
    pure (ts ++ reifiedTriples,st')
    -- pure $ ((map (\(p, o) -> triple subj p o) predObjs ++ reifiedTriples), st')

{- NOTE:
  remember to use `showTree` in the fork of xmlbf when pEndOfInput needs
  debugging.
-}

subjP :: ParseState -> Parser ((Node,Triples),ParseState)
subjP st = do
  -- void (pNodeNot "rdf:RDF") -- rdfms-rdf-names-use-error-001
  (do
      s <- unode <$> pAttr "rdf:about"
      pure ((s,[]),st { stateSubject = s } )
        <|> do
        -- theId <- pAttr "rdf:ID"
        let theBnode = BNodeGen (stateGenId st)
            st' = st { stateGenId = stateGenId st + 1}
        pure ((theBnode,[]),st'))

-- predObjP :: Parser ((Node,Node))
predObjP :: ParseState -> Parser Triples
predObjP st = do
  void newlines
  (do pAnyElement $ do
        void (pNodeNot "rdf:Description")  -- rdfms-rdf-names-use-error-011
        p <- unode <$> pName
        (ts) <-
            (do
                -- typed literal
                theType <- pAttr "rdf:datatype"
                theText <- pText
                pure [triple (stateSubject st) p ((lnode (typedL (TL.toStrict theText) theType)))])
            <|>
          -- blank node
          (do (p1,o1) <- oneAttr
              -- TODO: increment stateGenId
              let bnode = BNodeGen (stateGenId st)
                  t1 = triple (stateSubject st) p bnode
                  a = case stateBaseUrl st of
                        Nothing -> T.pack ""
                        Just (BaseUrl uri) -> uri
                  Right txt = resolveIRI a p1
                  p2 = unode txt
                  -- TODO: typed and lang literals
                  t2 = triple bnode p2 (lnode (plainL o1))
              pure [t1,t2])
            <|>
            (do
                -- plain literal
                theText <- pText
                newlines
                pure [triple
                      (stateSubject st)
                      p
                      (lnode (plainL (TL.toStrict theText)))])
        newlines
        pure ts
        <|>
        pFail "unable to parse predicate/object pair"
    )
  -- TODO: reify triple

-- TODO: unodes, and all different kinds of plain text nodes
-- objP :: Parser (Node)
-- objP {- st -} = do
--   -- unode
--   -- xs <- head <$> prefixes
--   -- error (show xs)
--   -- TODO for:
--   --  Element "eg:Creator"
--   --  [("eg:named","D\252rst")]
--   --  []
--   pure (unode "http://www.example.com")
--   <|> do
--   -- typed literal
--   theType <- pAttr "rdf:datatype"
--   theText <- pText
--   pure ((lnode (typedL (TL.toStrict theText) theType)))
--   <|> do
--   -- plain literal
--   theText <- pText
--   pure (lnode (plainL (TL.toStrict theText)))



rdfDescription' :: ParseState -> Parser (PrefixMappings,Maybe BaseUrl,Triples)
rdfDescription' st = do
  newlines
  pfixes <- prefixes
  (_,(triples,st')) <- pElement' (rdfTriplesP st)
  newlines
  pure (PrefixMappings (Map.fromList pfixes), Nothing, triples)

rdfDescription :: Rdf a => ParseState -> Parser (RDF a)
rdfDescription st = do
  (pfixes,bUrl,triples) <- pElement "rdf:RDF" (rdfDescription' st)
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
