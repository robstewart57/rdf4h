{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}

-- |An parser for the RDF/XML format
-- <http://www.w3.org/TR/REC-rdf-syntax/>.

module Text.RDF.RDF4H.XmlParser
  ( XmlParser(..)
  , parseXmlDebug
  ) where

import           Data.RDF.Types hiding (empty, resolveQName)
import qualified Data.RDF.Types as RDF
import           Data.RDF.IRI
import           Data.RDF.Graph.TList
import           Text.RDF.RDF4H.ParserUtils hiding (Parser)
import           Text.RDF.RDF4H.XmlParser.Identifiers
import           Text.RDF.RDF4H.XmlParser.Xmlbf hiding (Node)
import qualified Text.RDF.RDF4H.XmlParser.Xeno as Xeno

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Strict
#if MIN_VERSION_base(4,9,0)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#else
#endif
#else
#endif
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as Map
import           Data.Maybe
#if MIN_VERSION_base(4,10,0)
import           Data.Either
#else
#endif
import           Data.Bifunctor
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
-- import           Xmlbf hiding (Node, State)
-- import qualified Xmlbf.Xeno as Xeno

instance RdfParser XmlParser where
  parseString (XmlParser bUrl dUrl) = parseXmlRDF bUrl dUrl
  parseFile   (XmlParser bUrl dUrl) = parseFile'  bUrl dUrl
  parseURL    (XmlParser bUrl dUrl) = parseURL'   bUrl dUrl

-- |Configuration for the XML parser
data XmlParser =
  XmlParser (Maybe BaseUrl) -- ^ The /default/ base URI to parse the document.
            (Maybe Text) -- ^ The /retrieval URI/ of the XML document.

parseFile' :: (Rdf a)
  => Maybe BaseUrl
  -> Maybe Text
  -> FilePath
  -> IO (Either ParseFailure (RDF a))
parseFile' bUrl dUrl fpath = parseXmlRDF bUrl dUrl <$> TIO.readFile fpath

parseURL' :: (Rdf a)
  => Maybe BaseUrl
  -- ^ The optional base URI of the document.
  -> Maybe Text
  -- ^ The document URI (i.e., the URI of the document itself); if Nothing, use location URI.
  -> String
  -- ^ The location URI from which to retrieve the XML document.
  -> IO (Either ParseFailure (RDF a))
  -- ^ The parse result, which is either a @ParseFailure@ or the RDF
  --   corresponding to the XML document.
parseURL' bUrl docUrl = parseFromURL (parseXmlRDF bUrl docUrl)

-- |The parser monad.
type Parser = ParserT (ExceptT String (State ParseState))

-- |Local state for the parser (dependant on the parent xml elements)
data ParseState = ParseState
  { stateBaseUri :: Maybe BaseUrl
  -- ^ The local base URI.
  , stateIdSet :: Set Text
  -- ^ The set of @rdf:ID@ found in the scope of the current base URI.
  , statePrefixMapping :: PrefixMappings
  -- ^ The namespace mapping.
  , stateLang :: Maybe Text
  -- ^ The local @xml:lang@
  , stateNodeAttrs :: HashMap Text Text
  -- ^ Current node RDF attributes.
  , stateSubject :: Maybe Subject
  -- ^ Current subject for triple construction.
  , stateCollectionIndex :: Int
  -- ^ Current collection index.
  , stateGenId :: Int
  } deriving(Show)

-- |Parse a xml Text to an RDF representation
parseXmlRDF :: (Rdf a)
  => Maybe BaseUrl
  -- ^ The base URI for the RDF if required
  -> Maybe Text
  -- ^ The request URI for the document to  if available
  -> Text
  -- ^ The contents to parse
  -> Either ParseFailure (RDF a)
  -- ^ The RDF representation of the triples or ParseFailure
parseXmlRDF bUrl dUrl = parseRdf . parseXml
  where
    bUrl' = BaseUrl <$> dUrl <|> bUrl
    parseXml = Xeno.fromRawXml . T.encodeUtf8
    parseRdf = first ParseFailure . join . second parseRdf'
    parseRdf' ns = join $ evalState (runExceptT (parseM rdfParser ns)) initState
    initState = ParseState bUrl' mempty mempty empty mempty empty 0 0

-- |A parser for debugging purposes.
parseXmlDebug
  :: FilePath
  -- ^ Path of the file to parse.
  -> IO (RDF TList)
parseXmlDebug f = fromRight RDF.empty <$> parseFile (XmlParser (Just . BaseUrl $ "http://base-url.com/") (Just "http://doc-url.com/")) f

-- |Document parser
rdfParser :: Rdf a => Parser (RDF a)
rdfParser = do
  bUri <- currentBaseUri
  triples <- (pRdf <* pWs) <|> pNodeElementList
  pEndOfInput
  mkRdf triples bUri <$> currentPrefixMappings

-- |Parser for @rdf:RDF@, if present.
-- See: https://www.w3.org/TR/rdf-syntax-grammar/#RDF
pRdf :: Parser Triples
pRdf = pAnyElement $ do
  attrs <- pRDFAttrs
  uri <- pName >>= pQName
  guard (uri == rdfTag)
  unless (null attrs) $ throwError "rdf:RDF: The set of attributes should be empty."
  pNodeElementList

-- |Parser for XML QName: resolve the namespace with the mapping in context.
--
--  Throws an error if the namespace is not defined.
pQName :: Text -> Parser Text
pQName qn = do
  pm <- currentPrefixMappings
  let qn' = resolveQName pm qn >>= validateIRI
  either throwError pure qn'

-- |Process the attributes of an XML element.
--
--  To be called __once__ per XML element.
pRDFAttrs :: Parser (HashMap Text Text)
pRDFAttrs = do
  -- Language (xml:lang)
  liftA2 (<|>) pLang currentLang >>= setLang
  -- Base URI (xml:base)
  liftA2 (<|>) pBase currentBaseUri >>= setBaseUri
  bUri <- currentBaseUri
  -- Process the rest of the attributes
  attrs <- pAttrs
  -- Get the namespace definitions (xmlns:)
  pm <- updatePrefixMappings (PrefixMappings $ HM.foldlWithKey' mkNameSpace mempty attrs)
  -- Filter and resolve RDF attributes
  let as = HM.foldlWithKey' (mkRdfAttribute pm bUri) mempty attrs
  setNodeAttrs as
  pure as
  where
    -- |Check if an XML attribute is a namespace definition
    --  and if so add it to the mapping.
    mkNameSpace
      :: Map.Map Text Text
      -- ^ Current namespace mapping
      -> Text
      -- ^ XML attribute to process
      -> Text
      -- ^ Value of the attribute
      -> Map.Map Text Text
    mkNameSpace ns qn iri =
      let qn' = parseQName qn
          ns' = f <$> qn' <*> validateIRI iri
          f (Nothing     , "xmlns") iri' = Map.insert mempty iri' ns
          f (Just "xmlns", prefix ) iri' = Map.insert prefix iri' ns
          f _                       _    = ns
      in either (const ns) id ns'
    -- |Check if an XML attribute is an RDF attribute
    --  and if so resolve its URI and keep it.
    mkRdfAttribute
      :: PrefixMappings
      -- ^ Namespace mapping
      -> Maybe BaseUrl
      -- ^ Base URI
      -> HM.HashMap Text Text
      -- ^ Current set of RDF attributes
      -> Text
      -- ^ XML attribute to process
      -> Text
      -- ^ Value of the attribute
      -> HM.HashMap Text Text
    mkRdfAttribute pm bUri as qn v =
      let as' = parseQName qn >>= f
          -- [NOTE] Ignore XML reserved names
          f (Nothing, n)
            | T.isPrefixOf "xml" n = Right as
            | otherwise            = case bUri of
                Nothing -> Right as -- [FIXME] manage missing base URI
                Just (BaseUrl bUri') -> (\a -> HM.insert a v as) <$> resolveIRI bUri' n
          f qn'@(Just prefix, _)
            | T.isPrefixOf "xml" prefix = Right as
            | otherwise = (\a -> HM.insert a v as) <$> resolveQName' pm qn'
      in either (const as) id as'

-- |Return the value of the requested RDF attribute using its URI.
--
--  Fails if the attribute is not defined.
pRDFAttr :: Text -> Parser Text
pRDFAttr a = do
  as <- currentNodeAttrs
  maybe
    (fail . mconcat $ ["Attribute \"", T.unpack a, "\" not found."])
    pure
    (HM.lookup a as)

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#nodeElementList
pNodeElementList :: Parser Triples
pNodeElementList = pWs *> (mconcat <$> some (keepState pNodeElement <* pWs))

-- |White spaces parser
--  See: https://www.w3.org/TR/rdf-syntax-grammar/#ws
pWs :: Parser ()
pWs = maybe True (T.all ws . TL.toStrict) <$> optional pText >>= guard
  where
    -- See: https://www.w3.org/TR/2000/REC-xml-20001006#NT-S
    ws c = c == '\x20' || c == '\x09' || c == '\x0d' || c == '\x0a'

-- https://www.w3.org/TR/rdf-syntax-grammar/#nodeElement
pNodeElement :: Parser Triples
pNodeElement = pAnyElement $ do
  -- Process attributes
  void pRDFAttrs
  -- Process URI, subject and @rdf:type@.
  (s, mt) <- pSubject
  ts1 <- pPropertyAttrs s
  -- Process propertyEltList
  ts2 <- keepState pPropertyEltList
  setSubject (Just s)
  let ts = ts1 <> ts2
  pure $ maybe ts (:ts) mt

-- |Process the following parts of a @nodeElement@: URI, subject and @rdf:type@.
-- See: https://www.w3.org/TR/rdf-syntax-grammar/#nodeElement
pSubject :: Parser (Node, Maybe Triple)
pSubject = do
  -- Create the subject
  -- [TODO] check the attributes that only one of the following may work
  s <- pUnodeId <|> pBnode <|> pUnode <|> pBnodeGen
  setSubject (Just s)
  -- Resolve URI
  uri <- pName >>= pQName
  -- Check that the URI is allowed
  unless (checkNodeUri uri) (throwError $ "URI not allowed: " <> T.unpack uri)
  -- Optional rdf:type triple
  mtype <- optional (pType1 s uri)
  pure (s, mtype)
  where
    checkNodeUri uri = isNotCoreSyntaxTerm uri && uri /= rdfLi && isNotOldTerm uri
    pUnodeId = (pIdAttr >>= mkUNodeID) <* removeNodeAttr rdfID
    pBnode = (BNode <$> pNodeIdAttr) <* removeNodeAttr rdfNodeID
    pUnode = (unode <$> pAboutAttr) <* removeNodeAttr rdfAbout
    -- Default subject: a new blank node
    pBnodeGen = newBNode
    pType1 n uri =
      if uri /= rdfDescription
        then pure $ Triple n rdfTypeNode (unode uri)
        else empty

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#propertyAttr
pPropertyAttrs :: Node -> Parser Triples
pPropertyAttrs s = do
  attrs <- currentNodeAttrs
  HM.elems <$> HM.traverseWithKey f attrs
  where
    f attr value
      | not (isPropertyAttrURI attr) = throwError $ "URI not allowed for attribute: " <> T.unpack attr
      | attr == rdfType = pure $ Triple s rdfTypeNode (unode value)
      | otherwise = do
          lang <- currentLang
          pure $ let mkLiteral = maybe plainL (flip plainLL) lang
                 in Triple s (unode attr) (lnode (mkLiteral value))

pLang :: Parser (Maybe Text)
pLang = optional (pAttr "xml:lang")

-- [TODO] resolve base uri in context
pBase :: Parser (Maybe BaseUrl)
pBase = optional $ do
  uri <- pAttr "xml:base"
  -- Parse and remove fragment
  BaseUrl <$> either
    throwError
    (pure . serializeIRI . removeIRIFragment)
    (parseIRI uri)

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#propertyEltList
pPropertyEltList :: Parser Triples
pPropertyEltList =  pWs
                 *> resetCollectionIndex
                 *> fmap mconcat (many (pPropertyElt <* pWs))

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#propertyElt
pPropertyElt :: Parser Triples
pPropertyElt = pAnyElement $ do
  -- Process attributes
  void pRDFAttrs
  -- Process the predicate from the URI
  uri <- pName >>= pQName >>= listExpansion
  unless (isPropertyAttrURI uri) (throwError $ "URI not allowed for propertyElt: " <> T.unpack uri)
  let p = unode uri
  -- Process 'propertyElt'
  pParseTypeLiteralPropertyElt p
    <|> pParseTypeResourcePropertyElt p
    <|> pParseTypeCollectionPropertyElt p
    <|> pParseTypeOtherPropertyElt p
    <|> pResourcePropertyElt p
    <|> pLiteralPropertyElt p
    <|> pEmptyPropertyElt p
  where
    listExpansion u
      | u == rdfLi = nextCollectionIndex
      | otherwise  = pure u

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#resourcePropertyElt
pResourcePropertyElt :: Node -> Parser Triples
pResourcePropertyElt p = do
  pWs
  -- [NOTE] We need to restore part of the state after exploring the element' children.
  (ts1, o) <- keepState $ liftA2 (,) pNodeElement currentSubject
  pWs
  mi <- optional pIdAttr <* removeNodeAttr rdfID
  -- No other attribute is allowed.
  checkAllowedAttributes []
  -- Generated triple
  s <- currentSubject
  let mt = flip Triple p <$> s <*> o
  -- Reify the triple
  ts2 <- maybe (pure mempty) (uncurry reifyTriple) (liftA2 (,) mi mt)
  pure $ maybe (ts1 <> ts2) (:(ts1 <> ts2)) mt

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#literalPropertyElt
pLiteralPropertyElt :: Node -> Parser Triples
pLiteralPropertyElt p = do
  l <- pText
  -- No children
  pChildren >>= guard . null
  mi <- optional pIdAttr <* removeNodeAttr rdfID
  checkAllowedAttributes [rdfDatatype]
  dt <- optional pDatatypeAttr
  s <- currentSubject
  lang <- currentLang
  -- Generated triple
  let l' = TL.toStrict l
      o = lnode . fromMaybe (plainL l') $ (typedL l' <$> dt) <|> (plainLL l' <$> lang)
      mt = (\s' -> Triple s' p o) <$> s
  -- Reify the triple
  ts <- maybe (pure mempty) (uncurry reifyTriple) (liftA2 (,) mi mt)
  pure $ maybe ts (:ts) mt

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#parseTypeLiteralPropertyElt
pParseTypeLiteralPropertyElt :: Node -> Parser Triples
pParseTypeLiteralPropertyElt p = do
  pt <- pRDFAttr rdfParseType
  guard (pt == "Literal")
  mi <- optional pIdAttr <* removeNodeAttr rdfID
  checkAllowedAttributes [rdfParseType]
  l <- pXMLLiteral
  -- Generated triple
  s <- currentSubject
  let o = lnode (typedL l rdfXmlLiteral)
      mt = (\s' -> Triple s' p o) <$> s
  -- Reify the triple
  ts <- maybe (pure mempty) (uncurry reifyTriple) (liftA2 (,) mi mt)
  pure $ maybe ts (:ts) mt

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#parseTypeResourcePropertyElt
pParseTypeResourcePropertyElt :: Node -> Parser Triples
pParseTypeResourcePropertyElt p = do
  pt <- pRDFAttr rdfParseType
  guard (pt == "Resource")
  mi <- optional pIdAttr <* removeNodeAttr rdfID
  checkAllowedAttributes [rdfParseType]
  -- Generated triple
  s <- currentSubject
  o <- newBNode
  let mt = (\s' -> Triple s' p o) <$> s
  -- Reify the triple
  ts1 <- maybe (pure mempty) (uncurry reifyTriple) (liftA2 (,) mi mt)
  setSubject (Just o)
  -- Explore children
  ts2 <- keepCollectionIndex pPropertyEltList
  --setSubject s
  pure $ maybe (ts1 <> ts2) ((<> ts2) . (:ts1)) mt

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#parseTypeCollectionPropertyElt
pParseTypeCollectionPropertyElt :: Node -> Parser Triples
pParseTypeCollectionPropertyElt p = do
  pt <- pRDFAttr rdfParseType
  guard (pt == "Collection")
  mi <- optional pIdAttr <* removeNodeAttr rdfID
  checkAllowedAttributes [rdfParseType]
  s <- currentSubject
  case s of
    Nothing -> pure mempty
    Just s' -> do
      r <- optional pNodeElement
      case r of
        Nothing ->
          -- Empty collection
          let t = Triple s' p rdfNilNode
          in ([t] <>) <$> maybe (pure mempty) (`reifyTriple` t) mi
        Just ts1 -> do
          -- Non empty collection
          s'' <- currentSubject
          n <- newBNode
          -- Triples corresping to the first item
          let t = Triple s' p n
              ts2 = maybe mempty (\s''' -> [t, Triple n rdfFirstNode s''']) s''
          -- Process next item
          ts3 <- go n
          -- Reify triple
          ts4 <- maybe (pure mempty) (`reifyTriple` t) mi
          pure $ mconcat [ts1, ts2, ts3, ts4]
  where
    go s = do
      -- Generate the triples of the current item.
      r <- optional pNodeElement
      case r of
        -- End of the collection
        Nothing -> pure [Triple s rdfRestNode rdfNilNode]
        -- Add the item to the collection and process the next item
        Just ts1 -> do
          s' <- currentSubject
          n <- newBNode
          let ts2 = maybe mempty (\s'' -> [Triple s rdfRestNode n, Triple n rdfFirstNode s'']) s'
          -- Next item
          ts3 <- go n
          pure $ mconcat [ts1, ts2, ts3]

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#parseTypeOtherPropertyElt
pParseTypeOtherPropertyElt :: Node -> Parser Triples
pParseTypeOtherPropertyElt _p = do
  pt <- pRDFAttr rdfParseType
  guard (pt /= "Resource" && pt /= "Literal" && pt /= "Collection")
  checkAllowedAttributes [rdfParseType]
  _mi <- optional pIdAttr <* removeNodeAttr rdfID
  -- [FIXME] Implement 'parseTypeOtherPropertyElt'
  throwError "Not implemented: rdf:parseType = other"

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#emptyPropertyElt
pEmptyPropertyElt :: Node -> Parser Triples
pEmptyPropertyElt p = do
  s <- currentSubject
  case s of
    Nothing -> pure mempty
    Just s' -> do
      mi <- optional pIdAttr <* removeNodeAttr rdfID
      o <- pResourceAttr' <|> pNodeIdAttr' <|> newBNode
      let t = Triple s' p o
      -- Reify triple
      ts1 <- maybe (pure mempty) (`reifyTriple` t) mi
      ts2 <- pPropertyAttrs o
      pure (t:ts1 <> ts2)
  where
    pResourceAttr' = unode <$> pResourceAttr <* removeNodeAttr rdfResource
    pNodeIdAttr' = BNode <$> pNodeIdAttr <* removeNodeAttr rdfNodeID

checkAllowedAttributes :: HashSet Text -> Parser ()
checkAllowedAttributes as = do
  attrs <- currentNodeAttrs
  let diffAttrs = HS.difference (HM.keysSet attrs) as
  unless (null diffAttrs) (throwError $ "Attributes not allowed: " <> show diffAttrs)
-- See: https://www.w3.org/TR/rdf11-concepts/#dfn-rdf-xmlliteral,
--      https://www.w3.org/TR/rdf-syntax-grammar/#literal
pXMLLiteral :: Parser Text
pXMLLiteral =
  T.decodeUtf8 . BL.toStrict . BB.toLazyByteString . encode <$> pChildren

pIdAttr :: Parser Text
pIdAttr = do
  i <- pRDFAttr rdfID
  i' <- either throwError pure (checkRdfId i)
  -- Check the uniqueness of the ID in the context of the current base URI.
  checkIdIsUnique i'
  pure i'

checkIdIsUnique :: Text -> Parser ()
checkIdIsUnique i = do
  notUnique <- S.member i <$> currentIdSet
  when notUnique (throwError $ "rdf:ID already used in this context: " <> T.unpack i)
  updateIdSet i

pNodeIdAttr :: Parser Text
pNodeIdAttr = do
  i <- pRDFAttr rdfNodeID
  either throwError pure (checkRdfId i)

pAboutAttr :: Parser Text
pAboutAttr = pRDFAttr rdfAbout >>= checkIRI "rdf:about"

pResourceAttr :: Parser Text
pResourceAttr = pRDFAttr rdfResource >>= checkIRI "rdf:resource"

pDatatypeAttr :: Parser Text
pDatatypeAttr = pRDFAttr rdfDatatype >>= checkIRI "rdf:datatype"

reifyTriple :: Text -> Triple -> Parser Triples
reifyTriple i (Triple s p' o) = do
  n <- mkUNodeID i
  pure [ Triple n rdfTypeNode rdfStatementNode
       , Triple n rdfSubjectNode s
       , Triple n rdfPredicateNode p'
       , Triple n rdfObjectNode o ]

--------------------------------------------------------------------------------
-- URI checks

checkIRI :: String -> Text -> Parser Text
checkIRI msg iri = do
  bUri <- maybe mempty unBaseUrl <$> currentBaseUri
  case uriValidate iri of
    Nothing   -> throwError $ mconcat ["Malformed IRI for \"", msg, "\": ", T.unpack iri]
    Just iri' -> either throwError pure (resolveIRI bUri iri')

-- https://www.w3.org/TR/rdf-syntax-grammar/#propertyAttributeURIs
isPropertyAttrURI :: Text -> Bool
isPropertyAttrURI uri
  =  isNotCoreSyntaxTerm uri
  && uri /= rdfDescription
  && uri /= rdfLi
  && isNotOldTerm uri

-- https://www.w3.org/TR/rdf-syntax-grammar/#coreSyntaxTerms
isNotCoreSyntaxTerm :: Text -> Bool
isNotCoreSyntaxTerm uri
  =  uri /= rdfTag && uri /= rdfID && uri /= rdfAbout
  && uri /= rdfParseType && uri /= rdfResource
  && uri /= rdfNodeID && uri /= rdfDatatype

-- https://www.w3.org/TR/rdf-syntax-grammar/#oldTerms
isNotOldTerm :: Text -> Bool
isNotOldTerm uri =  uri /= rdfAboutEach
                 && uri /= rdfAboutEachPrefix
                 && uri /= rdfBagID

--------------------------------------------------------------------------------
-- Parser's state utils

-- |Create a new unique blank node
newBNode :: Parser Node
newBNode = do
  modify $ \st -> st { stateGenId = stateGenId st + 1 }
  BNodeGen . stateGenId <$> get

-- |Process a parser, restoring the state except for stateGenId and stateIdSet
keepState :: Parser a -> Parser a
keepState p = do
  st <- get
  let bUri = stateBaseUri st
      is = stateIdSet st
  p <* do
    st' <- get
    let i = stateGenId st'
        bUri' = stateBaseUri st'
        is' = stateIdSet st'
    -- Update the set of ID if necessary
    if bUri /= bUri'
      then put (st { stateGenId = i })
      else put (st { stateGenId = i, stateIdSet = is <> is' })

currentIdSet :: Parser (Set Text)
currentIdSet = stateIdSet <$> get

updateIdSet :: Text -> Parser ()
updateIdSet i = do
  is <- currentIdSet
  modify (\st -> st { stateIdSet = S.insert i is })

currentNodeAttrs :: Parser (HashMap Text Text)
currentNodeAttrs = stateNodeAttrs <$> get

setNodeAttrs :: HashMap Text Text -> Parser ()
setNodeAttrs as = modify (\st -> st { stateNodeAttrs = as })

removeNodeAttr :: Text -> Parser ()
removeNodeAttr a = HM.delete a <$> currentNodeAttrs >>= setNodeAttrs

currentPrefixMappings :: Parser PrefixMappings
currentPrefixMappings = statePrefixMapping <$> get

updatePrefixMappings :: PrefixMappings -> Parser PrefixMappings
updatePrefixMappings pm = do
  pm' <- (<> pm) <$> currentPrefixMappings
  modify (\st -> st { statePrefixMapping = pm' })
  pure pm'

currentCollectionIndex :: Parser Int
currentCollectionIndex = stateCollectionIndex <$> get

setCollectionIndex :: Int -> Parser ()
setCollectionIndex i = modify (\st -> st { stateCollectionIndex = i })

keepCollectionIndex :: Parser a -> Parser a
keepCollectionIndex p = do
  i <- currentCollectionIndex
  p <* setCollectionIndex i

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#section-List-Expand
nextCollectionIndex :: Parser Text
nextCollectionIndex = do
  modify $ \st -> st { stateCollectionIndex = stateCollectionIndex st + 1 }
  (rdfListIndex <>) . T.pack . show . stateCollectionIndex <$> get

resetCollectionIndex :: Parser ()
resetCollectionIndex = modify $ \st -> st { stateCollectionIndex = 0 }

currentBaseUri :: Parser (Maybe BaseUrl)
currentBaseUri = stateBaseUri <$> get

setBaseUri :: (Maybe BaseUrl) -> Parser ()
setBaseUri u = modify (\st -> st { stateBaseUri = u })

mkUNodeID :: Text -> Parser Node
mkUNodeID t = mkUnode <$> currentBaseUri
  where
    mkUnode = unode . \case
      Nothing          -> t
      Just (BaseUrl u) -> mconcat [u, "#", t]

currentSubject :: Parser (Maybe Subject)
currentSubject = stateSubject <$> get

setSubject :: (Maybe Subject) -> Parser ()
setSubject s = modify (\st -> st { stateSubject = s })

currentLang :: Parser (Maybe Text)
currentLang = stateLang <$> get

setLang :: (Maybe Text) -> Parser ()
setLang lang = modify (\st -> st { stateLang = lang })
