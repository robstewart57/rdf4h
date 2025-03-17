{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An parser for the RDF/XML format
--  <http://www.w3.org/TR/REC-rdf-syntax/>.
module Text.RDF.RDF4H.XmlParser
  ( XmlParser (..),
    parseXmlDebug,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.RDF.Graph.TList
import Data.RDF.IRI
import Data.RDF.Types hiding (empty, resolveQName)
import qualified Data.RDF.Types as RDF
import Text.RDF.RDF4H.ParserUtils hiding (Parser)
import Text.RDF.RDF4H.XmlParser.Identifiers
#if MIN_VERSION_base(4,9,0)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#else
#endif
#else
#endif
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
#if MIN_VERSION_base(4,10,0)
import           Data.Either
#else
#endif
import Data.Bifunctor
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Xmlbf hiding (Node)
import qualified Xmlbf.Xeno as Xeno

instance RdfParser XmlParser where
  parseString (XmlParser bUrl dUrl) = parseXmlRDF bUrl dUrl
  parseFile (XmlParser bUrl dUrl) = parseFile' bUrl dUrl
  parseURL (XmlParser bUrl dUrl) = parseURL' bUrl dUrl

-- | Configuration for the XML parser
data XmlParser
  = XmlParser
      -- | The /default/ base URI to parse the document.
      (Maybe BaseUrl)
      -- | The /retrieval URI/ of the XML document.
      (Maybe Text)

parseFile' ::
  (Rdf a) =>
  Maybe BaseUrl ->
  Maybe Text ->
  FilePath ->
  IO (Either ParseFailure (RDF a))
parseFile' bUrl dUrl fpath = parseXmlRDF bUrl dUrl <$> TIO.readFile fpath

parseURL' ::
  (Rdf a) =>
  -- | The optional base URI of the document.
  Maybe BaseUrl ->
  -- | The document URI (i.e., the URI of the document itself); if Nothing, use location URI.
  Maybe Text ->
  -- | The location URI from which to retrieve the XML document.
  String ->
  -- | The parse result, which is either a @ParseFailure@ or the RDF
  --   corresponding to the XML document.
  IO (Either ParseFailure (RDF a))
parseURL' bUrl docUrl = parseFromURL (parseXmlRDF bUrl docUrl)

-- | The parser monad.
type RdfXmlParser = ParserT (ExceptT String (State ParseState))

-- | Local state for the parser (dependant on the parent xml elements)
data ParseState = ParseState
  { -- | The local base URI.
    stateBaseUri :: Maybe BaseUrl,
    -- | The set of @rdf:ID@ found in the scope of the current base URI.
    stateIdSet :: Set Text,
    -- | The namespace mapping.
    statePrefixMapping :: PrefixMappings,
    -- | The local @xml:lang@
    stateLang :: Maybe Text,
    -- | Current node RDF attributes.
    stateNodeAttrs :: HashMap Text Text,
    -- | Current subject for triple construction.
    stateSubject :: Maybe Subject,
    -- | Current collection index.
    stateCollectionIndex :: Int,
    stateGenId :: Int
  }
  deriving (Show)

-- | Parse a xml Text to an RDF representation
parseXmlRDF ::
  (Rdf a) =>
  -- | The base URI for the RDF if required
  Maybe BaseUrl ->
  -- | The request URI for the document to  if available
  Maybe Text ->
  -- | The contents to parse
  Text ->
  -- | The RDF representation of the triples or ParseFailure
  Either ParseFailure (RDF a)
parseXmlRDF bUrl dUrl = parseRdf . parseXml
  where
    bUrl' = BaseUrl <$> dUrl <|> bUrl
    parseXml = Xeno.fromRawXml . T.encodeUtf8
    parseRdf = first ParseFailure . join . second parseRdf'
    parseRdf' ns = join $ evalState (runExceptT (parseM rdfParser ns)) initState
    initState = ParseState bUrl' mempty mempty empty mempty empty 0 0

-- | A parser for debugging purposes.
parseXmlDebug ::
  -- | Path of the file to parse.
  FilePath ->
  IO (RDF TList)
parseXmlDebug f = fromRight RDF.empty <$> parseFile (XmlParser (Just . BaseUrl $ "http://base-url.com/") (Just "http://doc-url.com/")) f

-- | Document parser
rdfParser :: (Rdf a) => RdfXmlParser (RDF a)
rdfParser = do
  bUri <- currentBaseUri
  triples <- (pRdf <* pWs) <|> pNodeElementList
  pEndOfInput
  mkRdf triples bUri <$> currentPrefixMappings

-- | RdfXmlParser for @rdf:RDF@, if present.
--  See: https://www.w3.org/TR/rdf-syntax-grammar/#RDF
pRdf :: RdfXmlParser Triples
pRdf = pAnyElement $ do
  attrs <- pRDFAttrs
  uri <- pName >>= pQName
  guard (uri == rdfTag)
  unless (null attrs) $ (throwError "rdf:RDF: The set of attributes should be empty.")
  pNodeElementList

-- | RdfXmlParser for XML QName: resolve the namespace with the mapping in context.
--
--   Throws an error if the namespace is not defined.
pQName :: Text -> RdfXmlParser Text
pQName qn = do
  pm <- currentPrefixMappings
  let qn' = resolveQName pm qn >>= validateIRI
  either throwError pure qn'

-- | Process the attributes of an XML element.
--
--   To be called __once__ per XML element.
pRDFAttrs :: RdfXmlParser (HashMap Text Text)
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
    -- \|Check if an XML attribute is a namespace definition
    --  and if so add it to the mapping.
    mkNameSpace ::
      Map.Map Text Text ->
      -- \^ Current namespace mapping
      Text ->
      -- \^ XML attribute to process
      Text ->
      -- \^ Value of the attribute
      Map.Map Text Text
    mkNameSpace ns qn iri =
      let qn' = parseQName qn
          ns' = f <$> qn' <*> validateIRI iri
          f (Nothing, "xmlns") iri' = Map.insert mempty iri' ns
          f (Just "xmlns", prefix) iri' = Map.insert prefix iri' ns
          f _ _ = ns
       in either (const ns) id ns'
    -- \|Check if an XML attribute is an RDF attribute
    --  and if so resolve its URI and keep it.
    mkRdfAttribute ::
      PrefixMappings ->
      -- \^ Namespace mapping
      Maybe BaseUrl ->
      -- \^ Base URI
      HM.HashMap Text Text ->
      -- \^ Current set of RDF attributes
      Text ->
      -- \^ XML attribute to process
      Text ->
      -- \^ Value of the attribute
      HM.HashMap Text Text
    mkRdfAttribute pm bUri as qn v =
      let as' = parseQName qn >>= f
          -- [NOTE] Ignore XML reserved names
          f (Nothing, n)
            | T.isPrefixOf "xml" n = Right as
            | otherwise = case bUri of
                Nothing -> Right as -- [FIXME] manage missing base URI
                Just (BaseUrl bUri') -> (\a -> HM.insert a v as) <$> resolveIRI bUri' n
          f qn'@(Just prefix, _)
            | T.isPrefixOf "xml" prefix = Right as
            | otherwise = (\a -> HM.insert a v as) <$> resolveQName' pm qn'
       in either (const as) id as'

-- | Return the value of the requested RDF attribute using its URI.
--
--   Fails if the attribute is not defined.
pRDFAttr :: Text -> RdfXmlParser Text
pRDFAttr a = do
  as <- currentNodeAttrs
  maybe
    (fail . mconcat $ ["Attribute \"", T.unpack a, "\" not found."])
    pure
    (HM.lookup a as)

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#nodeElementList
pNodeElementList :: RdfXmlParser Triples
pNodeElementList = pWs *> (mconcat <$> some (keepState pNodeElement <* pWs))

-- | White spaces parser
--   See: https://www.w3.org/TR/rdf-syntax-grammar/#ws
pWs :: RdfXmlParser ()
pWs = maybe True (T.all ws) <$> optional pText >>= guard
  where
    -- See: https://www.w3.org/TR/2000/REC-xml-20001006#NT-S
    ws c = c == '\x20' || c == '\x09' || c == '\x0d' || c == '\x0a'

-- https://www.w3.org/TR/rdf-syntax-grammar/#nodeElement
pNodeElement :: RdfXmlParser Triples
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
  pure $ maybe ts (: ts) mt

-- | Process the following parts of a @nodeElement@: URI, subject and @rdf:type@.
--  See: https://www.w3.org/TR/rdf-syntax-grammar/#nodeElement
pSubject :: RdfXmlParser (Node, Maybe Triple)
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
pPropertyAttrs :: Node -> RdfXmlParser Triples
pPropertyAttrs s = do
  attrs <- currentNodeAttrs
  HM.elems <$> HM.traverseWithKey f attrs
  where
    f attr value
      | not (isPropertyAttrURI attr) = throwError $ "URI not allowed for attribute: " <> T.unpack attr
      | attr == rdfType = pure $ Triple s rdfTypeNode (unode value)
      | otherwise = do
          lang <- currentLang
          pure $
            let mkLiteral = maybe plainL (flip plainLL) lang
             in Triple s (unode attr) (lnode (mkLiteral value))

pLang :: RdfXmlParser (Maybe Text)
pLang = optional (pAttr "xml:lang")

-- [TODO] resolve base uri in context
pBase :: RdfXmlParser (Maybe BaseUrl)
pBase = optional $ do
  uri <- pAttr "xml:base"
  -- Parse and remove fragment
  BaseUrl
    <$> either
      throwError
      (pure . serializeIRI . removeIRIFragment)
      (parseIRI uri)

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#propertyEltList
pPropertyEltList :: RdfXmlParser Triples
pPropertyEltList =
  pWs
    *> resetCollectionIndex
    *> fmap mconcat (many (pPropertyElt <* pWs))

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#propertyElt
pPropertyElt :: RdfXmlParser Triples
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
      | otherwise = pure u

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#resourcePropertyElt
pResourcePropertyElt :: Node -> RdfXmlParser Triples
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
  pure $ maybe (ts1 <> ts2) (: (ts1 <> ts2)) mt

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#literalPropertyElt
pLiteralPropertyElt :: Node -> RdfXmlParser Triples
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
  let o = lnode . fromMaybe (plainL l) $ (typedL l <$> dt) <|> (plainLL l <$> lang)
      mt = (\s' -> Triple s' p o) <$> s
  -- Reify the triple
  ts <- maybe (pure mempty) (uncurry reifyTriple) (liftA2 (,) mi mt)
  pure $ maybe ts (: ts) mt

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#parseTypeLiteralPropertyElt
pParseTypeLiteralPropertyElt :: Node -> RdfXmlParser Triples
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
  pure $ maybe ts (: ts) mt

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#parseTypeResourcePropertyElt
pParseTypeResourcePropertyElt :: Node -> RdfXmlParser Triples
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
  -- setSubject s
  pure $ maybe (ts1 <> ts2) ((<> ts2) . (: ts1)) mt

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#parseTypeCollectionPropertyElt
pParseTypeCollectionPropertyElt :: Node -> RdfXmlParser Triples
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
pParseTypeOtherPropertyElt :: Node -> RdfXmlParser Triples
pParseTypeOtherPropertyElt _p = do
  pt <- pRDFAttr rdfParseType
  guard (pt /= "Resource" && pt /= "Literal" && pt /= "Collection")
  checkAllowedAttributes [rdfParseType]
  _mi <- optional pIdAttr <* removeNodeAttr rdfID
  -- [FIXME] Implement 'parseTypeOtherPropertyElt'
  throwError "Not implemented: rdf:parseType = other"

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#emptyPropertyElt
pEmptyPropertyElt :: Node -> RdfXmlParser Triples
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
      pure (t : ts1 <> ts2)
  where
    pResourceAttr' = unode <$> pResourceAttr <* removeNodeAttr rdfResource
    pNodeIdAttr' = BNode <$> pNodeIdAttr <* removeNodeAttr rdfNodeID

checkAllowedAttributes :: HashSet Text -> RdfXmlParser ()
checkAllowedAttributes as = do
  attrs <- currentNodeAttrs
  let diffAttrs = HS.difference (HM.keysSet attrs) as
  unless (null diffAttrs) (throwError $ "Attributes not allowed: " <> show diffAttrs)

-- See: https://www.w3.org/TR/rdf11-concepts/#dfn-rdf-xmlliteral,
--      https://www.w3.org/TR/rdf-syntax-grammar/#literal
pXMLLiteral :: RdfXmlParser Text
pXMLLiteral =
  T.decodeUtf8 . BL.toStrict . BB.toLazyByteString . encode <$> pChildren

pIdAttr :: RdfXmlParser Text
pIdAttr = do
  i <- pRDFAttr rdfID
  i' <- either throwError pure (checkRdfId i)
  -- Check the uniqueness of the ID in the context of the current base URI.
  checkIdIsUnique i'
  pure i'

checkIdIsUnique :: Text -> RdfXmlParser ()
checkIdIsUnique i = do
  notUnique <- S.member i <$> currentIdSet
  when notUnique (throwError $ "rdf:ID already used in this context: " <> T.unpack i)
  updateIdSet i

pNodeIdAttr :: RdfXmlParser Text
pNodeIdAttr = do
  i <- pRDFAttr rdfNodeID
  either throwError pure (checkRdfId i)

pAboutAttr :: RdfXmlParser Text
pAboutAttr = pRDFAttr rdfAbout >>= checkIRI "rdf:about"

pResourceAttr :: RdfXmlParser Text
pResourceAttr = pRDFAttr rdfResource >>= checkIRI "rdf:resource"

pDatatypeAttr :: RdfXmlParser Text
pDatatypeAttr = pRDFAttr rdfDatatype >>= checkIRI "rdf:datatype"

reifyTriple :: Text -> Triple -> RdfXmlParser Triples
reifyTriple i (Triple s p' o) = do
  n <- mkUNodeID i
  pure
    [ Triple n rdfTypeNode rdfStatementNode,
      Triple n rdfSubjectNode s,
      Triple n rdfPredicateNode p',
      Triple n rdfObjectNode o
    ]

--------------------------------------------------------------------------------
-- URI checks

checkIRI :: String -> Text -> RdfXmlParser Text
checkIRI msg iri = do
  bUri <- maybe mempty unBaseUrl <$> currentBaseUri
  case uriValidate iri of
    Nothing -> throwError $ mconcat ["Malformed IRI for \"", msg, "\": ", T.unpack iri]
    Just iri' -> either throwError pure (resolveIRI bUri iri')

-- https://www.w3.org/TR/rdf-syntax-grammar/#propertyAttributeURIs
isPropertyAttrURI :: Text -> Bool
isPropertyAttrURI uri =
  isNotCoreSyntaxTerm uri
    && uri /= rdfDescription
    && uri /= rdfLi
    && isNotOldTerm uri

-- https://www.w3.org/TR/rdf-syntax-grammar/#coreSyntaxTerms
isNotCoreSyntaxTerm :: Text -> Bool
isNotCoreSyntaxTerm uri =
  uri /= rdfTag
    && uri /= rdfID
    && uri /= rdfAbout
    && uri /= rdfParseType
    && uri /= rdfResource
    && uri /= rdfNodeID
    && uri /= rdfDatatype

-- https://www.w3.org/TR/rdf-syntax-grammar/#oldTerms
isNotOldTerm :: Text -> Bool
isNotOldTerm uri =
  uri /= rdfAboutEach
    && uri /= rdfAboutEachPrefix
    && uri /= rdfBagID

--------------------------------------------------------------------------------
-- Parser's state utils

-- | Create a new unique blank node
newBNode :: RdfXmlParser Node
newBNode = do
  modify $ \st -> st {stateGenId = stateGenId st + 1}
  BNodeGen . stateGenId <$> get

-- | Process a parser, restoring the state except for stateGenId and stateIdSet
keepState :: RdfXmlParser a -> RdfXmlParser a
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
      then put (st {stateGenId = i})
      else put (st {stateGenId = i, stateIdSet = is <> is'})

currentIdSet :: RdfXmlParser (Set Text)
currentIdSet = stateIdSet <$> get

updateIdSet :: Text -> RdfXmlParser ()
updateIdSet i = do
  is <- currentIdSet
  modify (\st -> st {stateIdSet = S.insert i is})

currentNodeAttrs :: RdfXmlParser (HashMap Text Text)
currentNodeAttrs = stateNodeAttrs <$> get

setNodeAttrs :: HashMap Text Text -> RdfXmlParser ()
setNodeAttrs as = modify (\st -> st {stateNodeAttrs = as})

removeNodeAttr :: Text -> RdfXmlParser ()
removeNodeAttr a = HM.delete a <$> currentNodeAttrs >>= setNodeAttrs

currentPrefixMappings :: RdfXmlParser PrefixMappings
currentPrefixMappings = statePrefixMapping <$> get

updatePrefixMappings :: PrefixMappings -> RdfXmlParser PrefixMappings
updatePrefixMappings pm = do
  pm' <- (<> pm) <$> currentPrefixMappings
  modify (\st -> st {statePrefixMapping = pm'})
  pure pm'

currentCollectionIndex :: RdfXmlParser Int
currentCollectionIndex = stateCollectionIndex <$> get

setCollectionIndex :: Int -> RdfXmlParser ()
setCollectionIndex i = modify (\st -> st {stateCollectionIndex = i})

keepCollectionIndex :: RdfXmlParser a -> RdfXmlParser a
keepCollectionIndex p = do
  i <- currentCollectionIndex
  p <* setCollectionIndex i

-- See: https://www.w3.org/TR/rdf-syntax-grammar/#section-List-Expand
nextCollectionIndex :: RdfXmlParser Text
nextCollectionIndex = do
  modify $ \st -> st {stateCollectionIndex = stateCollectionIndex st + 1}
  (rdfListIndex <>) . T.pack . show . stateCollectionIndex <$> get

resetCollectionIndex :: RdfXmlParser ()
resetCollectionIndex = modify $ \st -> st {stateCollectionIndex = 0}

currentBaseUri :: RdfXmlParser (Maybe BaseUrl)
currentBaseUri = stateBaseUri <$> get

setBaseUri :: (Maybe BaseUrl) -> RdfXmlParser ()
setBaseUri u = modify (\st -> st {stateBaseUri = u})

mkUNodeID :: Text -> RdfXmlParser Node
mkUNodeID t = mkUnode <$> currentBaseUri
  where
    mkUnode =
      unode . \case
        Nothing -> t
        Just (BaseUrl u) -> mconcat [u, "#", t]

currentSubject :: RdfXmlParser (Maybe Subject)
currentSubject = stateSubject <$> get

setSubject :: (Maybe Subject) -> RdfXmlParser ()
setSubject s = modify (\st -> st {stateSubject = s})

currentLang :: RdfXmlParser (Maybe Text)
currentLang = stateLang <$> get

setLang :: (Maybe Text) -> RdfXmlParser ()
setLang lang = modify (\st -> st {stateLang = lang})
