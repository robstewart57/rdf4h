{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.RDF.Types
  ( -- * RDF triples, nodes and literals
    LValue (PlainL, PlainLL, TypedL),
    Node (UNode, BNode, BNodeGen, LNode),
    Subject,
    Predicate,
    Object,
    Triple (Triple),
    Triples,
    View (view),

    -- * Constructor functions
    plainL,
    plainLL,
    typedL,
    unode,
    bnode,
    bnodeUnsafe,
    lnode,
    triple,
    unodeValidate,
    uriValidate,
    uriValidateString,

    -- * Node query function
    isUNode,
    isLNode,
    isBNode,

    -- * Miscellaneous
    resolveQName,
    isAbsoluteUri,
    mkAbsoluteUrl,
    escapeRDFSyntax,
    unescapeUnicode,
    fileSchemeToFilePath,
    filePathToUri,
    iriFragment,
    uchar,

    -- * RDF data family
    RDF,

    -- * Rdf type class
    Rdf (baseUrl, prefixMappings, addPrefixMappings, empty, mkRdf, addTriple, removeTriple, triplesOf, uniqTriplesOf, select, query, showGraph),

    -- * Parsing RDF
    RdfParser (parseString, parseFile, parseURL),

    -- * Serializing RDF
    RdfSerializer (hWriteRdf, writeRdf, hWriteH, writeH, hWriteTs, hWriteT, writeT, writeTs, hWriteN, writeN),

    -- * Namespaces and Prefixes
    Namespace (PrefixedNS, PlainNS),
    PrefixMappings (PrefixMappings),
    PrefixMapping (PrefixMapping),

    -- * Supporting types
    BaseUrl (..),
    NodeSelector,
    ParseFailure (ParseFailure),
  )
where

#if MIN_VERSION_base(4,9,0)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#else
#endif
#else
#endif

import Control.Applicative
import qualified Control.Applicative as A
import Control.DeepSeq (NFData, rnf)
import Control.Monad (guard, (<=<))
import Data.Binary
import Data.Char (chr, ord)
import Data.Either (isRight)
import Data.Hashable (Hashable)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.RDF.BlankNode
import Data.RDF.IRI
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.URI
import qualified Network.URI as Network (parseURI, uriPath)
import qualified System.FilePath as FP
import System.IO
import Text.Parsec (ParseError, parse)
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Printf
import Prelude hiding (pred)

-------------------
-- LValue and constructor functions

-- | The actual value of an RDF literal, represented as the 'LValue'
--  parameter of an 'LNode'.
data LValue
  = -- Constructors are not exported, because we need to have more
    -- control over the format of the literal text that we store.

    -- | A plain (untyped) literal value in an unspecified language.
    PlainL !Text
  | -- | A plain (untyped) literal value with a language specifier.
    PlainLL !Text !Text
  | -- | A typed literal value consisting of the literal value and
    --  the URI of the datatype of the value, respectively.
    TypedL !Text !Text
  deriving (Generic, Show)

instance Binary LValue

instance NFData LValue where
  rnf (PlainL t) = rnf t
  rnf (PlainLL t1 t2) = rnf t1 `seq` rnf t2
  rnf (TypedL t1 t2) = rnf t1 `seq` rnf t2

-- | Return a PlainL LValue for the given string value.
{-# INLINE plainL #-}
plainL :: Text -> LValue
plainL = PlainL

-- | Return a PlainLL LValue for the given string value and language,
--  respectively.
{-# INLINE plainLL #-}
plainLL :: Text -> Text -> LValue
plainLL = PlainLL

-- | Return a TypedL LValue for the given string value and datatype URI,
--  respectively.
{-# INLINE typedL #-}
typedL :: Text -> Text -> LValue
typedL val dtype = TypedL (canonicalize dtype val) dtype

-------------------
-- Node and constructor functions

-- | An RDF node, which may be either a URIRef node ('UNode'), a blank
--  node ('BNode'), or a literal node ('LNode').
data Node
  = -- | An RDF URI reference. URIs conform to the RFC3986 standard. See
    --  <http://www.w3.org/TR/rdf-concepts/#section-Graph-URIref> for more
    --  information.
    UNode !Text
  | -- | An RDF blank node. See
    --  <http://www.w3.org/TR/rdf-concepts/#section-blank-nodes> for more
    --  information.
    BNode !Text
  | -- | An RDF blank node with an auto-generated identifier, as used in
    --  Turtle.
    BNodeGen !Int
  | -- | An RDF literal. See
    --  <http://www.w3.org/TR/rdf-concepts/#section-Graph-Literal> for more
    --  information.
    LNode !LValue
  deriving (Generic, Show)

instance Binary Node

instance NFData Node where
  rnf (UNode t) = rnf t
  rnf (BNode b) = rnf b
  rnf (BNodeGen bgen) = rnf bgen
  rnf (LNode lvalue) = rnf lvalue

-- | An alias for 'Node', defined for convenience and readability purposes.
type Subject = Node

-- | An alias for 'Node', defined for convenience and readability purposes.
type Predicate = Node

-- | An alias for 'Node', defined for convenience and readability purposes.
type Object = Node

-- | Return a URIRef node for the given URI.
{-# INLINE unode #-}
unode :: Text -> Node
unode = UNode

-- For background on 'unodeValidate', see:
-- http://stackoverflow.com/questions/33250184/unescaping-unicode-literals-found-in-haskell-strings
--
-- Escaped literals are defined in the Turtle spec, and is
-- inherited by the NTriples and XML specification.
-- http://www.w3.org/TR/turtle/#sec-escapes

-- | Validate a URI and return it in a @Just UNode@ if it is
--   valid, otherwise @Nothing@ is returned. Performs the following:
--
--   1. unescape unicode RDF literals
--   2. checks validity of this unescaped URI using 'isURI' from 'Network.URI'
--   3. if the unescaped URI is valid then 'Node' constructed with 'UNode'
unodeValidate :: Text -> Maybe Node
unodeValidate t = UNode <$> uriValidate t

-- | Validate a Text URI and return it in a @Just Text@ if it is
--   valid, otherwise @Nothing@ is returned. See 'unodeValidate'.
uriValidate :: Text -> Maybe Text
uriValidate = either (const Nothing) Just . isRdfURI

-- | Same as 'uriValidate', but on 'String' rather than 'Text'
uriValidateString :: String -> Maybe String
uriValidateString = fmap T.unpack . uriValidate . fromString

isRdfURI :: Text -> Either ParseError Text
isRdfURI t = parse (iriFragment <* eof) ("Invalid URI: " <> T.unpack t) t

-- IRIREF from NTriples spec (without <> enclosing)
-- [8] IRIREF ::= '<' ([^#x00-#x20<>"{}|^`\] | UCHAR)* '>'
iriFragment :: (CharParsing m, Monad m) => m Text
iriFragment = T.pack <$> many validUriChar
  where
    validUriChar = try (satisfy isValidUriChar) <|> validUnicodeEscaped
    validUnicodeEscaped = do
      c <- uchar
      guard (isValidUriChar c)
      return c
    isValidUriChar c =
      not (c >= '\x00' && c <= '\x20')
        && c `notElem` ("<>\"{}|^`\\" :: String)

-- UCHAR from NTriples spec
-- [10] UCHAR ::= '\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
uchar :: (CharParsing m, Monad m) => m Char
uchar = try shortUnicode <|> try longUnicode
  where
    shortUnicode = string "\\u" *> unescapeUnicodeParser 4
    longUnicode = string "\\U" *> unescapeUnicodeParser 8

unescapeUnicodeParser :: (CharParsing m, Monad m) => Int -> m Char
unescapeUnicodeParser n = do
  c <- go n 0
  guard (c <= 0x10FFFF)
  return $ chr c
  where
    {-# INLINE go #-}
    go 0 t = pure t
    go k t = do
      h <- anyChar >>= getHex
      let t' = t * 16 + h
      seq t' <$> go (k - 1) t'
    {-# INLINE getHex #-}
    getHex c
      | '0' <= c && c <= '9' = pure (ord c - ord '0')
      | 'A' <= c && c <= 'F' = pure (ord c - ord 'A' + 10)
      | 'a' <= c && c <= 'f' = pure (ord c - ord 'a' + 10)
      | otherwise = A.empty

-- | Unescapes @\Uxxxxxxxx@ and @\uxxxx@ character sequences according
--   to the RDF specification.
unescapeUnicode, escapeRDFSyntax :: Text -> Either ParseError Text
unescapeUnicode t = T.pack <$> parse (many unicodeEsc) "" t
  where
    unicodeEsc = uchar <|> anyChar
{-# DEPRECATED escapeRDFSyntax "Use unescapeUnicode instead" #-}
escapeRDFSyntax = unescapeUnicode

-- | Creates a blank node 'BNode' with a given label. Checks that
-- label is a syntactically valid label for a blank node i.e. a
-- BLANK_NODE_LABEL in
-- https://www.w3.org/TR/n-triples/#n-triples-grammar . Returns
-- 'Nothing' for invalid blank node labels. Blank node labels are
-- written as "_:abc" for a blank node with label "abc" see
-- https://www.w3.org/TR/sparql11-query/#QSynBlankNodes .
--
-- >>> bnode "_:abc"
-- Just (BNode "abc")
--
-- >>> bnode "abc"
-- Nothing
--
-- This does not check that the blank node label is unique for a
-- graph, since the function is not associated with a graph.
{-# INLINE bnode #-}
bnode :: Text -> Maybe Node
bnode t =
  case mkBNode t of
    Nothing -> Nothing
    Just bString -> Just (BNode (T.pack bString))

-- | Return a blank node using the given label. Does not check that
-- label is a syntactically valid label for a blank node i.e. a
-- BLANK_NODE_LABEL in
-- https://www.w3.org/TR/n-triples/#n-triples-grammar .
{-# INLINE bnodeUnsafe #-}
bnodeUnsafe :: Text -> Node
bnodeUnsafe = BNode

-- | Return a literal node using the given LValue.
{-# INLINE lnode #-}
lnode :: LValue -> Node
lnode = LNode

-------------------
-- Triple and constructor functions

-- | An RDF triple is a statement consisting of a subject, predicate,
--  and object, respectively.
--
--  See <http://www.w3.org/TR/rdf-concepts/#section-triples> for
--  more information.
data Triple = Triple !Node !Node !Node
  deriving (Generic, Show)

instance Binary Triple

instance NFData Triple where
  rnf (Triple s p o) = rnf s `seq` rnf p `seq` rnf o

-- | A list of triples. This is defined for convenience and readability.
type Triples = [Triple]

-- | A smart constructor function for 'Triple' that verifies the node arguments
--  are of the correct type and creates the new 'Triple' if so or calls 'error'.
--  /subj/ must be a 'UNode' or 'BNode', and /pred/ must be a 'UNode'.
triple :: Subject -> Predicate -> Object -> Triple
triple s p o
  | isLNode s = error $ "subject must be UNode or BNode: " <> show s
  | isLNode p = error $ "predicate must be UNode, not LNode: " <> show p
  | isBNode p = error $ "predicate must be UNode, not BNode: " <> show p
  | otherwise = Triple s p o

-- | Answer if given node is a URI Ref node.
{-# INLINE isUNode #-}
isUNode :: Node -> Bool
isUNode (UNode _) = True
isUNode _ = False

-- | Answer if given node is a blank node.
{-# INLINE isBNode #-}
isBNode :: Node -> Bool
isBNode (BNode _) = True
isBNode (BNodeGen _) = True
isBNode _ = False

-- | Answer if given node is a literal node.
{-# INLINE isLNode #-}
isLNode :: Node -> Bool
isLNode (LNode _) = True
isLNode _ = False

{-# INLINE isAbsoluteUri #-}

-- | returns @True@ if URI is absolute.
isAbsoluteUri :: Text -> Bool
isAbsoluteUri = isRight . parseIRI

-- | A type class for ADTs that expose views to clients.
class View a b where
  view :: a -> b

-- | RDF data family
data family RDF a

-- | An RDF value is a set of (unique) RDF triples, together with the
--  operations defined upon them.
--
--  For information about the efficiency of the functions, see the
--  documentation for the particular RDF instance.
--
--  For more information about the concept of an RDF graph, see
--  the following: <http://www.w3.org/TR/rdf-concepts/#section-rdf-graph>.
class (Generic rdfImpl, NFData rdfImpl) => Rdf rdfImpl where
  -- | Return the base URL of this RDF, if any.
  baseUrl :: RDF rdfImpl -> Maybe BaseUrl

  -- | Return the prefix mappings defined for this RDF, if any.
  prefixMappings :: RDF rdfImpl -> PrefixMappings

  -- | Return an RDF with the specified prefix mappings merged with
  --  the existing mappings. If the Bool arg is True, then a new mapping
  --  for an existing prefix will replace the old mapping; otherwise,
  --  the new mapping is ignored.
  addPrefixMappings :: RDF rdfImpl -> PrefixMappings -> Bool -> RDF rdfImpl

  -- | Return an empty RDF.
  empty :: RDF rdfImpl

  -- | Return a RDF containing all the given triples. Handling of duplicates
  --  in the input depend on the particular RDF implementation.
  mkRdf :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF rdfImpl

  -- | Adds a triple to an RDF graph.
  addTriple :: RDF rdfImpl -> Triple -> RDF rdfImpl

  -- | Removes all occurrences of a triple in an RDF graph.
  removeTriple :: RDF rdfImpl -> Triple -> RDF rdfImpl

  -- | Return all triples in the RDF, as a list.
  --
  --  Note that this function returns a list of triples in the RDF as they
  --  were added, without removing duplicates and without expanding namespaces.
  triplesOf :: RDF rdfImpl -> Triples

  -- | Return unique triples in the RDF, as a list.
  --
  --  This function performs namespace expansion and removal of duplicates.
  uniqTriplesOf :: RDF rdfImpl -> Triples

  -- | Select the triples in the RDF that match the given selectors.
  --
  --  The three NodeSelector parameters are optional functions that match
  --  the respective subject, predicate, and object of a triple. The triples
  --  returned are those in the given graph for which the first selector
  --  returns true when called on the subject, the second selector returns
  --  true when called on the predicate, and the third selector returns true
  --  when called on the ojbect. A 'Nothing' parameter is equivalent to a
  --  function that always returns true for the appropriate node; but
  --  implementations may be able to much more efficiently answer a select
  --  that involves a 'Nothing' parameter rather than an @(id True)@ parameter.
  --
  --  The following call illustrates the use of select, and would result in
  --  the selection of all and only the triples that have a blank node
  --  as subject and a literal node as object:
  --
  --  > select gr (Just isBNode) Nothing (Just isLNode)
  --
  --  Note: this function may be very slow; see the documentation for the
  --  particular RDF implementation for more information.
  select :: RDF rdfImpl -> NodeSelector -> NodeSelector -> NodeSelector -> Triples

  -- | Return the triples in the RDF that match the given pattern, where
  --  the pattern (3 Maybe Node parameters) is interpreted as a triple pattern.
  --
  --  The @Maybe Node@ params are interpreted as the subject, predicate, and
  --  object of a triple, respectively. @Just n@ is true iff the triple has
  --  a node equal to @n@ in the appropriate location; @Nothing@ is always
  --  true, regardless of the node in the appropriate location.
  --
  --  For example, @ query rdf (Just n1) Nothing (Just n2) @ would return all
  --  and only the triples that have @n1@ as subject and @n2@ as object,
  --  regardless of the predicate of the triple.
  query :: RDF rdfImpl -> Maybe Node -> Maybe Node -> Maybe Node -> Triples

  -- | pretty prints the RDF graph
  showGraph :: RDF rdfImpl -> String

instance (Rdf a) => Show (RDF a) where
  show = showGraph

-- | An RdfParser is a parser that knows how to parse 1 format of RDF and
--  can parse an RDF document of that type from a string, a file, or a URL.
--  Required configuration options will vary from instance to instance.
class RdfParser p where
  -- | Parse RDF from the given text, yielding a failure with error message or
  --  the resultant RDF.
  parseString :: (Rdf a) => p -> Text -> Either ParseFailure (RDF a)

  -- | Parse RDF from the local file with the given path, yielding a failure with error
  --  message or the resultant RDF in the IO monad.
  parseFile :: (Rdf a) => p -> String -> IO (Either ParseFailure (RDF a))

  -- | Parse RDF from the remote file with the given HTTP URL (https is not supported),
  --  yielding a failure with error message or the resultant graph in the IO monad.
  parseURL :: (Rdf a) => p -> String -> IO (Either ParseFailure (RDF a))

-- | An RdfSerializer is a serializer of RDF to some particular output format, such as
--  NTriples or Turtle.
class RdfSerializer s where
  -- | Write the RDF to a file handle using whatever configuration is specified by
  --  the first argument.
  hWriteRdf :: (Rdf a) => s -> Handle -> RDF a -> IO ()

  -- | Write the RDF to stdout; equivalent to @'hWriteRdf' stdout@.
  writeRdf :: (Rdf a) => s -> RDF a -> IO ()

  -- | Write to the file handle whatever header information is required based on
  --  the output format. For example, if serializing to Turtle, this method would
  --  write the necessary \@prefix declarations and possibly a \@baseUrl declaration,
  --  whereas for NTriples, there is no header section at all, so this would be a no-op.
  hWriteH :: (Rdf a) => s -> Handle -> RDF a -> IO ()

  -- | Write header information to stdout; equivalent to @'hWriteRdf' stdout@.
  writeH :: (Rdf a) => s -> RDF a -> IO ()

  -- | Write some triples to a file handle using whatever configuration is specified
  --  by the first argument.
  --
  --  WARNING: if the serialization format has header-level information
  --  that should be output (e.g., \@prefix declarations for Turtle), then you should
  --  use 'hWriteG' instead of this method unless you're sure this is safe to use, since
  --  otherwise the resultant document will be missing the header information and
  --  will not be valid.
  hWriteTs :: s -> Handle -> Triples -> IO ()

  -- | Write some triples to stdout; equivalent to @'hWriteTs' stdout@.
  writeTs :: s -> Triples -> IO ()

  -- | Write a single triple to the file handle using whatever configuration is
  --  specified by the first argument. The same WARNING applies as to 'hWriteTs'.
  hWriteT :: s -> Handle -> Triple -> IO ()

  -- | Write a single triple to stdout; equivalent to @'hWriteT' stdout@.
  writeT :: s -> Triple -> IO ()

  -- | Write a single node to the file handle using whatever configuration is
  --  specified by the first argument. The same WARNING applies as to 'hWriteTs'.
  hWriteN :: s -> Handle -> Node -> IO ()

  -- | Write a single node to sdout; equivalent to @'hWriteN' stdout@.
  writeN :: s -> Node -> IO ()

-- | The base URL of an RDF.
newtype BaseUrl = BaseUrl {unBaseUrl :: Text}
  deriving (Eq, Ord, Show, NFData, Semigroup, Generic)

instance Binary BaseUrl

instance Monoid BaseUrl where
  mempty = BaseUrl T.empty

-- | A 'NodeSelector' is either a function that returns 'True'
--   or 'False' for a node, or Nothing, which indicates that all
--  nodes would return 'True'.
--
--  The selector is said to select, or match, the nodes for
--  which it returns 'True'.
--
--  When used in conjunction with the 'select' method of 'Graph', three
--  node selectors are used to match a triple.
type NodeSelector = Maybe (Node -> Bool)

-- | Represents a failure in parsing an N-Triples document, including
--  an error message with information about the cause for the failure.
newtype ParseFailure = ParseFailure String
  deriving (Eq, Show)

-- | A node is equal to another node if they are both the same type
--  of node and if the field values are equal.
instance Eq Node where
  (UNode bs1) == (UNode bs2) = bs1 == bs2
  (BNode bs1) == (BNode bs2) = bs1 == bs2
  (BNodeGen i1) == (BNodeGen i2) = i1 == i2
  (LNode l1) == (LNode l2) = l1 == l2
  _ == _ = False

-- | Node ordering is defined first by type, with Unode < BNode < BNodeGen
--  < LNode PlainL < LNode PlainLL < LNode TypedL, and secondly by
--  the natural ordering of the node value.
--
--  E.g., a '(UNode _)' is LT any other type of node, and a
--  '(LNode (TypedL _ _))' is GT any other type of node, and the ordering
--  of '(BNodeGen 44)' and '(BNodeGen 3)' is that of the values, or
--  'compare 44 3', GT.
instance Ord Node where
  compare (UNode bs1) (UNode bs2) = compare bs1 bs2
  compare (UNode _) _ = LT
  compare _ (UNode _) = GT
  compare (BNode bs1) (BNode bs2) = compare bs1 bs2
  compare (BNode _) _ = LT
  compare _ (BNode _) = GT
  compare (BNodeGen i1) (BNodeGen i2) = compare i1 i2
  compare (BNodeGen _) _ = LT
  compare _ (BNodeGen _) = GT
  compare (LNode lv1) (LNode lv2) = compare lv1 lv2

instance Hashable Node

-- | Two triples are equal iff their respective subjects, predicates, and objects
--  are equal.
instance Eq Triple where
  (Triple s1 p1 o1) == (Triple s2 p2 o2) = s1 == s2 && p1 == p2 && o1 == o2

-- | The ordering of triples is based on that of the subject, predicate, and object
--  of the triple, in that order.
instance Ord Triple where
  {-# INLINE compare #-}
  (Triple s1 p1 o1) `compare` (Triple s2 p2 o2) =
    compare s1 s2 `mappend` compare p1 p2 `mappend` compare o1 o2

-- | Two 'LValue' values are equal iff they are of the same type and all fields are equal.
instance Eq LValue where
  (PlainL v1) == (PlainL v2) = v1 == v2
  (PlainLL v1 lt1) == (PlainLL v2 lt2) = T.toLower lt1 == T.toLower lt2 && v1 == v2
  (TypedL v1 dt1) == (TypedL v2 dt2) = v1 == v2 && dt1 == dt2
  _ == _ = False

-- | Ordering of 'LValue' values is as follows: (PlainL _) < (PlainLL _ _)
--  < (TypedL _ _), and values of the same type are ordered by field values,
--  with '(PlainLL literalValue language)' being ordered by language first and
--  literal value second, and '(TypedL literalValue datatypeUri)' being ordered
--  by datatype first and literal value second.
instance Ord LValue where
  {-# INLINE compare #-}
  compare (PlainL v1) (PlainL v2) = compare v1 v2
  compare (PlainL _) _ = LT
  compare _ (PlainL _) = GT
  compare (PlainLL v1 lt1) (PlainLL v2 lt2) = compare lt1 lt2 `mappend` compare v1 v2
  compare (PlainLL _ _) _ = LT
  compare _ (PlainLL _ _) = GT
  compare (TypedL v1 dt1) (TypedL v2 dt2) = compare dt1 dt2 `mappend` compare v1 v2

instance Hashable LValue

------------------------
-- Prefix mappings

-- | Represents a namespace as either a prefix and uri, respectively,
--   or just a uri.
data Namespace
  = PrefixedNS Text Text -- prefix and ns uri
  | PlainNS Text -- ns uri alone

instance Eq Namespace where
  (PrefixedNS _ u1) == (PrefixedNS _ u2) = u1 == u2
  (PlainNS u1) == (PlainNS u2) = u1 == u2
  (PrefixedNS _ u1) == (PlainNS u2) = u1 == u2
  (PlainNS u1) == (PrefixedNS _ u2) = u1 == u2

instance Show Namespace where
  show (PlainNS uri) = T.unpack uri
  show (PrefixedNS prefix uri) = printf "(PrefixNS %s %s)" (T.unpack prefix) (T.unpack uri)

-- | An alias for a map from prefix to namespace URI.
newtype PrefixMappings = PrefixMappings (Map Text Text)
  deriving (Eq, Ord, NFData, Semigroup, Monoid, Generic)

instance Binary PrefixMappings

instance Show PrefixMappings where
  -- This is really inefficient, but it's not used much so not what
  -- worth optimizing yet.
  show (PrefixMappings pmap) = printf "PrefixMappings [%s]" mappingsStr
    where
      showPM = show . PrefixMapping
      mappingsStr = List.intercalate ", " (fmap showPM (Map.toList pmap))

-- | A mapping of a prefix to the URI for that prefix.
newtype PrefixMapping = PrefixMapping (Text, Text)
  deriving (Eq, Ord)

instance Show PrefixMapping where
  show (PrefixMapping (prefix, uri)) = printf "PrefixMapping (%s, %s)" (show prefix) (show uri)

-----------------
-- Miscellaneous helper functions used throughout the project

-- | Resolve a prefix using the given prefix mappings.
resolveQName :: Text -> PrefixMappings -> Maybe Text
resolveQName prefix (PrefixMappings pms) = Map.lookup prefix pms

{-# INLINE mkAbsoluteUrl #-}
{-# DEPRECATED mkAbsoluteUrl "Use resolveIRI instead, because mkAbsoluteUrl is a partial function" #-}

-- | Make an absolute URL by returning as is if already an absolute URL and otherwise
--   appending the URL to the given base URL.
mkAbsoluteUrl :: Text -> Text -> Text
mkAbsoluteUrl base iri = either error id (resolveIRI base iri)

-----------------
-- Internal canonicalize functions, don't export

-- | Canonicalize the given 'Text' value using the 'Text'
--  as the datatype URI.
{-# NOINLINE canonicalize #-}
canonicalize :: Text -> Text -> Text
canonicalize typeTxt litValue =
  maybe litValue ($ litValue) (Map.lookup typeTxt canonicalizerTable)

-- A table of mappings from a 'Text' URI
-- to a function that canonicalizes a Text
-- assumed to be of that type.
{-# NOINLINE canonicalizerTable #-}
canonicalizerTable :: Map Text (Text -> Text)
canonicalizerTable =
  [(integerUri, _integerStr), (doubleUri, _doubleStr), (decimalUri, _decimalStr)]
  where
    integerUri = "http://www.w3.org/2001/XMLSchema#integer"
    decimalUri = "http://www.w3.org/2001/XMLSchema#decimal"
    doubleUri = "http://www.w3.org/2001/XMLSchema#double"

_integerStr, _decimalStr, _doubleStr :: Text -> Text
_integerStr t
  | T.length t == 1 = t
  | T.head t == '0' = _integerStr (T.tail t)
  | otherwise = t
-- exponent: [eE] ('-' | '+')? [0-9]+
-- ('-' | '+') ? ( [0-9]+ '.' [0-9]* exponent | '.' ([0-9])+ exponent | ([0-9])+ exponent )
_doubleStr s = T.pack $ show (read $ T.unpack s :: Double)
-- ('-' | '+')? ( [0-9]+ '.' [0-9]* | '.' ([0-9])+ | ([0-9])+ )
_decimalStr s =
  -- haskell double parser doesn't handle '1.'..,
  case T.last s of -- so we add a zero if that's the case and then parse
    '.' -> f (s `T.snoc` '0')
    _ -> f s
  where
    f s' = T.pack $ show (read $ T.unpack s' :: Double)

-- | Removes "file://" schema from URIs in 'UNode' nodes
fileSchemeToFilePath :: (IsString s) => Node -> Maybe s
fileSchemeToFilePath (UNode fileScheme)
  | "file://" `T.isPrefixOf` fileScheme = textToFilePath fileScheme
  | otherwise = Nothing
  where
    textToFilePath = pure . fromString <=< stringToFilePath . T.unpack
    stringToFilePath = fixPrefix <=< pure . unEscapeString . Network.uriPath <=< Network.parseURI
    fixPrefix "" = Nothing
    fixPrefix p@(p' : p'')
      | p' == FP.pathSeparator = Just (FP.normalise p) -- Posix path
      | p' == '/' = Just (FP.normalise p'') -- Windows classic Path
      | otherwise = Just ("\\\\" <> FP.normalise p) -- Windows UNC Path
fileSchemeToFilePath _ = Nothing

-- | Converts a file path to a URI with "file:" scheme
filePathToUri :: (IsString s) => FilePath -> Maybe s
filePathToUri p
  | FP.isRelative p = Nothing
  | otherwise = Just . fromString . as_uri . FP.normalise $ p
  where
    as_uri = ("file://" <>) . escapeURIString isAllowedInURI . as_posix . fix_prefix
    fix_prefix p' = case (FP.takeDrive p') of
      "/" -> p'
      '\\' : '\\' : _ -> drop 2 p'
      _ -> '/' : p'
    as_posix = fmap repl
    repl '\\' = '/'
    repl c = c
