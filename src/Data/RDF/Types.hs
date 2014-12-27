{-# LANGUAGE DeriveGeneric, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Data.RDF.Types (

  -- * RDF triples, nodes and literals
  LValue(PlainL,PlainLL,TypedL),
  Node(UNode,BNode,BNodeGen,LNode), Subject, Predicate, Object,
  Triple(Triple), Triples, View(view),

  -- * Constructor functions
  plainL,plainLL,typedL,
  unode,bnode,lnode,triple,

  -- * Node query function
  isUNode,isLNode,isBNode,

  -- * Miscellaneous
  resolveQName, absolutizeUrl, isAbsoluteUri, mkAbsoluteUrl,

  -- * RDF Type
  RDF(baseUrl,prefixMappings,addPrefixMappings,empty,mkRdf,triplesOf,uniqTriplesOf,select,query),

  -- * Parsing RDF
  RdfParser(parseString,parseFile,parseURL),

  -- * Serializing RDF
  RdfSerializer(hWriteRdf,writeRdf,hWriteH,writeH,hWriteTs,hWriteT,writeT, writeTs,hWriteN, writeN),

  -- * Namespaces and Prefixes
  Namespace(PrefixedNS,PlainNS),
  PrefixMappings(PrefixMappings),PrefixMapping(PrefixMapping),

  -- * Supporting types
  BaseUrl(BaseUrl), NodeSelector, ParseFailure(ParseFailure)

) where

import Prelude hiding (pred)
import qualified Data.Text as T
import System.IO
import Text.Printf
import Data.Map(Map)
import GHC.Generics (Generic)
import Data.Hashable(Hashable)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Network.URI as Network (isURI)
import Control.DeepSeq (NFData,rnf)

-------------------
-- LValue and constructor functions

-- |The actual value of an RDF literal, represented as the 'LValue'
-- parameter of an 'LNode'.
data LValue =
  -- Constructors are not exported, because we need to have more
  -- control over the format of the literal text that we store.

  -- |A plain (untyped) literal value in an unspecified language.
  PlainL !T.Text

  -- |A plain (untyped) literal value with a language specifier.
  | PlainLL !T.Text !T.Text

  -- |A typed literal value consisting of the literal value and
  -- the URI of the datatype of the value, respectively.
  | TypedL !T.Text  !T.Text
    deriving Generic

instance NFData LValue where
  rnf (PlainL t) = rnf t
  rnf (PlainLL t1 t2) = rnf t1 `seq` rnf t2
  rnf (TypedL t1 t2) = rnf t1 `seq` rnf t2

-- |Return a PlainL LValue for the given string value.
{-# INLINE plainL #-}
plainL :: T.Text -> LValue
plainL =  PlainL

-- |Return a PlainLL LValue for the given string value and language,
-- respectively.
{-# INLINE plainLL #-}
plainLL :: T.Text -> T.Text -> LValue
plainLL = PlainLL

-- |Return a TypedL LValue for the given string value and datatype URI,
-- respectively.
{-# INLINE typedL #-}
typedL :: T.Text -> T.Text -> LValue
typedL val dtype = TypedL (canonicalize dtype val) dtype

-------------------
-- Node and constructor functions

-- |An RDF node, which may be either a URIRef node ('UNode'), a blank
-- node ('BNode'), or a literal node ('LNode').
data Node =

  -- |An RDF URI reference. URIs conform to the RFC3986 standard. See
  -- <http://www.w3.org/TR/rdf-concepts/#section-Graph-URIref> for more
  -- information.
  UNode !T.Text

  -- |An RDF blank node. See
  -- <http://www.w3.org/TR/rdf-concepts/#section-blank-nodes> for more
  -- information.
  | BNode !T.Text

  -- |An RDF blank node with an auto-generated identifier, as used in
  -- Turtle.
  | BNodeGen !Int

  -- |An RDF literal. See
  -- <http://www.w3.org/TR/rdf-concepts/#section-Graph-Literal> for more
  -- information.
  | LNode !LValue
    deriving Generic

instance NFData Node where
  rnf (UNode t) = rnf t
  rnf (BNode b) = rnf b
  rnf (BNodeGen bgen) = rnf bgen
  rnf (LNode lvalue) = rnf lvalue

-- |An alias for 'Node', defined for convenience and readability purposes.
type Subject = Node

-- |An alias for 'Node', defined for convenience and readability purposes.
type Predicate = Node

-- |An alias for 'Node', defined for convenience and readability purposes.
type Object = Node

-- |Return a URIRef node for the given bytetring URI.
{-# INLINE unode #-}
unode :: T.Text -> Node
unode = UNode

-- |Return a blank node using the given string identifier.
{-# INLINE bnode #-}
bnode :: T.Text ->  Node
bnode = BNode

-- |Return a literal node using the given LValue.
{-# INLINE lnode #-}
lnode :: LValue ->  Node
lnode = LNode

-------------------
-- Triple and constructor functions

-- |An RDF triple is a statement consisting of a subject, predicate,
-- and object, respectively.
--
-- See <http://www.w3.org/TR/rdf-concepts/#section-triples> for
-- more information.
data Triple = Triple !Node !Node !Node

instance NFData Triple where
  rnf (Triple s p o) = rnf s `seq` rnf p `seq` rnf o

-- |A list of triples. This is defined for convenience and readability.
type Triples = [Triple]

-- |A smart constructor function for 'Triple' that verifies the node arguments
-- are of the correct type and creates the new 'Triple' if so or calls 'error'.
-- /subj/ must be a 'UNode' or 'BNode', and /pred/ must be a 'UNode'.
triple :: Subject -> Predicate -> Object -> Triple
triple subj pred obj
  | isLNode subj     =  error $ "subject must be UNode or BNode: "     ++ show subj
  | isLNode pred     =  error $ "predicate must be UNode, not LNode: " ++ show pred
  | isBNode pred     =  error $ "predicate must be UNode, not BNode: " ++ show pred
  | otherwise        =  Triple subj pred obj

-- |Answer if given node is a URI Ref node.
{-# INLINE isUNode #-}
isUNode :: Node -> Bool
isUNode (UNode _) = True
isUNode _         = False

-- |Answer if given node is a blank node.
{-# INLINE isBNode #-}
isBNode :: Node -> Bool
isBNode (BNode _)    = True
isBNode (BNodeGen _) = True
isBNode _            = False

-- |Answer if given node is a literal node.
{-# INLINE isLNode #-}
isLNode :: Node -> Bool
isLNode (LNode _) = True
isLNode _         = False

{-# INLINE isAbsoluteUri #-}
isAbsoluteUri :: T.Text -> Bool
isAbsoluteUri = Network.isURI . T.unpack

-- |A type class for ADTs that expose views to clients.
class View a b where
  view :: a -> b

-- |An RDF value is a set of (unique) RDF triples, together with the
-- operations defined upon them.
--
-- For information about the efficiency of the functions, see the
-- documentation for the particular RDF instance.
--
-- For more information about the concept of an RDF graph, see
-- the following: <http://www.w3.org/TR/rdf-concepts/#section-rdf-graph>.
class RDF rdf where

  -- |Return the base URL of this RDF, if any.
  baseUrl :: rdf -> Maybe BaseUrl

  -- |Return the prefix mappings defined for this RDF, if any.
  prefixMappings :: rdf -> PrefixMappings

  -- |Return an RDF with the specified prefix mappings merged with
  -- the existing mappings. If the Bool arg is True, then a new mapping
  -- for an existing prefix will replace the old mapping; otherwise,
  -- the new mapping is ignored.
  addPrefixMappings :: rdf -> PrefixMappings -> Bool -> rdf

  -- |Return an empty RDF.
  empty  :: rdf

  -- |Return a RDF containing all the given triples. Handling of duplicates
  -- in the input depend on the particular RDF implementation.
  mkRdf :: Triples -> Maybe BaseUrl -> PrefixMappings -> rdf

  -- |Return all triples in the RDF, as a list.
  --
  -- Note that this function returns a list of triples in the RDF as they
  -- were added, without removing duplicates and without expanding namespaces.
  triplesOf :: rdf -> Triples

  -- |Return unique triples in the RDF, as a list.
  --
  -- This function performs namespace expansion and removal of duplicates.
  uniqTriplesOf :: rdf -> Triples

  -- |Select the triples in the RDF that match the given selectors.
  --
  -- The three NodeSelector parameters are optional functions that match
  -- the respective subject, predicate, and object of a triple. The triples
  -- returned are those in the given graph for which the first selector
  -- returns true when called on the subject, the second selector returns
  -- true when called on the predicate, and the third selector returns true
  -- when called on the ojbect. A 'Nothing' parameter is equivalent to a
  -- function that always returns true for the appropriate node; but
  -- implementations may be able to much more efficiently answer a select
  -- that involves a 'Nothing' parameter rather than an @(id True)@ parameter.
  --
  -- The following call illustrates the use of select, and would result in
  -- the selection of all and only the triples that have a blank node
  -- as subject and a literal node as object:
  --
  -- > select gr (Just isBNode) Nothing (Just isLNode)
  --
  -- Note: this function may be very slow; see the documentation for the
  -- particular RDF implementation for more information.
  select    :: rdf -> NodeSelector -> NodeSelector -> NodeSelector -> Triples

  -- |Return the triples in the RDF that match the given pattern, where
  -- the pattern (3 Maybe Node parameters) is interpreted as a triple pattern.
  --
  -- The @Maybe Node@ params are interpreted as the subject, predicate, and
  -- object of a triple, respectively. @Just n@ is true iff the triple has
  -- a node equal to @n@ in the appropriate location; @Nothing@ is always
  -- true, regardless of the node in the appropriate location.
  --
  -- For example, @ query rdf (Just n1) Nothing (Just n2) @ would return all
  -- and only the triples that have @n1@ as subject and @n2@ as object,
  -- regardless of the predicate of the triple.
  query         :: rdf -> Maybe Node -> Maybe Node -> Maybe Node -> Triples

-- |An RdfParser is a parser that knows how to parse 1 format of RDF and
-- can parse an RDF document of that type from a string, a file, or a URL.
-- Required configuration options will vary from instance to instance.
class RdfParser p where

  -- |Parse RDF from the given text, yielding a failure with error message or
  -- the resultant RDF.
  parseString :: forall rdf. (RDF rdf) => p -> T.Text -> Either ParseFailure rdf

  -- |Parse RDF from the local file with the given path, yielding a failure with error
  -- message or the resultant RDF in the IO monad.
  parseFile   :: forall rdf. (RDF rdf) => p -> String     -> IO (Either ParseFailure rdf)

  -- |Parse RDF from the remote file with the given HTTP URL (https is not supported),
  -- yielding a failure with error message or the resultant graph in the IO monad.
  parseURL    :: forall rdf. (RDF rdf) => p -> String -> IO (Either ParseFailure rdf)


-- |An RdfSerializer is a serializer of RDF to some particular output format, such as
-- NTriples or Turtle.
class RdfSerializer s where
  -- |Write the RDF to a file handle using whatever configuration is specified by
  -- the first argument.
  hWriteRdf     :: forall rdf. (RDF rdf) => s -> Handle -> rdf -> IO ()

  -- |Write the RDF to stdout; equivalent to @'hWriteRdf' stdout@.
  writeRdf      :: forall rdf. (RDF rdf) => s -> rdf -> IO ()

  -- |Write to the file handle whatever header information is required based on
  -- the output format. For example, if serializing to Turtle, this method would
  -- write the necessary \@prefix declarations and possibly a \@baseUrl declaration,
  -- whereas for NTriples, there is no header section at all, so this would be a no-op.
  hWriteH     :: forall rdf. (RDF rdf) => s -> Handle -> rdf -> IO ()

  -- |Write header information to stdout; equivalent to @'hWriteRdf' stdout@.
  writeH      :: forall rdf. (RDF rdf) => s -> rdf -> IO ()

  -- |Write some triples to a file handle using whatever configuration is specified
  -- by the first argument. 
  -- 
  -- WARNING: if the serialization format has header-level information 
  -- that should be output (e.g., \@prefix declarations for Turtle), then you should
  -- use 'hWriteG' instead of this method unless you're sure this is safe to use, since
  -- otherwise the resultant document will be missing the header information and 
  -- will not be valid.
  hWriteTs    :: s -> Handle  -> Triples -> IO ()

  -- |Write some triples to stdout; equivalent to @'hWriteTs' stdout@.
  writeTs     :: s -> Triples -> IO ()

  -- |Write a single triple to the file handle using whatever configuration is 
  -- specified by the first argument. The same WARNING applies as to 'hWriteTs'.
  hWriteT     :: s -> Handle  -> Triple  -> IO ()

  -- |Write a single triple to stdout; equivalent to @'hWriteT' stdout@.
  writeT      :: s -> Triple  -> IO ()

  -- |Write a single node to the file handle using whatever configuration is 
  -- specified by the first argument. The same WARNING applies as to 'hWriteTs'.
  hWriteN     :: s -> Handle  -> Node    -> IO ()

  -- |Write a single node to sdout; equivalent to @'hWriteN' stdout@.
  writeN      :: s -> Node    -> IO ()


-- |The base URL of an RDF.
newtype BaseUrl = BaseUrl T.Text
  deriving (Eq, Ord, Show, NFData)

-- |A 'NodeSelector' is either a function that returns 'True'
--  or 'False' for a node, or Nothing, which indicates that all
-- nodes would return 'True'.
--
-- The selector is said to select, or match, the nodes for
-- which it returns 'True'.
--
-- When used in conjunction with the 'select' method of 'Graph', three
-- node selectors are used to match a triple.
type NodeSelector = Maybe (Node -> Bool)

-- |Represents a failure in parsing an N-Triples document, including
-- an error message with information about the cause for the failure.
newtype ParseFailure = ParseFailure String
  deriving (Eq, Show)

-- |A node is equal to another node if they are both the same type
-- of node and if the field values are equal.
instance Eq Node where
  (UNode bs1)    ==  (UNode bs2)     =   bs1 ==  bs2
  (BNode bs1)    ==  (BNode bs2)     =   bs1 ==  bs2
  (BNodeGen i1)  ==  (BNodeGen i2)   =  i1 == i2
  (LNode l1)     ==  (LNode l2)      =  l1 == l2
  _              ==  _               =  False

-- |Node ordering is defined first by type, with Unode < BNode < BNodeGen
-- < LNode PlainL < LNode PlainLL < LNode TypedL, and secondly by
-- the natural ordering of the node value.
--
-- E.g., a '(UNode _)' is LT any other type of node, and a
-- '(LNode (TypedL _ _))' is GT any other type of node, and the ordering
-- of '(BNodeGen 44)' and '(BNodeGen 3)' is that of the values, or
-- 'compare 44 3', GT.
instance Ord Node where
  compare = compareNode

compareNode :: Node -> Node -> Ordering
compareNode (UNode bs1)                      (UNode bs2)                      = compare bs1 bs2
compareNode (UNode _)                        _                                = LT
compareNode (BNode bs1)                      (BNode bs2)                      = compare bs1 bs2
compareNode (BNode _)                        (UNode _)                        = GT
compareNode (BNode _)                        _                                = LT
compareNode (BNodeGen i1)                    (BNodeGen i2)                    = compare i1 i2
compareNode (BNodeGen _)                     (LNode _)                        = LT
compareNode (BNodeGen _)                     _                                = GT
compareNode (LNode (PlainL bs1))             (LNode (PlainL bs2))             = compare bs1 bs2
compareNode (LNode (PlainL _))               (LNode _)                        = LT
compareNode (LNode (PlainLL bs1 bs1'))       (LNode (PlainLL bs2 bs2'))       =
  case compare bs1' bs2' of
    EQ -> compare bs1 bs2
    LT -> LT
    GT -> GT
compareNode (LNode (PlainLL _ _))            (LNode (PlainL _))               = GT
compareNode (LNode (PlainLL _ _))            (LNode _)                        = LT
compareNode (LNode (TypedL bsType1 bs1))         (LNode (TypedL bsType2 bs2))         =
  case compare bs1 bs2 of
    EQ -> compare bsType1 bsType2
    LT -> LT
    GT -> GT
compareNode (LNode (TypedL _ _))             (LNode _)                        = GT
compareNode (LNode _)                        _                                = GT

instance Hashable Node

-- |Two triples are equal iff their respective subjects, predicates, and objects
-- are equal.
instance Eq Triple where
  (Triple s1 p1 o1) == (Triple s2 p2 o2) = s1 == s2 && p1 == p2 && o1 == o2

-- |The ordering of triples is based on that of the subject, predicate, and object
-- of the triple, in that order.
instance Ord Triple where
  (Triple s1 p1 o1) `compare` (Triple s2 p2 o2) =
    case compareNode s1 s2 of
      EQ -> case compareNode p1 p2 of
              EQ -> compareNode o1 o2
              LT -> LT
              GT -> GT
      GT -> GT
      LT -> LT

-- |Two 'LValue' values are equal iff they are of the same type and all fields are
-- equal.
instance Eq LValue where
  (PlainL bs1)        ==  (PlainL bs2)        =  bs1 == bs2
  (PlainLL bs1 bs1')  ==  (PlainLL bs2 bs2')  =  bs1' == bs2'    &&  bs1 == bs2
  (TypedL bsType1 bs1)    ==  (TypedL bsType2 bs2)    =  bsType1 == bsType2 &&  bs1 == bs2
  _                   ==  _                   =  False

-- |Ordering of 'LValue' values is as follows: (PlainL _) < (PlainLL _ _)
-- < (TypedL _ _), and values of the same type are ordered by field values,
-- with '(PlainLL literalValue language)' being ordered by language first and
-- literal value second, and '(TypedL literalValue datatypeUri)' being ordered
-- by datatype first and literal value second.
instance Ord LValue where
  compare = compareLValue

{-# INLINE compareLValue #-}
compareLValue :: LValue -> LValue -> Ordering
compareLValue (PlainL bs1)       (PlainL bs2)       = compare bs1 bs2
compareLValue (PlainL _)         _                  = LT
compareLValue _                  (PlainL _)         = GT
compareLValue (PlainLL bs1 bs1') (PlainLL bs2 bs2') =
  case compare bs1' bs2' of
    EQ -> compare bs1 bs2
    GT -> GT
    LT -> LT
compareLValue (PlainLL _ _)       _                 = LT
compareLValue _                   (PlainLL _ _)     = GT
compareLValue (TypedL l1 t1) (TypedL l2 t2) =
  case compare t1 t2 of
    EQ -> compare l1 l2
    GT -> GT
    LT -> LT

instance Hashable LValue

-- String representations of the various data types; generally NTriples-like.

instance Show Triple where
  show (Triple s p o) =
    printf "Triple(%s,%s,%s)" (show s) (show p) (show o)

instance Show Node where
  show (UNode uri)                   = "UNode(" ++ show uri ++ ")"
  show (BNode  i)                    = "BNode(" ++ show i ++ ")"
  show (BNodeGen genId)              = "BNodeGen(" ++ show genId ++ ")"
  show (LNode lvalue)                = "LNode(" ++ show lvalue ++ ")"

instance Show LValue where
  show (PlainL lit)               = "PlainL(" ++ T.unpack lit ++ ")"
  show (PlainLL lit lang)         = "PlainLL(" ++ T.unpack lit ++ ", " ++ T.unpack lang ++ ")"
  show (TypedL lit dtype)         = "TypedL(" ++ T.unpack lit ++ "," ++ show dtype ++ ")"

------------------------
-- Prefix mappings

-- |Represents a namespace as either a prefix and uri, respectively,
--  or just a uri.
data Namespace = PrefixedNS  T.Text T.Text -- prefix and ns uri
               | PlainNS     T.Text            -- ns uri alone

instance Eq Namespace where
  (PrefixedNS _ u1) == (PrefixedNS _ u2)  = u1 == u2
  (PlainNS      u1) == (PlainNS      u2)  = u1 == u2
  (PrefixedNS _ u1) == (PlainNS      u2)  = u1 == u2
  (PlainNS      u1) == (PrefixedNS _ u2)  = u1 == u2

instance Show Namespace where
  show (PlainNS           uri)  =  T.unpack uri
  show (PrefixedNS prefix uri)  =  printf "(PrefixNS %s %s)" (T.unpack prefix) (T.unpack uri)

-- |An alias for a map from prefix to namespace URI.
newtype PrefixMappings   = PrefixMappings (Map T.Text T.Text)
  deriving (Eq, Ord,NFData)

instance Show PrefixMappings where
  -- This is really inefficient, but it's not used much so not what
  -- worth optimizing yet.
  show (PrefixMappings pmap) = printf "PrefixMappings [%s]" mappingsStr
    where showPM      = show . PrefixMapping
          mappingsStr = List.intercalate ", " (map showPM (Map.toList pmap))

-- |A mapping of a prefix to the URI for that prefix.
newtype PrefixMapping = PrefixMapping (T.Text, T.Text)
  deriving (Eq, Ord)
instance Show PrefixMapping where
  show (PrefixMapping (prefix, uri)) = printf "PrefixMapping (%s, %s)" (show prefix) (show uri)

-----------------
-- Miscellaneous helper functions used throughout the project

-- Resolve a prefix using the given prefix mappings and base URL. If the prefix is
-- empty, then the base URL will be used if there is a base URL and if the map
-- does not contain an entry for the empty prefix.
resolveQName :: Maybe BaseUrl -> T.Text -> PrefixMappings -> Maybe T.Text
resolveQName mbaseUrl prefix (PrefixMappings pms') =
  case (mbaseUrl, T.null prefix) of
    (Just (BaseUrl base), True)  ->  Just $ Map.findWithDefault base T.empty pms'
    (Nothing,             True)  ->  Nothing
    (_,                   _   )  ->  Map.lookup prefix pms'

{- alternative implementation from Text.RDF.RDF4H.ParserUtils
--
-- Resolve a prefix using the given prefix mappings and base URL. If the prefix is
-- empty, then the base URL will be used if there is a base URL and if the map
-- does not contain an entry for the empty prefix.
resolveQName :: Maybe BaseUrl -> T.Text -> PrefixMappings -> T.Text
resolveQName mbaseUrl prefix (PrefixMappings pms') =
  case (mbaseUrl, T.null prefix) of
    (Just (BaseUrl base), True)  ->  Map.findWithDefault base T.empty pms'
    (Nothing,             True)  ->  err1
    (_,                   _   )  ->  Map.findWithDefault err2 prefix pms'
  where
    err1 = error  "Cannot resolve empty QName prefix to a Base URL."
    err2 = error ("Cannot resolve QName prefix: " ++ T.unpack prefix)
-}

-- Resolve a URL fragment found on the right side of a prefix mapping
-- by converting it to an absolute URL if possible.
absolutizeUrl :: Maybe BaseUrl -> Maybe T.Text -> T.Text -> T.Text
absolutizeUrl mbUrl mdUrl urlFrag =
  if isAbsoluteUri urlFrag then urlFrag else
    (case (mbUrl, mdUrl) of
         (Nothing, Nothing) -> urlFrag
         (Just (BaseUrl bUrl), Nothing) -> bUrl `T.append` urlFrag
         (Nothing, Just dUrl) -> if isHash urlFrag then
                                     dUrl `T.append` urlFrag else urlFrag
         (Just (BaseUrl bUrl), Just dUrl) -> (if isHash urlFrag then dUrl
                                                  else bUrl)
                                                 `T.append` urlFrag)
  where
    isHash bs' = bs' == "#"

{- alternative implementation from Text.RDF.RDF4H.ParserUtils
--
-- Resolve a URL fragment found on the right side of a prefix mapping by converting it to an absolute URL if possible.
absolutizeUrl :: Maybe BaseUrl -> Maybe T.Text -> T.Text -> T.Text
absolutizeUrl mbUrl mdUrl urlFrag =
  if isAbsoluteUri urlFrag then urlFrag else
    (case (mbUrl, mdUrl) of
         (Nothing, Nothing) -> urlFrag
         (Just (BaseUrl bUrl), Nothing) -> bUrl `T.append` urlFrag
         (Nothing, Just dUrl) -> if isHash urlFrag then
                                     dUrl `T.append` urlFrag else urlFrag
         (Just (BaseUrl bUrl), Just dUrl) -> (if isHash urlFrag then dUrl
                                                  else bUrl)
                                                 `T.append` urlFrag)
  where
    isHash bs' = T.length bs' == 1 && T.head bs' == '#'
-}

{-# INLINE mkAbsoluteUrl #-}
-- Make an absolute URL by returning as is if already an absolute URL and otherwise
-- appending the URL to the given base URL.
mkAbsoluteUrl :: T.Text -> T.Text -> T.Text
mkAbsoluteUrl base url =
  if isAbsoluteUri url then url else base `T.append` url


-----------------
-- Internal canonicalize functions, don't export

-- |Canonicalize the given 'T.Text' value using the 'T.Text'
-- as the datatype URI.
{-# NOINLINE canonicalize #-}
canonicalize :: T.Text -> T.Text -> T.Text
canonicalize typeTxt litValue =
  case Map.lookup typeTxt canonicalizerTable of
    Nothing   ->  litValue
    Just fn   ->  fn litValue

-- A table of mappings from a 'T.Text' URI
-- to a function that canonicalizes a T.Text
-- assumed to be of that type.
{-# NOINLINE canonicalizerTable #-}
canonicalizerTable :: Map T.Text (T.Text -> T.Text)
canonicalizerTable =
  Map.fromList [(integerUri, _integerStr), (doubleUri, _doubleStr),
                (decimalUri, _decimalStr)]
  where
    integerUri =  "http://www.w3.org/2001/XMLSchema#integer"
    decimalUri =  "http://www.w3.org/2001/XMLSchema#decimal"
    doubleUri  =  "http://www.w3.org/2001/XMLSchema#double"

_integerStr, _decimalStr, _doubleStr :: T.Text -> T.Text
_integerStr = T.dropWhile (== '0')

-- exponent: [eE] ('-' | '+')? [0-9]+
-- ('-' | '+') ? ( [0-9]+ '.' [0-9]* exponent | '.' ([0-9])+ exponent | ([0-9])+ exponent )
_doubleStr s = T.pack $ show (read $ T.unpack s :: Double)

-- ('-' | '+')? ( [0-9]+ '.' [0-9]* | '.' ([0-9])+ | ([0-9])+ )
_decimalStr s =     -- haskell double parser doesn't handle '1.'..,
  case T.last s of   -- so we add a zero if that's the case and then parse
    '.' -> f (s `T.snoc` '0')
    _   -> f s
  where f s' = T.pack $ show (read $ T.unpack s' :: Double)
