-- |Defines types and utility functions related to namespaces, and
-- some predefined values for commonly used namespaces, such as
-- rdf, xsd, dublin core, etc.

module Data.RDF.Namespace(
  -- * Namespace types and functions
  Namespace(..), mkPlainNS, mkPrefixedNS, mkPrefixedNS',
  PrefixMapping(PrefixMapping), PrefixMappings(PrefixMappings), toPMList,
  mergePrefixMappings,
  mkUri,
  prefixOf, uriOf,
  -- * Predefined namespace values
  rdf, rdfs, dc, dct, owl, xsd, skos, foaf, ex, ex2, standard_ns_mappings
)
where

import Text.Printf
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as T

standard_namespaces :: [Namespace]
standard_namespaces = [rdf, rdfs, dc, dct, owl, xsd, skos, foaf, ex, ex2]

-- |The set of common predefined namespaces as a 'PrefixMappings' value.
standard_ns_mappings  :: PrefixMappings
standard_ns_mappings  =  PrefixMappings $ Map.fromList $ 
                         map (\(PrefixedNS pre uri) -> (pre, uri)) standard_namespaces

-- |The RDF namespace.
rdf  :: Namespace
rdf   =   mkPrefixedNS' "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

-- |The RDF Schema namespace.
rdfs :: Namespace
rdfs  =   mkPrefixedNS'  "rdfs"  "http://www.w3.org/2000/01/rdf-schema#"

-- |The Dublic Core namespace.
dc   :: Namespace
dc    =   mkPrefixedNS'  "dc"    "http://purl.org/dc/elements/1.1/"

-- |The Dublin Core terms namespace.
dct  :: Namespace
dct   =   mkPrefixedNS'  "dct"    "http://purl.org/dc/terms/"

-- |The OWL namespace.
owl  :: Namespace
owl   =   mkPrefixedNS'  "owl"   "http://www.w3.org/2002/07/owl#"

-- |The XML Schema namespace.
xsd  :: Namespace
xsd   =   mkPrefixedNS'  "xsd"   "http://www.w3.org/2001/XMLSchema#"

-- |The SKOS namespace.
skos :: Namespace
skos  =   mkPrefixedNS'  "skos"  "http://www.w3.org/2004/02/skos/core#"

-- |The friend of a friend namespace.
foaf :: Namespace
foaf  =   mkPrefixedNS'  "foaf"  "http://xmlns.com/foaf/0.1/"

-- |Example namespace #1.
ex   :: Namespace
ex    =   mkPrefixedNS'  "ex"    "http://www.example.org/"

-- |Example namespace #2.
ex2  :: Namespace
ex2   =   mkPrefixedNS'  "ex2"   "http://www2.example.org/"

-- |An alias for a map from prefix to namespace URI.
newtype PrefixMappings   = PrefixMappings (Map T.Text T.Text)
  deriving (Eq, Ord)
instance Show PrefixMappings where
  -- This is really inefficient, but it's not used much so not what
  -- worth optimizing yet.
  show (PrefixMappings pmap) = printf "PrefixMappings [%s]" mappingsStr
    where showPM      = show . PrefixMapping
          mappingsStr = List.intercalate ", " (map showPM (Map.toList pmap))

-- |Perform a left-biased merge of the two sets of prefix mappings.
mergePrefixMappings :: PrefixMappings -> PrefixMappings -> PrefixMappings
mergePrefixMappings (PrefixMappings p1s) (PrefixMappings p2s) = 
  PrefixMappings $ Map.union p1s p2s

-- |View the prefix mappings as a list of key-value pairs. The PM in
-- in the name is to reduce name clashes if used without qualifying.
toPMList :: PrefixMappings -> [(T.Text, T.Text)]
toPMList (PrefixMappings m) = Map.toList m

-- |A mapping of a prefix to the URI for that prefix.
newtype PrefixMapping = PrefixMapping (T.Text, T.Text)
  deriving (Eq, Ord)
instance Show PrefixMapping where
  show (PrefixMapping (prefix, uri)) = printf "PrefixMapping (%s, %s)" (show prefix) (show uri)

-- |Make a URI consisting of the given namespace and the given localname.
mkUri :: Namespace -> T.Text -> T.Text
mkUri ns local = uriOf ns `T.append` local

-- |Represents a namespace as either a prefix and uri, respectively,
--  or just a uri.
data Namespace = PrefixedNS  T.Text T.Text -- prefix and ns uri
               | PlainNS     T.Text            -- ns uri alone

-- |Make a namespace for the given URI reference.
mkPlainNS     ::  T.Text -> Namespace
mkPlainNS       =  PlainNS

-- |Make a namespace having the given prefix for the given URI reference,
-- respectively.
mkPrefixedNS  :: T.Text -> T.Text -> Namespace
mkPrefixedNS    =  PrefixedNS

-- |Make a namespace having the given prefix for the given URI reference,
-- respectively, using strings which will be converted to bytestrings
-- automatically.
mkPrefixedNS' :: String -> String -> Namespace
mkPrefixedNS' s1 s2 = mkPrefixedNS (T.pack s1) (T.pack s2)

instance Eq Namespace where
  (PrefixedNS _ u1) == (PrefixedNS _ u2)  = u1 == u2
  (PlainNS      u1) == (PlainNS      u2)  = u1 == u2
  (PrefixedNS _ u1) == (PlainNS      u2)  = u1 == u2
  (PlainNS      u1) == (PrefixedNS _ u2)  = u1 == u2

instance Show Namespace where
  show (PlainNS           uri)  =  T.unpack uri
  show (PrefixedNS prefix uri)  =  printf "(PrefixNS %s %s)" (T.unpack prefix) (T.unpack uri)

-- |Determine the URI of the given namespace.
uriOf     ::  Namespace -> T.Text
uriOf    (PlainNS      uri)  = uri
uriOf    (PrefixedNS _ uri)  = uri

-- |Determine the prefix of the given namespace, if it has one.
prefixOf  ::  Namespace -> Maybe T.Text
prefixOf (PlainNS      _)    = Nothing
prefixOf (PrefixedNS p _)    = Just p
