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
  rdf, rdfs, dc, dct, owl, xsd, skos, foaf, ex, ex2,
  standard_ns_mappings, ns_mappings
) where

import qualified Data.Text.Lazy as T
import Data.RDF.Types
import qualified Data.Map as Map

standard_namespaces :: [Namespace]
standard_namespaces = [rdf, rdfs, dc, dct, owl, xsd, skos, foaf, ex, ex2]

-- |The set of common predefined namespaces as a 'PrefixMappings' value.
standard_ns_mappings  :: PrefixMappings
standard_ns_mappings  =  ns_mappings standard_namespaces

-- |Takes a list of 'Namespace's and returns 'PrefixMappings'.
ns_mappings :: [Namespace] -> PrefixMappings
ns_mappings ns =  PrefixMappings $ Map.fromList $ 
                     map (\(PrefixedNS pre uri) -> (pre, uri)) ns

-- |The RDF namespace.
rdf  :: Namespace
rdf   =   mkPrefixedNS' "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

-- |The RDF Schema namespace.
rdfs :: Namespace
rdfs  =   mkPrefixedNS'  "rdfs"  "http://www.w3.org/2000/01/rdf-schema#"

-- |The Dublin Core namespace.
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


-- |Perform a left-biased merge of the two sets of prefix mappings.
mergePrefixMappings :: PrefixMappings -> PrefixMappings -> PrefixMappings
mergePrefixMappings (PrefixMappings p1s) (PrefixMappings p2s) = 
  PrefixMappings $ Map.union p1s p2s

-- |View the prefix mappings as a list of key-value pairs. The PM in
-- in the name is to reduce name clashes if used without qualifying.
toPMList :: PrefixMappings -> [(T.Text, T.Text)]
toPMList (PrefixMappings m) = Map.toList m

-- |Make a URI consisting of the given namespace and the given localname.
mkUri :: Namespace -> T.Text -> T.Text
mkUri ns local = uriOf ns `T.append` local


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

-- |Determine the URI of the given namespace.
uriOf     ::  Namespace -> T.Text
uriOf    (PlainNS      uri)  = uri
uriOf    (PrefixedNS _ uri)  = uri

-- |Determine the prefix of the given namespace, if it has one.
prefixOf  ::  Namespace -> Maybe T.Text
prefixOf (PlainNS      _)    = Nothing
prefixOf (PrefixedNS p _)    = Just p
