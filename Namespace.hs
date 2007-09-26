module Namespace(Namespace, makePlainNS, makePrefixedNS,
                 prefixOf, uriOf,
                 rdf, rdfs, dc, owl, xsd, skos, foaf, ex, ex2)
where

-- Standard namespaces defined here for convenience:
-- |The RDF namespace.
rdf  :: Namespace
rdf   =   makePrefixedNS  "rdf"   "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
-- |The RDF Schema namespace.
rdfs :: Namespace
rdfs  =   makePrefixedNS  "rdfs"  "http://www.w3.org/2000/01/rdf-schema#"
-- |The Dublic Core namespace.
dc   :: Namespace
dc    =   makePrefixedNS  "dc"    "http://purl.org/dc/elements/1.1/"
-- |The OWL namespace.
owl  :: Namespace
owl   =   makePrefixedNS  "owl"   "http://www.w3.org/2002/07/owl#"
-- |The XML Schema namespace.
xsd  :: Namespace
xsd   =   makePrefixedNS  "xsd"   "http://www.w3.org/2001/XMLSchema#"
-- |The SKOS namespace.
skos :: Namespace
skos  =   makePrefixedNs  "skos"  "http://www.w3.org/2004/02/skos/core#"
-- |The friend of a friend namespace.
foaf :: Namespace
foaf  =   makePrefixedNs  "foaf"  "http://xmlns.com/foaf/0.1/"
-- |Example namespace #1.
ex   :: Namespace
ex    =   makePrefixedNS  "ex"    "http://www.example.org/"
-- |Example namespace #2.
ex2  :: Namespace
ex2   =   makePrefixedNS  "ex2"   "http://www2.example.org/"

-- |Represents a namespace as either a prefix and uri, respectively,
--  or just a uri.
data Namespace = PrefixedNS  String String -- prefix and ns uri
               | PlainNS     String        -- ns uri alone

-- |Make a namespace for the given URI reference.
makePlainNS     ::  String -> Namespace
makePlainNS       =  PlainNS
-- |Make a namespace having the given prefix for the given URI reference,
-- respectively.
makePrefixedNS  :: String -> String -> Namespace
makePrefixedNS    =  PrefixedNS

instance Eq Namespace where
  (PrefixedNS _ u1) == (PrefixedNS _ u2)  = u1 == u2
  (PlainNS      u1) == (PlainNS      u2)  = u1 == u2
  (PrefixedNS _ u1) == (PlainNS      u2)  = u1 == u2
  (PlainNS      u1) == (PrefixedNS _ u2)  = u1 == u2

instance Show Namespace where
  show (PlainNS           uri)  =  uri
  show (PrefixedNS prefix uri)  =  show (prefix, uri)

-- |Determine the URI of the given namespace.
uriOf     ::  Namespace -> String
uriOf    (PlainNS      uri)  = uri
uriOf    (PrefixedNS _ uri)  = uri
-- |Determine the prefix of the given namespace, if it has one.
prefixOf  ::  Namespace -> Maybe String
prefixOf (PlainNS      _)    = Nothing
prefixOf (PrefixedNS p _)    = Just p
