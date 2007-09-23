module Namespace(Namespace, makePlainNS, makePrefixedNS,
                 prefixOf, uriOf,
                 rdf, rdfs, dc, owl, xsd, ex, ex2)
where

-- Standard namespaces defined here for convenience:

rdf   =   makePrefixedNS  "rdf"   "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
rdfs  =   makePrefixedNS  "rdfs"  "http://www.w3.org/2000/01/rdf-schema#"
dc    =   makePrefixedNS  "dc"    "http://purl.org/dc/elements/1.1/"
owl   =   makePrefixedNS  "owl"   "http://www.w3.org/2002/07/owl#"
xsd   =   makePrefixedNS  "xsd"   "http://www.w3.org/2001/XMLSchema#"

ex    =   makePrefixedNS  "ex"    "http://www.example.org/"
ex2   =   makePrefixedNS  "ex2"   "http://www2.example.org/"

-- |Represents a namespace as either a prefix and uri or just a uri.
data Namespace = PrefixedNS  String String -- prefix and ns uri
               | PlainNS     String        -- ns uri alone

makePlainNS       =  PlainNS
makePrefixedNS    =  PrefixedNS

instance Eq Namespace where
  (PrefixedNS _ u1) == (PrefixedNS _ u2)  = u1 == u2
  (PlainNS      u1) == (PlainNS      u2)  = u1 == u2
  (PrefixedNS _ u1) == (PlainNS      u2)  = u1 == u2
  (PlainNS      u1) == (PrefixedNS _ u2)  = u1 == u2

instance Show Namespace where
  show (PlainNS           uri)  =  uri
  show (PrefixedNS prefix uri)  =  show (prefix, uri)

uriOf    (PlainNS      uri)  = uri
uriOf    (PrefixedNS _ uri)  = uri
prefixOf (PlainNS      _)    = Nothing
prefixOf (PrefixedNS p _)    = Just p
