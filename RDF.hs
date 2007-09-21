module RDF (RDFGraph,
            Triple(Triple), 
            Subject(SubjectURI, SubjectID), 
            Predicate(PredicateURI), 
            Object(ObjectURI, ObjectID, ObjectLiteral),
            Literal(LanguageString, DatatypeString),
            Language)
where

{-
This module defines the core RDF types and functions that are used
by other modules. We don't export any of the constructors, so there
are functions for creating a new value of each of the various types.
-}

newtype RDFGraph = RDFGraph [Triple]

newRDFGraph :: [Triple] -> RDFGraph
newRDFGraph = RDFGraph 

-- |A triple represents a single RDF statement, and consists of a Subject,
-- a Predicate, and an Object.
data Triple = Triple Subject Predicate Object
       deriving (Eq, Show)

-- |The subject is  a URIRef (the identifying URI of a resource) or a 
-- NodeId (for bnodes). URIRef and NodeId are simply strings.
data Subject = SubjectURI URIRef | SubjectID NodeID
       deriving (Eq, Show)
type URIRef = String
type NodeID = String

-- |The predicate consists of a URIRef (the identifying URI of the predicate
-- resource). And URIRef, again, is simply a string.
newtype Predicate = PredicateURI URIRef
          deriving (Eq, Show)

-- |The object may be either a literal (a simple value) or may be a resource, 
-- and if a resource, may be a resource identified by URIRef or a blank node
-- identified by NodeId.
data Object = ObjectURI URIRef | ObjectID NodeID | ObjectLiteral Literal
       deriving (Eq, Show)

-- |Language is simply a string. E.g., "en".
type Language = String

-- |A literal is either a language literal (with optional specifier of the actual
-- language of the string value) or a datatype literal (with specified XSD datatype).
data Literal = LanguageString String (Maybe Language) | DatatypeString String URIRef
       deriving (Eq, Show)

