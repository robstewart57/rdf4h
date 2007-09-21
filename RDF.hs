module RDF (Graph, emptyGraph, graph,
            Triple(Triple), 
            Subject(SubjectUriRef, SubjectBNode), 
            Predicate(PredicateUriRef),
            Object(ObjectUriRef, ObjectBNode, ObjectLiteral),
            UriRef(UriRef),
            NodeId(NodeId),
            Literal(PlainLiteral, TypedLiteral),
            Language(Language))
where

import Text.Printf

{-
This module defines the core RDF types and functions that are used
by other modules. We don't export any of the constructors, so there
are functions for creating a new value of each of the various types.
-}

-- |A graph is just a list of triples for the moment, but we don't export
-- the constructor so that we may easily change it later.
newtype Graph = Graph [Triple]

-- |Create a new empty RDF graph.
emptyGraph :: Graph
emptyGraph = Graph []

-- |Create a new graph containing the given triples.
graph :: [Triple] -> Graph
graph = Graph 
 
-- |A triple represents a single RDF statement, and consists of a Subject,
-- a Predicate, and an Object.
data Triple = Triple Subject Predicate Object
       deriving (Eq)
instance Show Triple where
  show (Triple subj pred obj) = 
    printf "%s %s %s ." (show subj) (show pred) (show obj)

-- |Represents the type of a graph node.
data NodeType = UriRefType | BNodeType | LiteralNodeType
  deriving (Eq, Ord, Show)

class (Eq n) => Node n where
  nodeType :: n -> NodeType
  nodeUri  :: n -> Maybe UriRef
  nodeId   :: n -> Maybe NodeId

-- |The subject of a triple is either a URIRef or a  NodeId (for bnodes).
data Subject = SubjectUriRef UriRef | SubjectBNode NodeId
       deriving (Eq, Ord)

instance Show Subject where
  show (SubjectUriRef (UriRef uri)) = show uri
  show (SubjectBNode  (NodeId id))  = show id

-- |Instance declaration of Subject as a Node.
instance Node Subject where
  nodeType   (SubjectUriRef _)     = UriRefType
  nodeType   (SubjectBNode  _)     = BNodeType
  nodeUri    (SubjectUriRef uri)   = Just uri
  nodeUri    (SubjectBNode  _)     = Nothing
  nodeId     (SubjectUriRef _)     = Nothing
  nodeId     (SubjectBNode  id)    = Just id

-- |A predicate may only be a URI reference.
newtype Predicate = PredicateUriRef UriRef
  deriving (Eq, Ord)

instance Show Predicate where
  show (PredicateUriRef uri) = show uri
                               
-- |Instance declaration of Predicate as a graph Node.
instance Node Predicate where
  nodeType   (PredicateUriRef _)   = UriRefType
  nodeUri    (PredicateUriRef uri) = Just uri
  nodeId     (PredicateUriRef _)   = Nothing

-- |The object may be either a literal (a simple value) or may be a resource, 
-- and if a resource, may be a resource identified by URIRef or a blank node
-- identified by NodeId.
data Object = ObjectUriRef UriRef | ObjectBNode NodeId | ObjectLiteral Literal
       deriving (Eq)

instance Show Object where
  show (ObjectUriRef uri)   = show uri
  show (ObjectBNode id)     = show id
  show (ObjectLiteral lit)  = show lit

-- |Instance declaration of Object as a graph Node.
instance Node Object where
  nodeType   (ObjectUriRef _)      = UriRefType
  nodeType   (ObjectBNode  _)      = BNodeType
  nodeType   (ObjectLiteral _)  = LiteralNodeType
  nodeUri    (ObjectUriRef uri)    = Just uri
  nodeUri    (ObjectBNode  _)      = Nothing
  nodeUri    (ObjectLiteral _)  = Nothing
  nodeId     (ObjectUriRef _)      = Nothing
  nodeId     (ObjectBNode id)      = Just id
  nodeId     (ObjectLiteral _)  = Nothing

-- |A node id is an identifier for a blank node.
newtype NodeId = NodeId String
  deriving (Eq, Ord)

instance Show NodeId where
  show (NodeId id) = id

-- |A URIRef is the identifying URI of a resource.
newtype UriRef = UriRef String
  deriving (Eq, Ord)

instance Show UriRef where
  show (UriRef uri) = "<" ++ uri ++ ">"

-- |Language is simply a string. E.g., "en".
newtype Language = Language String
  deriving (Eq, Ord)
instance Show Language where
  show (Language str) = str

-- |A literal is either a language literal (with optional specifier of the
-- actual language of the string value) or a datatype literal (with 
-- specified XSD datatype).
data Literal = PlainLiteral String (Maybe Language) | 
               TypedLiteral String UriRef
  deriving (Eq, Ord)
instance Show Literal where
  show (PlainLiteral str (Just (Language lang)))  = printf "\"%s\"@\"%s\"" str lang
  show (PlainLiteral str Nothing)                 = printf "\"%s\"" str
  show (TypedLiteral str (UriRef uri))            = printf "\"%s\"^^<%s>" str uri


