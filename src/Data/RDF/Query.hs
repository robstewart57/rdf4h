{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.RDF.Query
  ( -- * Query functions
    equalSubjects,
    equalPredicates,
    equalObjects,
    subjectOf,
    predicateOf,
    objectOf,
    isEmpty,
    rdfContainsNode,
    tripleContainsNode,
    subjectsWithPredicate,
    objectsOfPredicate,
    uordered,

    -- * RDF graph functions
    isIsomorphic,
    expandTriples,
    fromEither,

    -- * expansion functions
    expandTriple,
    expandNode,
    expandURI,

    -- * absolutizing functions
    absolutizeTriple,
    absolutizeNode,
    absolutizeNodeUnsafe,
    QueryException (..),
  )
where

import Control.Applicative ((<|>))
import Control.Exception
import Data.List
import Data.Maybe (fromMaybe)
import Data.RDF.IRI
import qualified Data.RDF.Namespace as NS
import Data.RDF.Types
#if MIN_VERSION_base(4,9,0)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#else
#endif
#else
#endif
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (pred)

-- | Answer the subject node of the triple.
{-# INLINE subjectOf #-}
subjectOf :: Triple -> Node
subjectOf (Triple s _ _) = s

-- | Answer the predicate node of the triple.
{-# INLINE predicateOf #-}
predicateOf :: Triple -> Node
predicateOf (Triple _ p _) = p

-- | Answer the object node of the triple.
{-# INLINE objectOf #-}
objectOf :: Triple -> Node
objectOf (Triple _ _ o) = o

-- | Answer if rdf contains node.
rdfContainsNode :: (Rdf a) => RDF a -> Node -> Bool
rdfContainsNode rdf node = any (tripleContainsNode node) (triplesOf rdf)

-- | Answer if triple contains node.
--  Note that it doesn't perform namespace expansion!
tripleContainsNode :: Node -> Triple -> Bool
{-# INLINE tripleContainsNode #-}
tripleContainsNode node (Triple s p o) = s == node || p == node || o == node

-- | Determine whether two triples have equal subjects.
--  Note that it doesn't perform namespace expansion!
equalSubjects :: Triple -> Triple -> Bool
equalSubjects (Triple s1 _ _) (Triple s2 _ _) = s1 == s2

-- | Determine whether two triples have equal predicates.
--  Note that it doesn't perform namespace expansion!
equalPredicates :: Triple -> Triple -> Bool
equalPredicates (Triple _ p1 _) (Triple _ p2 _) = p1 == p2

-- | Determine whether two triples have equal objects.
--  Note that it doesn't perform namespace expansion!
equalObjects :: Triple -> Triple -> Bool
equalObjects (Triple _ _ o1) (Triple _ _ o2) = o1 == o2

-- | Determines whether the 'RDF' contains zero triples.
isEmpty :: Rdf a => RDF a -> Bool
isEmpty = null . triplesOf

-- | Lists of all subjects of triples with the given predicate.
subjectsWithPredicate :: Rdf a => RDF a -> Predicate -> [Subject]
subjectsWithPredicate rdf pred = subjectOf <$> query rdf Nothing (Just pred) Nothing

-- | Lists of all objects of triples with the given predicate.
objectsOfPredicate :: Rdf a => RDF a -> Predicate -> [Object]
objectsOfPredicate rdf pred = objectOf <$> query rdf Nothing (Just pred) Nothing

-- | Convert a parse result into an RDF if it was successful
--  and error and terminate if not.
fromEither :: Either ParseFailure (RDF a) -> RDF a
fromEither (Left err) = error (show err)
fromEither (Right rdf) = rdf

-- | Convert a list of triples into a sorted list of unique triples.
uordered :: Triples -> Triples
uordered = sort . nub

-- graphFromEdges :: Ord key => [(node, key, [key])] -> (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)

-- | This determines if two RDF representations are equal regardless
--  of blank node names, triple order and prefixes. In math terms,
--  this is the \simeq latex operator, or ~= . Unsafe because it
--  assumes IRI resolution will succeed, may throw an
--  'IRIResolutionException` exception.
isIsomorphic :: (Rdf a, Rdf b) => RDF a -> RDF b -> Bool
isIsomorphic g1 g2 = and $ zipWith compareTripleUnlessBlank (normalize g1) (normalize g2)
  where
    compareNodeUnlessBlank :: Node -> Node -> Bool
    compareNodeUnlessBlank (BNode _) (BNode _) = True
    compareNodeUnlessBlank (UNode n1) (UNode n2) = n1 == n2
    compareNodeUnlessBlank (BNodeGen i1) (BNodeGen i2) = i1 == i2
    compareNodeUnlessBlank (LNode l1) (LNode l2) = l1 == l2
    compareNodeUnlessBlank (BNodeGen _) (BNode _) = True
    compareNodeUnlessBlank (BNode _) (BNodeGen _) = True
    compareNodeUnlessBlank _ _ = False -- isn't this exhaustive already?
    compareTripleUnlessBlank :: Triple -> Triple -> Bool
    compareTripleUnlessBlank (Triple s1 p1 o1) (Triple s2 p2 o2) =
      compareNodeUnlessBlank s1 s2
        && compareNodeUnlessBlank p1 p2
        && compareNodeUnlessBlank o1 o2
    normalize :: (Rdf a) => RDF a -> Triples
    normalize = sort . nub . expandTriples

-- | Expand the triples in a graph with the prefix map and base URL
-- for that graph. Unsafe because it assumes IRI resolution will
-- succeed, may throw an 'IRIResolutionException` exception.
expandTriples :: (Rdf a) => RDF a -> Triples
expandTriples rdf = normalize <$> triplesOf rdf
  where
    normalize = absolutizeTriple (baseUrl rdf) . expandTriple (prefixMappings rdf)

-- | Expand the triple with the prefix map.
expandTriple :: PrefixMappings -> Triple -> Triple
expandTriple pms (Triple s p o) = triple (expandNode pms s) (expandNode pms p) (expandNode pms o)

-- | Expand the node with the prefix map.
--  Only UNodes are expanded, other kinds of nodes are returned as-is.
expandNode :: PrefixMappings -> Node -> Node
expandNode pms (UNode u) = unode $ expandURI pms u
expandNode _ n = n

-- | Expand the URI with the prefix map.
--  Also expands "a" to "http://www.w3.org/1999/02/22-rdf-syntax-ns#type".
expandURI :: PrefixMappings -> Text -> Text
expandURI _ "a" = NS.mkUri NS.rdf "type"
expandURI pms iri = fromMaybe iri $ foldl' f Nothing (NS.toPMList pms)
  where
    f :: Maybe Text -> (Text, Text) -> Maybe Text
    f x (p, u) = x <|> (T.append u <$> T.stripPrefix (T.append p ":") iri)

-- | Prefixes relative URIs in the triple with BaseUrl. Unsafe because
-- it assumes IRI resolution will succeed, may throw an
-- 'IRIResolutionException` exception.
absolutizeTriple :: Maybe BaseUrl -> Triple -> Triple
absolutizeTriple base (Triple s p o) = triple (absolutizeNodeUnsafe base s) (absolutizeNodeUnsafe base p) (absolutizeNodeUnsafe base o)

-- | Prepends BaseUrl to UNodes with relative URIs.
absolutizeNode :: Maybe BaseUrl -> Node -> Either String Node
absolutizeNode (Just (BaseUrl b)) (UNode u) =
  case resolveIRI b u of
    Left iriErr -> Left iriErr
    Right t -> Right (unode t)
absolutizeNode _ n = Right n

data QueryException
  = IRIResolutionException String
  deriving (Show)

instance Exception QueryException

-- | Prepends BaseUrl to UNodes with relative URIs. Unsafe because it
-- assumes IRI resolution will succeed, may throw an
-- 'IRIResolutionException` exception.
absolutizeNodeUnsafe :: Maybe BaseUrl -> Node -> Node
absolutizeNodeUnsafe (Just (BaseUrl b)) (UNode u) =
  case resolveIRI b u of
    Left iriErr -> throw (IRIResolutionException iriErr)
    Right t -> unode t
absolutizeNodeUnsafe _ n = n
