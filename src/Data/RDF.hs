-- |The Core module provides the fundamental types,
-- type classes, and functions of the library.
--

-- TODO: update writeT to writeTriple, etc.

module Data.RDF (

  -- * Export types
  module Data.RDF.Types,

  -- * RDF graph functions
  isIsomorphic,

  -- * Constructor functions
  plainL,plainLL,typedL,
  unode,bnode,lnode,triple,
  
  -- * Query functions
  isUNode,isLNode,isBNode,sortTriples,
  equalSubjects, equalPredicates, equalObjects,
  subjectOf, predicateOf, objectOf, isEmpty,
  rdfContainsNode, tripleContainsNode,
  listSubjectsWithPredicate, listObjectsOfPredicate,
  
  -- * Utility functions
  toPMList, s2t,t2s, fromEither, removeDupes

)
where

import Data.RDF.Namespace
import Data.RDF.Utils ( s2t, t2s, canonicalize )
import Data.RDF.Types
import qualified Data.Text as T
import Data.List
import Prelude hiding (pred)

----------------------
-- Constructor functions

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


-- |A smart constructor function for 'Triple' that verifies the node arguments
-- are of the correct type and creates the new 'Triple' if so or calls 'error'.
-- /subj/ must be a 'UNode' or 'BNode', and /pred/ must be a 'UNode'.
triple :: Subject -> Predicate -> Object -> Triple
triple subj pred obj
  | isLNode subj     =  error $ "subject must be UNode or BNode: "     ++ show subj
  | isLNode pred     =  error $ "predicate must be UNode, not LNode: " ++ show pred
  | isBNode pred     =  error $ "predicate must be UNode, not BNode: " ++ show pred
  | otherwise        =  Triple subj pred obj

-- |Answer the given list of triples in sorted order.
sortTriples :: Triples -> Triples
sortTriples = sort

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


-- End of constructor functions
---------------


-- |Answer the subject node of the triple.
{-# INLINE subjectOf #-}
subjectOf :: Triple -> Node
subjectOf (Triple s _ _) = s

-- |Answer the predicate node of the triple.
{-# INLINE predicateOf #-}
predicateOf :: Triple -> Node
predicateOf (Triple _ p _) = p

-- |Answer the object node of the triple.
{-# INLINE objectOf #-}
objectOf :: Triple -> Node
objectOf (Triple _ _ o)   = o

-- |Answer if rdf contains node.
rdfContainsNode :: forall rdf. (RDF rdf) => rdf -> Node -> Bool
rdfContainsNode rdf node =
  let ts = triplesOf rdf
      xs = map (tripleContainsNode node) ts
  in elem True xs

-- |Answer if triple contains node.
tripleContainsNode :: Node -> Triple -> Bool
{-# INLINE tripleContainsNode #-}
tripleContainsNode node t = 
 subjectOf t == node || predicateOf t == node || objectOf t == node


-- |Determine whether two triples have equal subjects.
equalSubjects :: Triple -> Triple -> Bool
equalSubjects (Triple s1 _ _) (Triple s2 _ _) = s1 == s2

-- |Determine whether two triples have equal predicates.
equalPredicates :: Triple -> Triple -> Bool
equalPredicates (Triple _ p1 _) (Triple _ p2 _) = p1 == p2

-- |Determine whether two triples have equal objects.
equalObjects :: Triple -> Triple -> Bool
equalObjects (Triple _ _ o1) (Triple _ _ o2) = o1 == o2

-- |Determines whether the 'RDF' contains zero triples.
isEmpty :: RDF rdf => rdf -> Bool
isEmpty rdf =
  let ts = triplesOf rdf
  in null ts

-- |Lists of all subjects of triples with the given predicate.
listSubjectsWithPredicate :: RDF rdf => rdf -> Predicate -> [Subject]
listSubjectsWithPredicate rdf pred =
  listNodesWithPredicate rdf pred subjectOf

-- |Lists of all objects of triples with the given predicate.
listObjectsOfPredicate :: RDF rdf => rdf -> Predicate -> [Object]
listObjectsOfPredicate rdf pred =
  listNodesWithPredicate rdf pred objectOf

listNodesWithPredicate :: RDF rdf => rdf -> Predicate -> (Triple -> Node) -> [Node]
listNodesWithPredicate rdf pred f =
  let ts = triplesOf rdf
      xs = filter (\t -> predicateOf t == pred) ts
  in map f xs


-- |Convert a parse result into an RDF if it was successful
-- and error and terminate if not.
fromEither :: RDF rdf => Either ParseFailure rdf -> rdf
fromEither res =
  case res of
    (Left err) -> error (show err)
    (Right rdf) -> rdf

-- |Remove duplicate triples, returning unique triples. This 
-- function may return the triples in a different order than 
-- given.
removeDupes :: Triples -> Triples
removeDupes =  map head . group . sort

-- |This determines if two RDF representations are equal regardless of blank
-- node names, triple order and prefixes.  In math terms, this is the \simeq
-- latex operator, or ~=
isIsomorphic :: forall rdf1 rdf2. (RDF rdf1, RDF rdf2) => rdf1 -> rdf2 -> Bool
isIsomorphic g1 g2 = normalize g1 == normalize g2
  where normalize :: forall rdf. (RDF rdf) => rdf -> Triples
        normalize = sort . nub . expandTriples

-- |Expand the triples in a graph with the prefix map and base URL for that
-- graph.
expandTriples :: (RDF rdf) => rdf -> Triples
expandTriples rdf = expandTriples' [] (baseUrl rdf) (prefixMappings rdf) (triplesOf rdf)

expandTriples' :: Triples -> Maybe BaseUrl -> PrefixMappings -> Triples -> Triples
expandTriples' acc _ _ [] = acc
expandTriples' acc baseUrl prefixMappings (t:rest) = expandTriples' (normalize baseUrl prefixMappings t : acc) baseUrl prefixMappings rest
  where normalize baseUrl prefixMappings = expandPrefixes prefixMappings . expandBaseUrl baseUrl
        expandBaseUrl (Just _) triple = triple
        expandBaseUrl Nothing triple = triple
        expandPrefixes _ triple = triple
