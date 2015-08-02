module Data.RDF.Query (

  -- * Query functions
  equalSubjects, equalPredicates, equalObjects,
  subjectOf, predicateOf, objectOf, isEmpty,
  rdfContainsNode, tripleContainsNode,
  listSubjectsWithPredicate, listObjectsOfPredicate,

  -- * RDF graph functions
  isIsomorphic, expandTriples, fromEither,

  -- * expansion functions
  expandTriple, expandNode, expandURI,

  -- * absolutizing functions
  absolutizeTriple, absolutizeNode
) where

import Prelude hiding (pred)
import Data.List
import Data.RDF.Types
import qualified Data.RDF.Namespace as NS (toPMList, uriOf, rdf)
import qualified Data.Text.Lazy as T
import Data.Maybe (catMaybes)

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
-- Note that it doesn't perform namespace expansion!
tripleContainsNode :: Node -> Triple -> Bool
{-# INLINE tripleContainsNode #-}
tripleContainsNode node t = 
 subjectOf t == node || predicateOf t == node || objectOf t == node

-- |Determine whether two triples have equal subjects.
-- Note that it doesn't perform namespace expansion!
equalSubjects :: Triple -> Triple -> Bool
equalSubjects (Triple s1 _ _) (Triple s2 _ _) = s1 == s2

-- |Determine whether two triples have equal predicates.
-- Note that it doesn't perform namespace expansion!
equalPredicates :: Triple -> Triple -> Bool
equalPredicates (Triple _ p1 _) (Triple _ p2 _) = p1 == p2

-- |Determine whether two triples have equal objects.
-- Note that it doesn't perform namespace expansion!
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
expandTriples' acc baseURL prefixMaps (t:rest) = expandTriples' (normalize baseURL prefixMaps t : acc) baseURL prefixMaps rest
  where normalize baseURL' prefixMaps' = absolutizeTriple baseURL' . expandTriple prefixMaps'

-- |Expand the triple with the prefix map.
expandTriple :: PrefixMappings -> Triple -> Triple
expandTriple pms t = triple (expandNode pms $ subjectOf t) (expandNode pms $ predicateOf t) (expandNode pms $ objectOf t)

-- |Expand the node with the prefix map.
-- Only UNodes are expanded, other kinds of nodes are returned as-is.
expandNode :: PrefixMappings -> Node -> Node
expandNode pms (UNode n) = unode $ expandURI pms n
expandNode _ n'          = n'

-- |Expand the URI with the prefix map.
-- Also expands "a" to "http://www.w3.org/1999/02/22-rdf-syntax-ns#type".
expandURI :: PrefixMappings -> T.Text -> T.Text
expandURI _ "a"  = T.append (NS.uriOf NS.rdf) "type"
expandURI pms' x = firstExpandedOrOriginal x $ catMaybes $ map (resourceTail x) (NS.toPMList pms')
  where resourceTail :: T.Text -> (T.Text, T.Text) -> Maybe T.Text
        resourceTail x' (p', u') = T.stripPrefix (T.append p' ":") x' >>= Just . T.append u'
        firstExpandedOrOriginal :: a -> [a] -> a
        firstExpandedOrOriginal orig' [] = orig'
        firstExpandedOrOriginal _ (e:_)  = e

-- |Prefixes relative URIs in the triple with BaseUrl.
absolutizeTriple :: Maybe BaseUrl -> Triple -> Triple
absolutizeTriple base t = triple (absolutizeNode base $ subjectOf t) (absolutizeNode base $ predicateOf t) (absolutizeNode base $ objectOf t)

-- |Prepends BaseUrl to UNodes with relative URIs.
absolutizeNode :: Maybe BaseUrl -> Node -> Node
absolutizeNode (Just (BaseUrl b')) (UNode u') = unode $ mkAbsoluteUrl b' u'
absolutizeNode _ n                     = n
