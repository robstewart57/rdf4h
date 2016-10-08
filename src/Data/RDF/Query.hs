module Data.RDF.Query (

  -- * Query functions
  equalSubjects, equalPredicates, equalObjects,
  subjectOf, predicateOf, objectOf, isEmpty,
  rdfContainsNode, tripleContainsNode,
  subjectsWithPredicate, objectsOfPredicate, uordered,

  -- * RDF graph functions
  isIsomorphic, isGraphIsomorphic, expandTriples, fromEither,

  -- * expansion functions
  expandTriple, expandNode, expandURI,

  -- * absolutizing functions
  absolutizeTriple, absolutizeNode
) where

import Prelude hiding (pred)
import Data.List
import Data.RDF.Types
import qualified Data.RDF.Namespace as NS (toPMList, uriOf, rdf)
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import Data.Graph (Graph,graphFromEdges)
import qualified Data.Graph.Automorphism as Automorphism
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

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
rdfContainsNode :: (Rdf a) => RDF a -> Node -> Bool
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
isEmpty :: Rdf a => RDF a -> Bool
isEmpty rdf =
  let ts = triplesOf rdf
  in null ts

-- |Lists of all subjects of triples with the given predicate.
subjectsWithPredicate :: Rdf a => RDF a -> Predicate -> [Subject]
subjectsWithPredicate rdf pred = map subjectOf $ query rdf Nothing (Just pred) Nothing

-- |Lists of all objects of triples with the given predicate.
objectsOfPredicate :: Rdf a => RDF a -> Predicate -> [Object]
objectsOfPredicate rdf pred = map objectOf $ query rdf Nothing (Just pred) Nothing

-- |Convert a parse result into an RDF if it was successful
-- and error and terminate if not.
fromEither :: Rdf a => Either ParseFailure (RDF a) -> RDF a
fromEither res =
  case res of
    (Left err) -> error (show err)
    (Right rdf) -> rdf

-- |Convert a list of triples into a sorted list of unique triples.
uordered :: Triples -> Triples
uordered  =  sort . nub

-- graphFromEdges :: Ord key => [(node, key, [key])] -> (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)

-- |This determines if two RDF representations are equal regardless of blank
-- node names, triple order and prefixes.  In math terms, this is the \simeq
-- latex operator, or ~=
isIsomorphic :: (Rdf a, Rdf b) => RDF a -> RDF b -> Bool
isIsomorphic g1 g2 = and $ zipWith compareTripleUnlessBlank (normalize g1) (normalize g2)
  where
    compareNodeUnlessBlank :: Node -> Node -> Bool
    compareNodeUnlessBlank (BNode _)     (BNode _)     = True
    compareNodeUnlessBlank (UNode n1)    (UNode n2)    = n1 == n2
    compareNodeUnlessBlank (BNodeGen i1) (BNodeGen i2) = i1 == i2
    compareNodeUnlessBlank (LNode l1)    (LNode l2)    = l1 == l2
    compareNodeUnlessBlank (BNodeGen _)  (BNode _)     = True
    compareNodeUnlessBlank (BNode _)     (BNodeGen _)  = True
    compareNodeUnlessBlank _             _             = False

    compareTripleUnlessBlank :: Triple -> Triple -> Bool
    compareTripleUnlessBlank (Triple s1 p1 o1) (Triple s2 p2 o2) =
        compareNodeUnlessBlank s1 s2 &&
        compareNodeUnlessBlank p1 p2 &&
        compareNodeUnlessBlank o1 o2

    normalize :: (Rdf a) => RDF a -> Triples
    normalize = sort . nub . expandTriples

-- | Compares the structure of two graphs and returns 'True' if
--   their graph structures are identical. This does not consider the nature of
--   each node in the graph, i.e. the URI text of 'UNode' nodes, the generated
--   index of a blank node, or the values in literal nodes.
isGraphIsomorphic :: (Rdf a, Rdf b) => RDF a -> RDF b -> Bool
isGraphIsomorphic g1 g2 = Automorphism.isIsomorphic g1' g2'
    where
      g1' = rdfGraphToDataGraph g1
      g2' = rdfGraphToDataGraph g2
      rdfGraphToDataGraph :: Rdf c => RDF c -> Graph
      rdfGraphToDataGraph g = dataGraph
          where
            triples = expandTriples g
            triplesHashMap :: HashMap (Subject,Predicate) [Object]
            triplesHashMap = HashMap.fromListWith (++) [((s,p), [o]) | Triple s p o <- triples]
            triplesGrouped :: [((Subject,Predicate),[Object])]
            triplesGrouped = HashMap.toList triplesHashMap
            (dataGraph,_,_) = (graphFromEdges . map (\((s,p),os) -> (s,p,os))) triplesGrouped

-- |Expand the triples in a graph with the prefix map and base URL for that
-- graph.
expandTriples :: (Rdf a) => RDF a -> Triples
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
