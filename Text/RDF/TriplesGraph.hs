-- |"TriplesGraph" contains a list-backed graph implementation suitable
-- for smallish graphs.
module Text.RDF.TriplesGraph(TriplesGraph, empty, mkGraph, triplesOf, select, query)

where

import Text.RDF.Core
import Text.RDF.Namespace

import qualified Data.Map as Map
--import qualified Data.Set.AVL as Set

import Data.List

import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as B

-- |A simple implementation of the 'Graph' type class that represents
-- the graph internally as a list of triples. 
--
-- Note that this type of graph is fine for interactive
-- experimentation and querying of smallish (<10,000 triples) graphs,
-- but there are better options for larger graphs or graphs that you
-- will do many queries against (e.g., 'AvlGraph' is faster for queries).
-- 
-- The time complexity of the functions (where n == num_triples) are:
--
--  * 'empty'    : O(1)
--
--  * 'mkGraph'  : O(n)
--
--  * 'triplesOf': O(1)
--
--  * 'select'   : O(n)
--
--  * 'query'    : O(n)
newtype TriplesGraph = TriplesGraph (Triples, Maybe BaseUrl, PrefixMappings)

instance Graph TriplesGraph where
  baseUrl         = baseUrl'
  prefixMappings  = prefixMappings'
  empty           = empty'
  mkGraph         = mkGraph'
  triplesOf       = triplesOf'
  select          = select'
  query           = query'

prefixMappings' :: TriplesGraph -> PrefixMappings
prefixMappings' (TriplesGraph (_, _, pms)) = pms

baseUrl' :: TriplesGraph -> Maybe BaseUrl
baseUrl' (TriplesGraph (_, baseUrl, _)) = baseUrl

{-# NOINLINE empty' #-}
empty' :: TriplesGraph
empty' = TriplesGraph ([], Nothing, PrefixMappings Map.empty)

mkGraph' :: Triples -> Maybe BaseUrl -> PrefixMappings -> TriplesGraph
mkGraph' ts baseUrl pms = TriplesGraph $! (dupeFreeTs, baseUrl, pms)
  where 
    dupeFreeTs = nub . sort . map normalizeTriple $ ts
    normalizeTriple (Triple s p o) = triple s p (norm o)
    norm :: Node -> Node
    norm n 
      | isLNode n  = 
        case n of
          (UNode fs)      -> UNode   fs
          (BNode fs)      -> BNode   fs
          (BNodeGen i)    -> BNodeGen i
          (LNode litVal)  ->
            case litVal of          -- TODO: this is very inefficiently implemented
              (PlainL s)         -> LNode $ PlainL  (escapeQuotes $ B.concatMap replace s)
              (PlainLL val lang) -> LNode $ PlainLL (escapeQuotes $ B.concatMap replace val) lang
              (TypedL val dtype) -> LNode $ TypedL  (escapeQuotes $ B.concatMap replace val) dtype
      | otherwise  = n
    replace c = 
      case c of
        '\n' -> newLineBs
        '\t' -> tabBs      -- not strictly necessary, but a conformance test converts tabs, so we do too
        _    -> B.singleton c

escapeQuotes :: ByteString -> ByteString
escapeQuotes bs =
  case null pieces of
    True  -> bs
    False ->
      case null (tail pieces) of
        True  -> bs
        False -> f [] (head pieces) (tail pieces)
  where 
    pieces = B.split '"' bs :: [ByteString]
    f acc prev []     = B.concat $ reverse (prev:acc)
    f acc prev (x:xs) =
      case B.last prev == '\\' of
        True  ->  f (prev `B.snoc` '"' : acc) x xs
        False ->  f (prev `B.append` backslashQuoteBs : acc) x xs

tabBs, newLineBs, backslashQuoteBs :: ByteString
tabBs            = B.pack "\\t"
newLineBs        = B.pack "\\n"
backslashQuoteBs = B.pack "\\\""

-- mapAccumL :: (acc -> Char -> (acc, Char)) -> acc -> ByteString -> (acc, ByteString)
-- scanl1 :: (Char -> Char -> Char) -> ByteString -> ByteString
triplesOf' :: TriplesGraph -> Triples
triplesOf' (TriplesGraph (ts, _, _)) = ts

select' :: TriplesGraph -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
select' (TriplesGraph (ts, _, _)) s p o = filter (matchSelect s p o) ts

query' :: TriplesGraph -> Maybe Subject -> Maybe Predicate -> Maybe Object -> Triples
query' (TriplesGraph (ts, _, _)) s p o = filter (matchPattern s p o) ts

matchSelect :: NodeSelector -> NodeSelector -> NodeSelector -> Triple -> Bool
matchSelect s p o t = 
  match s (subjectOf t) && match p (predicateOf t) && match o (objectOf t)
  where 
    match Nothing   _ = True
    match (Just fn) n = fn n

matchPattern :: Maybe Subject -> Maybe Predicate -> Maybe Object -> Triple -> Bool
matchPattern subj pred obj t = smatch t && pmatch t && omatch t
  where
    smatch trp = matchNode subj (subjectOf trp)
    pmatch trp = matchNode pred (predicateOf trp)
    omatch trp = matchNode obj (objectOf trp)

matchNode :: Maybe Node -> Node -> Bool
matchNode Nothing   _  = True
matchNode (Just n1) n2 = n1 == n2
