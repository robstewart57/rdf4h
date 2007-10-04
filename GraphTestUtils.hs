module GraphTestUtils where

import RDF
import Namespace

import Data.Char
import Data.List
import qualified Data.Set.AVL as S
import Control.Monad
import System.Random
import Test.QuickCheck
import Foreign(unsafePerformIO)

-- Test stubs, which just require the appropriate graph impl function
-- passed in to determine the graph implementation to be tested.

-- empty graph should have no triples
p_empty :: Graph g => (g -> Triples) -> g -> Bool
p_empty _triplesOf _empty = _triplesOf _empty == []


-- triplesOf any graph should return unique triples used to create it
p_mkGraph_triplesOf :: Graph g => (g -> Triples) -> (Triples -> g) -> Triples -> Bool
p_mkGraph_triplesOf _triplesOf _mkGraph ts = 
  ordered (_triplesOf (_mkGraph ts)) == uordered ts

-- duplicate input triples should not be returned
p_mkGraph_no_dupes :: Graph g => (g -> Triples) -> (Triples -> g) -> Triples -> Bool
p_mkGraph_no_dupes _triplesOf _mkGraph ts = 
  case null ts of
    True  -> True
    False -> ordered result == uordered ts
  where 
    tsWithDupe = head ts : ts
    result = _triplesOf $ _mkGraph tsWithDupe

-- query with all 3 wildcards should yield all triples in graph
p_query_all_wildcard :: Graph g => (Triples -> g) -> Triples -> Bool
p_query_all_wildcard  _mkGraph ts = uordered ts == ordered result
  where 
    result = query (_mkGraph ts) Nothing Nothing Nothing

-- query with no wildcard and a triple in the graph should yield
-- a singleton list with just the triple.
p_query_matched_no_wildcards :: Graph g => (g -> Triples) -> g -> Property
p_query_matched_no_wildcards _triplesOf gr = 
  classify (null ts) "trivial" $ 
    forAll (tripleFromGen _triplesOf gr) f
  where 
    ts = _triplesOf gr
    f t = case t of 
            Nothing   ->  True
            (Just t') ->  [t'] == queryT gr t'

-- query with no wildcard and a triple no in the graph should yield []
p_query_unmatched_no_wildcards :: Graph g => (g -> Triples) -> g -> Triple -> Property
p_query_unmatched_no_wildcards _triplesOf gr t = 
  classify (t `elem` ts) "ignored" $
    not (t `elem` ts) ==> [] == queryT gr t
  where 
    ts = _triplesOf gr

-- query with fixed subject and wildcards for pred and obj should yield
-- a list with all triples having subject, and graph minus result triples
-- should yield all triple with unequal subjects.
p_query_matched_po_wildcards :: Graph g => (g -> Triples) -> g -> Property
p_query_matched_po_wildcards _triplesOf gr =
  forAll (tripleFromGen _triplesOf gr) f
  where
    f :: Maybe Triple -> Bool
    f Nothing   = True
    f (Just t)  =
      let 
        all_ts = _triplesOf gr
        all_ts_sorted = ordered all_ts
        results = ordered (query gr (Just $ subjectOf t) Nothing Nothing)
        notResults = ldiff all_ts_sorted results        
      in
        all (sameSubj t) results &&
        all (not . sameSubj t) notResults

-- Utility functions and test data ... --

debug :: String -> Triples -> Bool
debug msg ts = unsafePerformIO $ 
                 putStrLn msg >> mapM (putStrLn . show) ts >> return True

ldiff :: Triples -> Triples -> Triples
ldiff l1 l2 = S.toList $(S.fromList l1) `S.difference` (S.fromList l2)

sameSubj t1 t2 = subjectOf t1 == subjectOf t2
samePred t1 t2 = predicateOf t1 == predicateOf t2
sameObj  t1 t2 = objectOf t1 == objectOf t2

tripleFromGen :: Graph g => (g -> Triples) -> g -> Gen (Maybe Triple)
tripleFromGen _triplesOf gr = 
  case null ts of
    True  -> return Nothing
    False -> oneof $ map (return . Just) ts
  where ts = _triplesOf gr

queryT :: Graph g => g -> Triple -> Triples
queryT gr t = query gr (Just $ subjectOf t) (Just $ predicateOf t) (Just $ objectOf t)


languages = [Nothing, Just "fr", Just "en"]
datatypes = [makeUri xsd "string", makeUri xsd "int", makeUri xsd "token"]
uris = map (makeUri ex) [n ++ show i | n <- ["foo", "bar", "quz", "zak"], i <- [0..9]]
plainliterals = [PlainL lit lang | lit <- litvalues, lang <- languages]
typedliterals = [TypedL lit dtype | lit <- litvalues, dtype <- datatypes]
litvalues = ["hello", "world", "peace", "earth", "", "haskell"]

unodes :: [Node]
unodes = map UNode uris
bnodes :: [Node]
bnodes = map (\s -> BNode $ ":_anon" ++ show s) [1..5]
lnodes :: [Node]
lnodes = [LNode lit | lit <- plainliterals ++ typedliterals]

test_triples :: Triples
test_triples = [triple subj pred obj | subj <- unodes ++ bnodes, 
                                       pred <- unodes,
                                       obj <- unodes ++ bnodes ++ lnodes]


ordered :: Triples -> Triples
ordered = sort

uordered :: Triples -> Triples
uordered = sort . S.toList . S.fromList

maxN = min 100 (length test_triples - 1)

instance Arbitrary Triple where
  arbitrary = liftM3 triple arbitraryS arbitraryP arbitraryO
  coarbitrary = undefined

instance Arbitrary Node where
  arbitrary = oneof $ map return unodes
  coarbitrary = undefined

arbitraryTNum :: Gen Int
arbitraryTNum = choose (0, maxN - 1)

arbitraryTs :: Gen Triples
arbitraryTs = do
  n <- sized (\n -> choose (0, maxN))
  xs <- sequence [arbitrary | i <- [1..n]]
  return xs

arbitraryT :: Gen Triple
arbitraryT = elements test_triples

arbitraryN = choose (0, maxN - 1)


arbitraryS, arbitraryP, arbitraryO :: Gen Node
arbitraryS = oneof $ map return $ unodes ++ bnodes
arbitraryP = oneof $ map return unodes
arbitraryO = oneof $ map return $ unodes ++ bnodes ++ lnodes


