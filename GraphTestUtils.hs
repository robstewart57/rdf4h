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
p_query_match_none :: Graph g => (Triples -> g) -> Triples -> Bool
p_query_match_none  _mkGraph ts = uordered ts == ordered result
  where 
    result = query (_mkGraph ts) Nothing Nothing Nothing

-- query with no wildcard and a triple in the graph should yield
-- a singleton list with just the triple.
p_query_matched_spo :: Graph g => (g -> Triples) -> g -> Property
p_query_matched_spo _triplesOf gr = 
  classify (null ts) "trivial" $ 
    forAll (tripleFromGen _triplesOf gr) f
  where 
    ts = _triplesOf gr
    f t = case t of 
            Nothing   ->  True
            (Just t') ->  [t'] == queryT gr t'

-- query with no wildcard and a triple no in the graph should yield []
p_query_unmatched_spo :: Graph g => (g -> Triples) -> g -> Triple -> Property
p_query_unmatched_spo _triplesOf gr t = 
  classify (t `elem` ts) "ignored" $
    not (t `elem` ts) ==> [] == queryT gr t
  where 
    ts = _triplesOf gr

-- query with fixed subject and wildcards for pred and obj should yield
-- a list with all triples having subject, and graph minus result triples
-- should yield all triple with unequal subjects.
p_query_match_s :: Graph g => (g -> Triples) -> g -> Property
p_query_match_s = mk_query_match_fn sameSubj f
  where f t = (Just (subjectOf t), Nothing, Nothing)

-- query w/ fixed predicate and wildcards for subj and obj should yield
-- a list with all triples having predicate, and graph minus result triples
-- should yield all triple with unequal predicates.
p_query_match_p :: Graph g => (g -> Triples) -> g -> Property
p_query_match_p = mk_query_match_fn samePred f
  where f t = (Nothing, Just (predicateOf t), Nothing)

-- likewise for fixed subject and predicate with object wildcard
p_query_match_o :: Graph g => (g -> Triples) -> g -> Property
p_query_match_o = mk_query_match_fn sameObj f 
  where f t = (Nothing, Nothing, Just (objectOf t))

-- verify likewise for fixed subject and predicate with wildcard object
p_query_match_sp :: Graph g => (g -> Triples) -> g -> Property
p_query_match_sp = mk_query_match_fn same f
  where same t1 t2 = sameSubj t1 t2 && samePred t1 t2
        f t = (Just $ subjectOf t, Just $ predicateOf t, Nothing)

-- fixed subject and object with wildcard predicate
p_query_match_so :: Graph g => (g -> Triples) -> g -> Property
p_query_match_so = mk_query_match_fn same f
  where same t1 t2 = sameSubj t1 t2 && sameObj t1 t2
        f t = (Just $ subjectOf t, Nothing, Just $ objectOf t)

-- fixed predicate and object with wildcard subject
p_query_match_po :: Graph g => (g -> Triples) -> g -> Property
p_query_match_po = mk_query_match_fn same f
  where same t1 t2 = samePred t1 t2 && sameObj t1 t2
        f t = (Nothing, Just $ predicateOf t, Just $ objectOf t)

mk_query_match_fn :: Graph g => (Triple -> Triple -> Bool)
  -> (Triple -> (Maybe Node, Maybe Node, Maybe Node))
  -> (g -> Triples) -> g -> Property
mk_query_match_fn tripleCompareFn  mkPatternFn _triplesOf gr =
  forAll (tripleFromGen _triplesOf gr) f
  where
    f :: Maybe Triple -> Bool
    f Nothing   = True
    f (Just t)  =
      let 
        all_ts = _triplesOf gr
        all_ts_sorted = ordered all_ts
        results = ordered $ queryC gr (mkPatternFn t)
        notResults = ldiff all_ts_sorted results        
      in
        all (tripleCompareFn t) results &&
        all (not . tripleCompareFn t) notResults

p_select_match_none :: Graph g => g -> Bool
p_select_match_none gr = select gr Nothing Nothing Nothing == triplesOf gr

p_select_match_s :: Graph g => (g -> Triples) -> g -> Property
p_select_match_s =
  p_select_match_fn same mkPattern
  where 
    same = equivNode (==) subjectOf
    mkPattern t = (Just (\n -> n == subjectOf t), Nothing, Nothing)
        
p_select_match_p :: Graph g => (g -> Triples) -> g -> Property
p_select_match_p =
  p_select_match_fn same mkPattern
  where 
    same = equivNode equiv predicateOf
    equiv (UNode u1) (UNode u2) = last u1 == last u2
    mkPattern t = (Nothing, Just (\n -> lastChar n == lastChar (predicateOf t)) , Nothing)
    lastChar (UNode uri) = last uri
    

p_select_match_o :: Graph g => (g -> Triples) -> g -> Property
p_select_match_o =
  p_select_match_fn same mkPattern
  where
    same = equivNode (/=) objectOf
    mkPattern t = (Nothing, Nothing, Just (\n -> n /= objectOf t))

p_select_match_sp :: Graph g => (g -> Triples) -> g -> Property
p_select_match_sp =
  p_select_match_fn same mkPattern
  where
    same t1 t2 = subjectOf t1 == subjectOf t2 && predicateOf t1 /= predicateOf t2
    mkPattern t = (Just (\n -> n == subjectOf t), Just (\n -> n /= predicateOf t), Nothing)

p_select_match_so :: Graph g => (g -> Triples) -> g -> Property
p_select_match_so =
  p_select_match_fn same mkPattern
  where
    same t1 t2 = subjectOf t1 /= subjectOf t2 && objectOf t1 == objectOf t2
    mkPattern t = (Just (\n -> n /= subjectOf t), Nothing, Just (\n -> n == objectOf t))

p_select_match_po :: Graph g => (g -> Triples) -> g -> Property
p_select_match_po =
  p_select_match_fn same mkPattern
  where
    same t1 t2 = predicateOf t1 == predicateOf t2 && objectOf t1 == objectOf t2
    mkPattern t = (Nothing, Just (\n -> n == predicateOf t), Just (\n -> n == objectOf t))

p_select_match_spo :: Graph g => (g -> Triples) -> g -> Property
p_select_match_spo =
  p_select_match_fn same mkPattern
  where
    same t1 t2 = subjectOf t1 == subjectOf t2 && predicateOf t1 == predicateOf t2 &&
                 objectOf t1 /= objectOf t2
    mkPattern t = (Just (\n -> n == subjectOf t),
                   Just (\n -> n == predicateOf t),
                   Just (\n -> n /= objectOf t))

equivNode :: (Node -> Node -> Bool) -> (Triple -> Node) -> Triple -> Triple -> Bool
equivNode eqFn exFn t1 t2 = (exFn t1) `eqFn` (exFn t2)

p_select_match_fn :: Graph g => (Triple -> Triple -> Bool)
  -> (Triple -> (NodeSelector, NodeSelector, NodeSelector))
  -> (g -> Triples) -> g -> Property
p_select_match_fn tripleCompareFn mkPatternFn _triplesOf gr =
  forAll (tripleFromGen _triplesOf gr) f
  where 
    f :: Maybe Triple -> Bool
    f Nothing = True
    f (Just t) =
      let
        all_ts = triplesOf gr
        all_ts_sorted = ordered all_ts
        results = ordered $ selectC gr (mkPatternFn t)
        notResults = ldiff all_ts_sorted results
      in
        all (tripleCompareFn t) results &&
        all (not . tripleCompareFn t) notResults

-- Utility functions and test data ... --

-- a curried version of query that delegates to the actual query after unpacking
-- curried maybe node pattern.
queryC :: Graph g => g -> (Maybe Node, Maybe Node, Maybe Node) -> Triples
queryC gr (s, p, o) = query gr s p o

selectC :: Graph g => g -> (NodeSelector, NodeSelector, NodeSelector) -> Triples
selectC gr (s, p, o) = select gr s p o

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 fn = \(x,y,z) -> fn x y z

curry3 :: ((a, b, c) -> d)    ->     (a -> b -> c -> d)
curry3 fn = \x -> \y -> \z -> fn (x,y,z)

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


