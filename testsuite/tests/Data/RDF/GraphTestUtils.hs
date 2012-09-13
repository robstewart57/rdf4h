module Data.RDF.GraphTestUtils where

import Data.RDF
import Data.RDF.Namespace
import qualified Data.Text as T
import Test.QuickCheck
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import System.IO.Unsafe(unsafePerformIO)

instance Arbitrary BaseUrl where
  arbitrary = oneof $ map (return . BaseUrl . s2t) ["http://example.com/a", "http://asdf.org/b"]
  --coarbitrary = undefined

instance Arbitrary PrefixMappings where
  arbitrary = oneof [return $ PrefixMappings Map.empty, return $ PrefixMappings $
                          Map.fromAscList [(s2t "eg1", s2t "http://example.com/1"),
                                        (s2t "eg2", s2t "http://example.com/2")]]
  --coarbitrary = undefined

-- Test stubs, which just require the appropriate RDF impl function
-- passed in to determine the implementation to be tested.

-- empty RDF should have no triples
p_empty :: RDF rdf => (rdf -> Triples) -> rdf -> Bool
p_empty _triplesOf _empty = _triplesOf _empty == []

-- triplesOf any RDF should return unique triples used to create it
p_mkRdf_triplesOf :: RDF rdf => (rdf -> Triples) -> (Triples -> Maybe BaseUrl -> PrefixMappings -> rdf) -> Triples -> Maybe BaseUrl -> PrefixMappings -> Bool
p_mkRdf_triplesOf _triplesOf _mkRdf ts bUrl pms =
  uordered (_triplesOf (_mkRdf ts bUrl pms)) == uordered ts

-- duplicate input triples should not be returned
p_mkRdf_no_dupes :: RDF rdf => (rdf -> Triples) -> (Triples -> Maybe BaseUrl -> PrefixMappings -> rdf) -> Triples -> Maybe BaseUrl -> PrefixMappings -> Bool
p_mkRdf_no_dupes _triplesOf _mkRdf ts bUrl pms =
  null ts || (result == uordered ts)
   where
    tsWithDupe = head ts : ts
    result = _triplesOf $ _mkRdf tsWithDupe bUrl pms

-- query with all 3 wildcards should yield all triples in RDF
p_query_match_none :: RDF rdf => (Triples -> Maybe BaseUrl -> PrefixMappings -> rdf) -> Triples -> Maybe BaseUrl -> PrefixMappings -> Bool
p_query_match_none  _mkRdf ts bUrl pms = uordered ts == uordered result
  where
    result = query (_mkRdf ts bUrl pms) Nothing Nothing Nothing

-- query with no wildcard and a triple in the RDF should yield
-- a singleton list with just the triple.
p_query_matched_spo :: RDF rdf => (rdf -> Triples) -> rdf -> Property
p_query_matched_spo _triplesOf rdf =
  classify (null ts) "trivial" $
    forAll (tripleFromGen _triplesOf rdf) f
  where
    ts = _triplesOf rdf
    f t = case t of
            Nothing   ->  True
            (Just t') ->  [t'] == queryT rdf t'

-- query as in p_query_matched_spo after duplicating a triple in the
-- RDF, so that we can verify that the results just have 1, even
-- if the RDF itself doesn't ensure that there are no dupes internally.
p_query_matched_spo_no_dupes :: RDF rdf => (rdf -> Triples) -> (Triples -> Maybe BaseUrl -> PrefixMappings -> rdf) -> rdf -> Property
p_query_matched_spo_no_dupes _triplesOf _mkRdf rdf =
  classify (null ts) "trivial" $
    forAll (tripleFromGen _triplesOf rdf) f
  where
    ts = _triplesOf rdf
    f t = case t of
            Nothing   -> True
            Just t'   -> [t'] == queryT (mkRdfWithDupe _triplesOf _mkRdf rdf t') t'

-- query with no wildcard and a triple no in the RDF should yield []
p_query_unmatched_spo :: RDF rdf => (rdf -> Triples) -> rdf -> Triple -> Property
p_query_unmatched_spo _triplesOf rdf t =
  classify (t `elem` ts) "ignored" $
    notElem t ts ==> [] == queryT rdf t
  where
    ts = _triplesOf rdf

-- query with fixed subject and wildcards for pred and obj should yield
-- a list with all triples having subject, and RDF minus result triples
-- should yield all triple with unequal subjects.
p_query_match_s :: RDF rdf => (rdf -> Triples) -> rdf -> Property
p_query_match_s = mk_query_match_fn sameSubj f
  where f t = (Just (subjectOf t), Nothing, Nothing)

-- query w/ fixed predicate and wildcards for subj and obj should yield
-- a list with all triples having predicate, and RDFgraph minus result triples
-- should yield all triple with unequal predicates.
p_query_match_p :: RDF rdf => (rdf -> Triples) -> rdf -> Property
p_query_match_p = mk_query_match_fn samePred f
  where f t = (Nothing, Just (predicateOf t), Nothing)

-- likewise for fixed subject and predicate with object wildcard
p_query_match_o :: RDF rdf => (rdf -> Triples) -> rdf -> Property
p_query_match_o = mk_query_match_fn sameObj f
  where f t = (Nothing, Nothing, Just (objectOf t))

-- verify likewise for fixed subject and predicate with wildcard object
p_query_match_sp :: RDF rdf => (rdf -> Triples) -> rdf -> Property
p_query_match_sp = mk_query_match_fn same f
  where same t1 t2 = sameSubj t1 t2 && samePred t1 t2
        f t = (Just $ subjectOf t, Just $ predicateOf t, Nothing)

-- fixed subject and object with wildcard predicate
p_query_match_so :: RDF rdf => (rdf -> Triples) -> rdf -> Property
p_query_match_so = mk_query_match_fn same f
  where same t1 t2 = sameSubj t1 t2 && sameObj t1 t2
        f t = (Just $ subjectOf t, Nothing, Just $ objectOf t)

-- fixed predicate and object with wildcard subject
p_query_match_po :: RDF rdf => (rdf -> Triples) -> rdf -> Property
p_query_match_po = mk_query_match_fn same f
  where same t1 t2 = samePred t1 t2 && sameObj t1 t2
        f t = (Nothing, Just $ predicateOf t, Just $ objectOf t)

mk_query_match_fn :: RDF rdf => (Triple -> Triple -> Bool)
  -> (Triple -> (Maybe Node, Maybe Node, Maybe Node))
  -> (rdf -> Triples) -> rdf -> Property
mk_query_match_fn tripleCompareFn  mkPatternFn _triplesOf rdf =
  forAll (tripleFromGen _triplesOf rdf) f
  where
    f :: Maybe Triple -> Bool
    f Nothing   = True
    f (Just t)  =
      let
        all_ts = _triplesOf rdf
        all_ts_sorted = uordered all_ts
        results = uordered $ queryC rdf (mkPatternFn t)
        notResults = ldiff all_ts_sorted results
      in
        all (tripleCompareFn t) results &&
        all (not . tripleCompareFn t) notResults

p_select_match_none :: RDF rdf => rdf -> Bool
p_select_match_none rdf = select rdf Nothing Nothing Nothing == removeDupes (triplesOf rdf)

p_select_match_s :: RDF rdf => (rdf -> Triples) -> rdf -> Property
p_select_match_s =
  p_select_match_fn same mkPattern
  where
    same = equivNode (==) subjectOf
    mkPattern t = (Just (\n -> n == subjectOf t), Nothing, Nothing)

p_select_match_p :: RDF rdf => (rdf -> Triples) -> rdf -> Property
p_select_match_p =
  p_select_match_fn same mkPattern
  where
    same = equivNode equiv predicateOf
    equiv (UNode u1) (UNode u2) = T.last u1 == T.last u2
    equiv _          _          = error "GraphTestUtils.p_select_match_p.equiv"
    mkPattern t = (Nothing, Just (\n -> lastChar n == lastChar (predicateOf t)) , Nothing)
    lastChar (UNode uri) = T.last uri
    lastChar _           = error "GraphTestUtils.p_select_match_p.lastChar"


p_select_match_o :: RDF rdf => (rdf -> Triples) -> rdf -> Property
p_select_match_o =
  p_select_match_fn same mkPattern
  where
    same = equivNode (/=) objectOf
    mkPattern t = (Nothing, Nothing, Just (\n -> n /= objectOf t))

p_select_match_sp :: RDF rdf => (rdf -> Triples) -> rdf -> Property
p_select_match_sp =
  p_select_match_fn same mkPattern
  where
    same t1 t2 = subjectOf t1 == subjectOf t2 && predicateOf t1 /= predicateOf t2
    mkPattern t = (Just (\n -> n == subjectOf t), Just (\n -> n /= predicateOf t), Nothing)

p_select_match_so :: RDF rdf => (rdf -> Triples) -> rdf -> Property
p_select_match_so =
  p_select_match_fn same mkPattern
  where
    same t1 t2 = subjectOf t1 /= subjectOf t2 && objectOf t1 == objectOf t2
    mkPattern t = (Just (\n -> n /= subjectOf t), Nothing, Just (\n -> n == objectOf t))

p_select_match_po :: RDF rdf => (rdf -> Triples) -> rdf -> Property
p_select_match_po =
  p_select_match_fn same mkPattern
  where
    same t1 t2 = predicateOf t1 == predicateOf t2 && objectOf t1 == objectOf t2
    mkPattern t = (Nothing, Just (\n -> n == predicateOf t), Just (\n -> n == objectOf t))

p_select_match_spo :: RDF rdf => (rdf -> Triples) -> rdf -> Property
p_select_match_spo =
  p_select_match_fn same mkPattern
  where
    same t1 t2 = subjectOf t1 == subjectOf t2 && predicateOf t1 == predicateOf t2 &&
                 objectOf t1 /= objectOf t2
    mkPattern t = (Just (\n -> n == subjectOf t),
                   Just (\n -> n == predicateOf t),
                   Just (\n -> n /= objectOf t))

equivNode :: (Node -> Node -> Bool) -> (Triple -> Node) -> Triple -> Triple -> Bool
equivNode eqFn exFn t1 t2 = exFn t1 `eqFn` exFn t2

p_select_match_fn :: RDF rdf => (Triple -> Triple -> Bool)
  -> (Triple -> (NodeSelector, NodeSelector, NodeSelector))
  -> (rdf -> Triples) -> rdf -> Property
p_select_match_fn tripleCompareFn mkPatternFn _triplesOf rdf =
  forAll (tripleFromGen _triplesOf rdf) f
  where
    f :: Maybe Triple -> Bool
    f Nothing = True
    f (Just t) =
      let
        all_ts = triplesOf rdf
        all_ts_sorted = uordered all_ts
        results = uordered $ selectC rdf (mkPatternFn t)
        notResults = ldiff all_ts_sorted results
      in
        all (tripleCompareFn t) results &&
        all (not . tripleCompareFn t) notResults

mkRdfWithDupe :: RDF rdf => (rdf -> Triples) -> (Triples -> Maybe BaseUrl -> PrefixMappings -> rdf) -> rdf -> Triple -> rdf
mkRdfWithDupe _triplesOf _mkRdf rdf t = _mkRdf ts (baseUrl rdf) (prefixMappings rdf)
  where ts = t : _triplesOf rdf


-- Utility functions and test data ... --

-- a curried version of query that delegates to the actual query after unpacking
-- curried maybe node pattern.
queryC :: RDF rdf => rdf -> (Maybe Node, Maybe Node, Maybe Node) -> Triples
queryC rdf (s, p, o) = query rdf s p o

selectC :: RDF rdf => rdf -> (NodeSelector, NodeSelector, NodeSelector) -> Triples
selectC rdf (s, p, o) = select rdf s p o

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 fn (x, y, z) = fn x y z

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 fn x y z = fn (x, y, z)

debug :: String -> Triples -> Bool
debug msg ts =
  unsafePerformIO $
    putStrLn msg >> mapM print ts >> return True

ldiff :: Triples -> Triples -> Triples
ldiff l1 l2 = Set.toList $(Set.fromList l1) `Set.difference` Set.fromList l2

sameSubj :: Triple -> Triple -> Bool
sameSubj t1 t2 = subjectOf t1 == subjectOf t2

samePred :: Triple -> Triple -> Bool
samePred t1 t2 = predicateOf t1 == predicateOf t2

sameObj :: Triple -> Triple -> Bool
sameObj  t1 t2 = objectOf t1 == objectOf t2

-- Convert a list of triples into a sorted list of unique triples.
uordered :: Triples -> Triples
uordered  =  map head . group . sortTriples

tripleFromGen :: RDF rdf => (rdf -> Triples) -> rdf -> Gen (Maybe Triple)
tripleFromGen _triplesOf rdf =
  if null ts
  then return Nothing
  else oneof $ map (return . Just) ts
   where ts = _triplesOf rdf

queryT :: RDF rdf => rdf -> Triple -> Triples
queryT rdf t = query rdf (Just $ subjectOf t) (Just $ predicateOf t) (Just $ objectOf t)

languages :: [T.Text]
languages = [s2t "fr", s2t "en"]

datatypes :: [T.Text]
datatypes = map (mkUri xsd . s2t) ["string", "int", "token"]

uris :: [T.Text]
uris = map (mkUri ex) [s2t n `T.append` s2t (show (i::Int)) | n <- ["foo", "bar", "quz", "zak"], i <- [0..9]]

plainliterals :: [LValue]
plainliterals = [plainLL lit lang | lit <- litvalues, lang <- languages]

typedliterals :: [LValue]
typedliterals = [typedL lit dtype | lit <- litvalues, dtype <- datatypes]

litvalues :: [T.Text]
litvalues = map T.pack ["hello", "world", "peace", "earth", "", "haskell"]

unodes :: [Node]
unodes = map UNode uris

bnodes :: [ Node]
bnodes = map (BNode . \i -> s2t ":_genid" `T.append` s2t (show (i::Int))) [1..5]

lnodes :: [Node]
lnodes = [LNode lit | lit <- plainliterals ++ typedliterals]

test_triples :: [Triple]
test_triples = [triple s p o | s <- unodes ++ bnodes, p <- unodes, o <- unodes ++ bnodes ++ lnodes]

maxN :: Int
maxN = min 100 (length test_triples - 1)

instance Arbitrary Triple where
  arbitrary = liftM3 triple arbitraryS arbitraryP arbitraryO
  --coarbitrary = undefined

instance Arbitrary Node where
  arbitrary = oneof $ map return unodes
  --coarbitrary = undefined

arbitraryTNum :: Gen Int
arbitraryTNum = choose (0, maxN - 1)

arbitraryTs :: Gen Triples
arbitraryTs = do
  n <- sized (\_ -> choose (0, maxN))
  sequence [arbitrary | _ <- [1..n]]


arbitraryT :: Gen Triple
arbitraryT = elements test_triples

arbitraryN :: Gen Int
arbitraryN = choose (0, maxN - 1)

arbitraryS, arbitraryP, arbitraryO :: Gen Node
arbitraryS = oneof $ map return $ unodes ++ bnodes
arbitraryP = oneof $ map return unodes
arbitraryO = oneof $ map return $ unodes ++ bnodes ++ lnodes


