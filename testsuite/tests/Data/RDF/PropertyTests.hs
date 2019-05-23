{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.RDF.PropertyTests (graphTests) where

import qualified Data.Text.IO as T
import Data.RDF hiding (empty)
import Data.RDF.Namespace hiding (rdf)
import qualified Data.Text as T
import Test.QuickCheck
import Data.List
import Data.Semigroup ((<>))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import Control.Exception (bracket)
import GHC.Generics ()
import System.Directory (removeFile)
import System.IO

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO,run)

----------------------------------------------------
--  property based quick check test cases         --
----------------------------------------------------

graphTests
  :: (Rdf rdf)
  => TestName
  -> RDF rdf -- ^ empty
  -> (Triples -> Maybe BaseUrl -> PrefixMappings -> RDF rdf) -- ^ _mkRdf
  -> TestTree
graphTests testGroupName empty _mkRdf =
  testGroup
    testGroupName
    [ testProperty "empty" (p_empty empty)
    , testProperty "mkRdf_triplesOf" (p_mkRdf_triplesOf _mkRdf)
    , testProperty "mkRdf_no_dupes" (p_mkRdf_no_dupes _mkRdf)
    , testProperty "query_match_none" (p_query_match_none _mkRdf)
      -- see comment above p_query_matched_spo_no_dupes for why this is disabled
      -- , testProperty "query_matched_spo_no_dupes" (p_query_matched_spo_no_dupes _triplesOf _mkRdf)
    , testProperty "query_match_s" (p_query_match_s empty)
    , testProperty "query_match_p" (p_query_match_p empty)
    , testProperty "query_match_o" (p_query_match_o empty)
    , testProperty "query_match_sp" (p_query_match_sp empty)
    , testProperty "query_match_so" (p_query_match_so empty)
    , testProperty "query_match_po" (p_query_match_po empty)
    , testProperty "query_match_spo" (p_query_match_spo empty)
    , testProperty "query_unmatched_spo" (p_query_unmatched_spo empty)
    , testProperty "select_match_none" (p_select_match_none empty)
    , testProperty "select_match_s" (p_select_match_s empty)
    , testProperty "select_match_p" (p_select_match_p empty)
    , testProperty "select_match_o" (p_select_match_o empty)
    , testProperty "select_match_sp" (p_select_match_sp empty)
    , testProperty "select_match_so" (p_select_match_so empty)
    , testProperty "select_match_po" (p_select_match_po empty)
    , testProperty "select_match_spo" (p_select_match_spo empty)
    , testProperty "reversed RDF handle write" (p_reverseRdfTest _mkRdf)
      -- adding and removing triples from a graph.
    , testProperty "add_triple" (p_add_triple _mkRdf)
    , testProperty "remove_triple" (p_remove_triple _mkRdf)
    , testProperty
        "remove_triple_from_graph"
        (p_remove_triple_from_graph _mkRdf)
    , testProperty
        "remove_triple_from_singleton_graph_query_s"
        (p_remove_triple_from_singleton_graph_query_s empty)
    , testProperty
        "remove_triple_from_singleton_graph_query_p"
        (p_remove_triple_from_singleton_graph_query_p empty)
    , testProperty
        "remove_triple_from_singleton_graph_query_o"
        (p_remove_triple_from_singleton_graph_query_o empty)
    , testProperty
        "p_add_then_remove_triples"
        (p_add_then_remove_triples empty)
    ]

newtype SingletonGraph rdf = SingletonGraph
  { rdfGraph :: (RDF rdf)
  }

instance (Rdf rdf) =>
         Arbitrary (SingletonGraph rdf) where
  arbitrary = do
    pref <- arbitraryPrefixMappings
    baseU' <- arbitraryBaseUrl
    baseU <- oneof [return (Just baseU'), return Nothing]
    t <- liftM3 triple arbitraryS arbitraryP arbitraryO
    return SingletonGraph {rdfGraph = (mkRdf [t] baseU pref)}

instance (Rdf rdf) =>
         Show (SingletonGraph rdf) where
  show singletonGraph = showGraph (rdfGraph singletonGraph)

instance Arbitrary BaseUrl where
  arbitrary = arbitraryBaseUrl

instance Arbitrary PrefixMappings where
  arbitrary = arbitraryPrefixMappings

arbitraryBaseUrl :: Gen BaseUrl
arbitraryBaseUrl =
  oneof $
  fmap
    (return . BaseUrl . T.pack)
    ["http://example.org/", "http://example.com/a", "http://asdf.org/b", "http://asdf.org/c"]

arbitraryPrefixMappings :: Gen PrefixMappings
arbitraryPrefixMappings =
  oneof
    [ return $ PrefixMappings Map.empty
    , return $
      PrefixMappings $
      Map.fromAscList
        [ (T.pack "ex", T.pack "ex:")
        , (T.pack "eg1", T.pack "http://example.org/1")
        , (T.pack "eg2", T.pack "http://example.org/2")
        , (T.pack "eg3", T.pack "http://example.org/3")
        ]
    ]


-- Test stubs, which just require the appropriate RDF impl function
-- passed in to determine the implementation to be tested.

-- empty RDF should have no triples
p_empty
  :: Rdf rdf
  => RDF rdf -> Bool
p_empty empty = null (triplesOf empty)

-- triplesOf any RDF should return unique triples used to create it
p_mkRdf_triplesOf
  :: Rdf rdf
  => (Triples -> Maybe BaseUrl -> PrefixMappings -> RDF rdf)
  -> Triples
  -> Maybe BaseUrl
  -> PrefixMappings
  -> Bool
p_mkRdf_triplesOf _mkRdf ts bUrl pms =
  uordered (triplesOf (_mkRdf ts bUrl pms)) == uordered ts

-- duplicate input triples should not be returned when
-- uniqTriplesof is used
p_mkRdf_no_dupes
  :: Rdf rdf
  => (Triples -> Maybe BaseUrl -> PrefixMappings -> RDF rdf)
  -> Triples
  -> Maybe BaseUrl
  -> PrefixMappings
  -> Bool
p_mkRdf_no_dupes _mkRdf ts bUrl pms = null ts || (sort result == uordered ts)
  where
    tsWithDupe = head ts : ts
    result = uniqTriplesOf $ _mkRdf tsWithDupe bUrl pms

-- Note: in TriplesGraph and PatriciaTreeGraph `query` expands triples
--       but `ts` here is not  necessarily expanded. What is the correct
--       property this test should check?
--
-- query with all 3 wildcards should yield all triples in RDF
p_query_match_none
  :: Rdf rdf
  => (Triples -> Maybe BaseUrl -> PrefixMappings -> RDF rdf)
  -> Triples
  -> Maybe BaseUrl
  -> PrefixMappings
  -> Bool
p_query_match_none _mkRdf ts bUrl pms = uordered ts == uordered result
  where
    result = query (_mkRdf ts bUrl pms) Nothing Nothing Nothing

-- query with no wildcard and a triple in the RDF should yield
-- a singleton list with just the triple.
p_query_match_spo
  :: Rdf rdf
  => RDF rdf
  -> RDF rdf
  -> Property
p_query_match_spo _unused rdf =
  classify (null ts) "trivial" $ forAll (tripleFromGen triplesOf rdf) f
  where
    ts = triplesOf rdf
    f t =
      case t of
        Nothing -> True
        (Just t') -> [t'] == queryT rdf t'

{- disabled:
-- removing duplicates from `query` (and `select`) is deprecated, see
--  https://github.com/cordawyn/rdf4h/commit/9dd4729908db8d2f80088706592adac81a0f3016
--
-- query as in p_query_matched_spo after duplicating a triple in the
-- RDF, so that we can verify that the results just have 1, even
-- if the RDF itself doesn't ensure that there are no dupes internally.
p_query_matched_spo_no_dupes :: RDF rdf => (RDF rdf -> Triples) -> (Triples -> Maybe BaseUrl -> PrefixMappings -> rdf) -> rdf -> Property
p_query_matched_spo_no_dupes _triplesOf _mkRdf rdf =
  classify (null ts) "trivial" $
    forAll (tripleFromGen _triplesOf rdf) f
  where
    ts = _triplesOf rdf
    f t = case t of
            Nothing   -> True
            Just t'   -> [t'] == queryT (mkRdfWithDupe _triplesOf _mkRdf rdf t') t'
-}

-- query with no wildcard and a triple no in the RDF should yield []
p_query_unmatched_spo
  :: Rdf rdf
  => RDF rdf -> Triple -> Property
p_query_unmatched_spo rdf t =
  classify (t `elem` ts) "ignored" $ notElem t ts ==> [] == queryT rdf t
  where
    ts = triplesOf rdf

-- query with fixed subject and wildcards for pred and obj should yield
-- a list with all triples having subject, and RDF minus result triples
-- should yield all triple with unequal subjects.
p_query_match_s
  :: Rdf rdf
  => RDF rdf -> Property
p_query_match_s = mk_query_match_fn sameSubj f
  where
    f t = (Just (subjectOf t), Nothing, Nothing)

-- query with fixed predicate and wildcards for subj and obj should yield
-- a list with all triples having predicate, and a RDF graph minus result triples
-- should yield all triple with unequal predicates.
p_query_match_p
  :: Rdf rdf
  => RDF rdf -> RDF rdf -> Property
p_query_match_p _unused = mk_query_match_fn samePred f
  where
    f t = (Nothing, Just (predicateOf t), Nothing)

-- likewise for fixed subject and predicate with object wildcard
p_query_match_o
  :: Rdf rdf
  => RDF rdf -> RDF rdf -> Property
p_query_match_o _unused = mk_query_match_fn sameObj f
  where
    f t = (Nothing, Nothing, Just (objectOf t))

-- verify likewise for fixed subject and predicate with wildcard object
p_query_match_sp
  :: Rdf rdf
  => RDF rdf -> RDF rdf -> Property
p_query_match_sp _unsed = mk_query_match_fn same f
  where
    same t1 t2 = sameSubj t1 t2 && samePred t1 t2
    f t = (Just $ subjectOf t, Just $ predicateOf t, Nothing)

-- fixed subject and object with wildcard predicate
p_query_match_so
  :: Rdf rdf
  => RDF rdf -> RDF rdf -> Property
p_query_match_so _unused = mk_query_match_fn same f
  where
    same t1 t2 = sameSubj t1 t2 && sameObj t1 t2
    f t = (Just $ subjectOf t, Nothing, Just $ objectOf t)

-- fixed predicate and object with wildcard subject
p_query_match_po
  :: Rdf rdf
  => RDF rdf -> RDF rdf -> Property
p_query_match_po _unused = mk_query_match_fn same f
  where
    same t1 t2 = samePred t1 t2 && sameObj t1 t2
    f t = (Nothing, Just $ predicateOf t, Just $ objectOf t)

{-
 This function:

 1) creates a random RDF graph.
 2) extracts a random triple from the graph.
 3) queries the graph for that triple (according to the
    (Maybe Node, Maybe Node, Maybe Node) pattern specified
    by the mkPatternFn function.
 4) extracts all triples in the graph that was not returned by
    the query above.
 5) checks that all triples returned by the query match the triple
    comparison function (sameSubj, samePred, sameObj).
 6) checks that all triples not returned by the query do not
    match the tripl comparison function.
-}
mk_query_match_fn
  :: Rdf rdf
  => (Triple -> Triple -> Bool)
  -> (Triple -> (Maybe Node, Maybe Node, Maybe Node))
  -> RDF rdf
  -> Property
mk_query_match_fn tripleCompareFn mkPatternFn rdf =
  forAll (tripleFromGen triplesOf rdf) f
  where
    f :: Maybe Triple -> Bool
    f Nothing = True
    f (Just t) =
      let all_ts = triplesOf rdf
          all_ts_sorted = uordered all_ts
          results = uordered $ queryC rdf (mkPatternFn t)
          -- `notResults` is the difference of the two sets of triples
          notResults = ldiff all_ts_sorted results
      in all (tripleCompareFn t) results &&
         all (not . tripleCompareFn t) notResults

p_select_match_none
  :: Rdf rdf
  => RDF rdf -> Bool
p_select_match_none rdf = sort ts1 == sort ts2
  where
    ts1 = select rdf Nothing Nothing Nothing
    -- ts2 = (nub . triplesOf) rdf
    -- may have duplicates, see comments in
    --   https://github.com/cordawyn/rdf4h/commit/9dd4729908db8d2f80088706592adac81a0f3016
    ts2 = triplesOf rdf

p_select_match_s
  :: Rdf rdf
  => RDF rdf -> Property
p_select_match_s = p_select_match_fn same mkPattern triplesOf
  where
    same = equivNode (==) subjectOf
    mkPattern t = (Just (\n -> n == subjectOf t), Nothing, Nothing)

p_select_match_p
  :: Rdf rdf
  => RDF rdf -> Property
p_select_match_p = p_select_match_fn same mkPattern triplesOf
  where
    same = equivNode equiv predicateOf
    equiv (UNode u1) (UNode u2) = T.last u1 == T.last u2
    equiv _ _ = error "GraphTestUtils.p_select_match_p.equiv"
    mkPattern t =
      (Nothing, Just (\n -> lastChar n == lastChar (predicateOf t)), Nothing)
    lastChar (UNode uri) = T.last uri
    lastChar _ = error "GraphTestUtils.p_select_match_p.lastChar"


p_select_match_o :: Rdf rdf => RDF rdf -> Property
p_select_match_o =
  p_select_match_fn same mkPattern triplesOf
  where
    same = equivNode (/=) objectOf
    mkPattern t = (Nothing, Nothing, Just (\n -> n /= objectOf t))

p_select_match_sp :: Rdf rdf => RDF rdf -> Property
p_select_match_sp =
  p_select_match_fn same mkPattern triplesOf
  where
    same t1 t2 = subjectOf t1 == subjectOf t2 && predicateOf t1 /= predicateOf t2
    mkPattern t = (Just (\n -> n == subjectOf t), Just (\n -> n /= predicateOf t), Nothing)

p_select_match_so
  :: Rdf rdf
  => RDF rdf -> Property
p_select_match_so = p_select_match_fn same mkPattern triplesOf
  where
    same t1 t2 = subjectOf t1 /= subjectOf t2 && objectOf t1 == objectOf t2
    mkPattern t =
      (Just (\n -> n /= subjectOf t), Nothing, Just (\n -> n == objectOf t))

p_select_match_po
  :: Rdf rdf
  => RDF rdf -> RDF rdf -> Property
p_select_match_po _unsed = p_select_match_fn same mkPattern triplesOf
  where
    same t1 t2 = predicateOf t1 == predicateOf t2 && objectOf t1 == objectOf t2
    mkPattern t =
      (Nothing, Just (\n -> n == predicateOf t), Just (\n -> n == objectOf t))

p_select_match_spo
  :: (Rdf rdf)
  => RDF rdf -> RDF rdf -> Property
p_select_match_spo _unused = p_select_match_fn same mkPattern triplesOf
  where
    same t1 t2 =
      subjectOf t1 == subjectOf t2 &&
      predicateOf t1 == predicateOf t2 && objectOf t1 /= objectOf t2
    mkPattern t =
      ( Just (\n -> n == subjectOf t)
      , Just (\n -> n == predicateOf t)
      , Just (\n -> n /= objectOf t))

-- |adding a triple to a graph.
p_add_triple
  :: (Rdf rdf)
  => (Triples -> Maybe BaseUrl -> PrefixMappings -> RDF rdf)
  -> Triples
  -> Maybe BaseUrl
  -> PrefixMappings
  -> Triple -- ^ new triple to be added
  -> Bool
p_add_triple _mkRdf ts bUrl pms newTriple =
  uordered (newTriple : triplesOf oldGr) == uordered (triplesOf newGr)
  where
    oldGr = _mkRdf ts bUrl pms
    newGr = addTriple oldGr newTriple

-- |removing a triple that may or may not be in a graph. If it is not,
--  this operation is silently ignored.
p_remove_triple
  :: (Rdf rdf)
  => (Triples -> Maybe BaseUrl -> PrefixMappings -> RDF rdf)
  -> Triples
  -> Maybe BaseUrl
  -> PrefixMappings
  -> Triple -- ^ triple to be removed
  -> Bool
p_remove_triple _mkRdf ts bUrl pms tripleToBeRemoved =
  uordered (filter (/= tripleToBeRemoved) oldTriples) == uordered newTriples
  where
    oldGr = _mkRdf ts bUrl pms
    newGr = removeTriple oldGr tripleToBeRemoved
    oldTriples = triplesOf oldGr
    newTriples = triplesOf newGr

-- |removing a triple from a graph. The new graph should not contain
-- any instances of the triple.
p_remove_triple_from_graph
  :: (Rdf rdf)
  => (Triples -> Maybe BaseUrl -> PrefixMappings -> RDF rdf)
  -> Triples
  -> Maybe BaseUrl
  -> PrefixMappings
  -> Property
p_remove_triple_from_graph _mkRdf ts bUrl pms =
  classify (null ts) "p_remove_triple_from_graph tests were trivial" $
  forAll (tripleFromGen triplesOf (_mkRdf ts bUrl pms)) f
  where
    f :: Maybe Triple -> Bool
    f Nothing = True
    f (Just tripleToBeRemoved) =
      p_remove_triple _mkRdf ts bUrl pms tripleToBeRemoved

-- TODO: refactor the following 3 functions.

-- |removing a triple from a graph that contained only that triple.
-- Performing a ((Just s) Nothing Nothing) query should return an
-- empty list.
p_remove_triple_from_singleton_graph_query_s
  :: (Rdf rdf)
  => RDF rdf -> SingletonGraph rdf -> Bool
p_remove_triple_from_singleton_graph_query_s _unused singletonGraph =
  null (query newGr (Just s) Nothing Nothing)
  where
    tripleInGraph@(Triple s _p _o) = head (triplesOf (rdfGraph singletonGraph))
    newGr = removeTriple (rdfGraph singletonGraph) tripleInGraph

-- |removing a triple from a graph that contained only that triple.
-- Performing a (Nothing (Just p) Nothing) query should return an
-- empty list.
p_remove_triple_from_singleton_graph_query_p
  :: (Rdf rdf)
  => RDF rdf -> SingletonGraph rdf -> Bool
p_remove_triple_from_singleton_graph_query_p _unused singletonGraph =
  null (query newGr Nothing (Just p) Nothing)
  where
    tripleInGraph@(Triple _s p _o) = head (triplesOf (rdfGraph singletonGraph))
    newGr = removeTriple (rdfGraph singletonGraph) tripleInGraph

-- |removing a triple from a graph that contained only that triple.
-- Performing a (Nothing Nothing (Just o)) query should return an
-- empty list.
p_remove_triple_from_singleton_graph_query_o
  :: (Rdf rdf)
  => RDF rdf -> SingletonGraph rdf -> Bool
p_remove_triple_from_singleton_graph_query_o _unused singletonGraph =
  null (query newGr Nothing Nothing (Just o))
  where
    tripleInGraph@(Triple _s _p o) = head (triplesOf (rdfGraph singletonGraph))
    newGr = removeTriple (rdfGraph singletonGraph) tripleInGraph

p_add_then_remove_triples
  :: (Rdf rdf)
  => RDF rdf -- ^ empty
  -> Triples -- ^ triples to add then remove
  -> Bool
p_add_then_remove_triples _empty genTriples =
  let emptyGraph = _empty
      populatedGraph =
        foldr (flip addTriple) emptyGraph genTriples
      emptiedGraph =
        foldr (flip removeTriple) populatedGraph genTriples
  in null (triplesOf emptiedGraph)

equivNode :: (Node -> Node -> Bool)
          -> (Triple -> Node)
          -> Triple
          -> Triple
          -> Bool
equivNode eqFn exFn t1 t2 = exFn t1 `eqFn` exFn t2

p_select_match_fn
  :: Rdf rdf
  => (Triple -> Triple -> Bool)
  -> (Triple -> (NodeSelector, NodeSelector, NodeSelector))
  -> (RDF rdf -> Triples)
  -> RDF rdf
  -> Property
p_select_match_fn tripleCompareFn mkPatternFn _triplesOf rdf =
  forAll (tripleFromGen _triplesOf rdf) f
  where
    f :: Maybe Triple -> Bool
    f Nothing = True
    f (Just t) =
      let all_ts = triplesOf rdf
          all_ts_sorted = uordered all_ts
          results = uordered $ selectC rdf (mkPatternFn t)
          notResults = ldiff all_ts_sorted results
      in all (tripleCompareFn t) results &&
         all (not . tripleCompareFn t) notResults

-- Utility functions and test data ... --

-- a curried version of query that delegates to the actual query after unpacking
-- curried maybe node pattern.
queryC
  :: Rdf rdf
  => RDF rdf -> (Maybe Node, Maybe Node, Maybe Node) -> Triples
queryC rdf (s, p, o) = query rdf s p o

selectC
  :: Rdf rdf
  => RDF rdf -> (NodeSelector, NodeSelector, NodeSelector) -> Triples
selectC rdf (s, p, o) = select rdf s p o

ldiff :: Triples -> Triples -> Triples
ldiff l1 l2 = Set.toList $(Set.fromList l1) `Set.difference` Set.fromList l2

sameSubj :: Triple -> Triple -> Bool
sameSubj t1 t2 = subjectOf t1 == subjectOf t2

samePred :: Triple -> Triple -> Bool
samePred t1 t2 = predicateOf t1 == predicateOf t2

sameObj :: Triple -> Triple -> Bool
sameObj  t1 t2 = objectOf t1 == objectOf t2

-- |pick a random triple from an RDF graph if the graph is not empty.
tripleFromGen
  :: (RDF rdf -> Triples) -> RDF rdf -> Gen (Maybe Triple)
tripleFromGen _triplesOf rdf =
  if null ts
    then return Nothing
    else oneof $ fmap (return . Just) ts
  where
    ts = _triplesOf rdf

queryT
  :: Rdf rdf
  => RDF rdf -> Triple -> Triples
queryT rdf t =
  query rdf (Just $ subjectOf t) (Just $ predicateOf t) (Just $ objectOf t)

languages :: [T.Text]
languages = [T.pack "fr", T.pack "en"]

datatypes :: [T.Text]
datatypes = fmap (mkUri xsd . T.pack) ["string", "int", "token"]

uris :: [T.Text]
uris =  [mkUri ex (n <> T.pack (show (i :: Int)))
        | n <- ["foo", "bar", "quz", "zak"], i <- [0 .. 2]]
     <> ["ex:" <> n <> T.pack (show (i::Int))
        | n <- ["s", "p", "o"], i <- [1..3]]

plainliterals :: [LValue]
plainliterals = [plainLL lit lang | lit <- litvalues, lang <- languages]

typedliterals :: [LValue]
typedliterals = [typedL lit dtype | lit <- litvalues, dtype <- datatypes]

litvalues :: [T.Text]
litvalues = fmap T.pack ["hello", "world", "peace", "earth", "", "haskell"]

unodes :: [Node]
unodes = fmap UNode uris

bnodes :: [ Node]
bnodes = fmap (BNode . \i -> T.pack ":_genid" <> T.pack (show (i::Int))) [1..5]

lnodes :: [Node]
lnodes = [LNode lit | lit <- plainliterals <> typedliterals]

-- maximum number of triples
maxN :: Int
maxN = 10

instance (Rdf rdf) => Arbitrary (RDF rdf) where
  arbitrary = do
    prefix <- arbitraryPrefixMappings
    baseU' <- arbitraryBaseUrl
    baseU <- oneof [return (Just baseU'), return Nothing]
    ts <- arbitraryTs
    return $ mkRdf ts baseU prefix

instance Arbitrary Triple where
  arbitrary = do
    s <- arbitraryS
    p <- arbitraryP
    triple s p <$> arbitraryO

instance Arbitrary Node where
  arbitrary = oneof $ fmap return unodes

arbitraryTs :: Gen Triples
arbitraryTs = do
  n <- sized (\_ -> choose (0, maxN))
  sequence [arbitrary | _ <- [1 .. n]]

arbitraryS, arbitraryP, arbitraryO :: Gen Node
arbitraryS = oneof $ fmap return $ unodes <> bnodes
arbitraryP = oneof $ fmap return unodes
arbitraryO = oneof $ fmap return $ unodes <> bnodes <> lnodes

----------------------------------------------------
--  Unit test cases                               --
----------------------------------------------------

-- Reported by Daniel Bergey:
--   https://github.com/robstewart57/rdf4h/issues/4

p_reverseRdfTest
  :: Rdf a
  => (Triples -> Maybe BaseUrl -> PrefixMappings -> RDF a) -> Property
p_reverseRdfTest _mkRdf =
  monadicIO $ do
    fileContents <- Test.QuickCheck.Monadic.run $ bracket
      (openTempFile "." "tmp")
      (\(path, h) -> hClose h >> removeFile path)
      (\(_, h) -> do
        hSetEncoding h utf8
        hWriteRdf NTriplesSerializer h rdf
        hSeek h AbsoluteSeek 0
        T.hGetContents h)
    assert $ expected == fileContents
  where
    rdf = _mkRdf ts (Just $ BaseUrl "file://") (ns_mappings mempty)
    ts :: [Triple]
    ts =
      [ Triple
          (unode "file:///this/is/not/a/palindrome")
          (unode "file:///this/is/not/a/palindrome")
          (LNode . PlainL $ "literal string")
      ]
    expected = "<file:///this/is/not/a/palindrome> \
               \<file:///this/is/not/a/palindrome> \
               \\"literal string\" .\n"
