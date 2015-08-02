module Data.RDF.GraphTestUtils where

import Control.Applicative ((<$>))
import Data.ByteString (pack)
import qualified Data.ByteString.Char8 as C
import Data.Knob
import Data.RDF.Types
import Data.RDF.Query
import Data.RDF.Namespace
import Text.RDF.RDF4H.NTriplesSerializer
import qualified Data.Text.Lazy as T
import Test.QuickCheck
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import System.IO.Unsafe(unsafePerformIO)
import System.IO

import Test.Framework (Test,TestName,testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Monadic (assert, monadicIO,run)

----------------------------------------------------
--  property based quick check test cases         --
----------------------------------------------------

graphTests :: forall rdf. (Arbitrary rdf, RDF rdf, Show rdf)
           => TestName -> (rdf -> Triples) -> (rdf -> Triples) -> rdf -> (Triples -> Maybe BaseUrl -> PrefixMappings -> rdf) -> [Test]
graphTests testGroupName _triplesOf _uniqTriplesOf _empty _mkRdf = [ testGroup testGroupName
            [
              testProperty "empty"                      (p_empty _triplesOf _empty)
            , testProperty "mkRdf_triplesOf"            (p_mkRdf_triplesOf _triplesOf _mkRdf)
            , testProperty "mkRdf_no_dupes"             (p_mkRdf_no_dupes _uniqTriplesOf _mkRdf)
            , testProperty "query_match_none"           (p_query_match_none _mkRdf)
            , testProperty "query_matched_spo"          (p_query_matched_spo _triplesOf)
            -- see comment above p_query_matched_spo_no_dupes for why this is disabled
            -- , testProperty "query_matched_spo_no_dupes" (p_query_matched_spo_no_dupes _triplesOf _mkRdf)
            , testProperty "query_unmatched_spo"        (p_query_unmatched_spo _triplesOf)
            , testProperty "query_match_s"              (p_query_match_s _triplesOf)
            , testProperty "query_match_p"              (p_query_match_p _triplesOf)
            , testProperty "query_match_o"              (p_query_match_o _triplesOf)
            , testProperty "query_match_sp"             (p_query_match_sp _triplesOf)
            , testProperty "query_match_so"             (p_query_match_so _triplesOf)
            , testProperty "query_match_po"             (p_query_match_po _triplesOf)
            , testProperty "select_match_none"                 (p_select_match_none _triplesOf)
            , testProperty "select_match_s"             (p_select_match_s _triplesOf)
            , testProperty "select_match_p"             (p_select_match_p _triplesOf)
            , testProperty "select_match_o"             (p_select_match_o _triplesOf)
            , testProperty "select_match_sp"            (p_select_match_sp _triplesOf)
            , testProperty "select_match_so"            (p_select_match_so _triplesOf)
            , testProperty "select_match_po"            (p_select_match_po _triplesOf)
            , testProperty "select_match_spo"           (p_select_match_spo _triplesOf)
            , testProperty "reversed RDF handle write"  (p_reverseRdfTest _mkRdf)
            ]
        ]


instance Arbitrary BaseUrl where
  arbitrary = oneof $ map (return . BaseUrl . T.pack) ["http://example.com/a", "http://asdf.org/b"]
  --coarbitrary = undefined

instance Arbitrary PrefixMappings where
  arbitrary = oneof [return $ PrefixMappings Map.empty, return $ PrefixMappings $
                          Map.fromAscList [(T.pack "eg1", T.pack "http://example.com/1"),
                                        (T.pack "eg2", T.pack "http://example.com/2")]]
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

-- duplicate input triples should not be returned when
-- uniqTriplesof is used
p_mkRdf_no_dupes :: RDF rdf => (rdf -> Triples) -> (Triples -> Maybe BaseUrl -> PrefixMappings -> rdf) -> Triples -> Maybe BaseUrl -> PrefixMappings -> Bool
p_mkRdf_no_dupes _uniqtriplesOf _mkRdf ts bUrl pms =
  null ts || (sort result == uordered ts)
   where
    tsWithDupe = head ts : ts
    result = _uniqtriplesOf $ _mkRdf tsWithDupe bUrl pms

-- Note: in TriplesGraph and PatriciaTreeGraph `query` expands triples
--       but `ts` here is not  necessarily expanded. What is the correct
--       property this test should check?
--
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

{- disabled:
-- removing duplicates from `query` (and `select`) is deprecated, see
--  https://github.com/cordawyn/rdf4h/commit/9dd4729908db8d2f80088706592adac81a0f3016
--
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
-}

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

p_select_match_none :: RDF rdf => (rdf -> Triples) -> rdf -> Bool
p_select_match_none _triplesOf_not_used rdf = sort ts1 == sort ts2
    where
      ts1 = select rdf Nothing Nothing Nothing
      -- ts2 = (nub . triplesOf) rdf

      -- may have duplicates, see comments in
      --   https://github.com/cordawyn/rdf4h/commit/9dd4729908db8d2f80088706592adac81a0f3016
      ts2 = triplesOf rdf

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
uordered  =  sort . nub

tripleFromGen :: RDF rdf => (rdf -> Triples) -> rdf -> Gen (Maybe Triple)
tripleFromGen _triplesOf rdf =
  if null ts
  then return Nothing
  else oneof $ map (return . Just) ts
   where ts = _triplesOf rdf

queryT :: RDF rdf => rdf -> Triple -> Triples
queryT rdf t = query rdf (Just $ subjectOf t) (Just $ predicateOf t) (Just $ objectOf t)

languages :: [T.Text]
languages = [T.pack "fr", T.pack "en"]

datatypes :: [T.Text]
datatypes = map (mkUri xsd . T.pack) ["string", "int", "token"]

uris :: [T.Text]
uris = map (mkUri ex) [T.pack n `T.append` T.pack (show (i::Int)) | n <- ["foo", "bar", "quz", "zak"], i <- [0..9]]

plainliterals :: [LValue]
plainliterals = [plainLL lit lang | lit <- litvalues, lang <- languages]

typedliterals :: [LValue]
typedliterals = [typedL lit dtype | lit <- litvalues, dtype <- datatypes]

litvalues :: [T.Text]
litvalues = map T.pack ["hello", "world", "peace", "earth", "", "haskell"]

unodes :: [Node]
unodes = map UNode uris

bnodes :: [ Node]
bnodes = map (BNode . \i -> T.pack ":_genid" `T.append` T.pack (show (i::Int))) [1..5]

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

----------------------------------------------------
--  Unit test cases                               --
----------------------------------------------------

-- Reported by Daniel Bergey:
--   https://github.com/robstewart57/rdf4h/issues/4

p_reverseRdfTest :: RDF rdf => (Triples -> Maybe BaseUrl -> PrefixMappings -> rdf) -> Property
p_reverseRdfTest _mkRdf = monadicIO $ do
    fileContents <- run $ do
      knob <- newKnob (pack [])
      h <- newFileHandle knob "test.rdf" WriteMode
      hWriteRdf NTriplesSerializer h rdfGraph
      hClose h
      C.unpack <$> Data.Knob.getContents knob
    let expected = "<file:///this/is/not/a/palindrome> <file:///this/is/not/a/palindrome> \"literal string\" .\n"
    assert $ expected == fileContents

  where
    rdfGraph = _mkRdf ts (Just $ BaseUrl "file://") (ns_mappings [])

    ts :: [Triple]
    ts = [Triple
           (unode "file:///this/is/not/a/palindrome")
           (unode "file:///this/is/not/a/palindrome")
           (LNode . PlainL . T.pack $ "literal string")]
