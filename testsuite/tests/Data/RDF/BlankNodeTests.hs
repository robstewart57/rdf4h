{-# LANGUAGE OverloadedStrings #-}

module Data.RDF.BlankNodeTests
  ( blankNodeTests,
  )
where

import Data.RDF.Types
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

blankNodeTests ::
  (Rdf rdf) =>
  TestName ->
  -- | empty
  RDF rdf ->
  -- | _mkRdf
  (Triples -> Maybe BaseUrl -> PrefixMappings -> Maybe Int -> RDF rdf) ->
  TestTree
blankNodeTests testGroupName _rdf _mkRdf =
  testGroup
    ("blank-node-tests-" <> testGroupName)
    [ testGroup
        "blank-node-unit-tests"
        [ testCase "valid-bnode" $
            assertEqual
              ""
              (Just (BNode "foo"))
              (bnode "_:foo"),
          testCase "invalid-bnode1" $
            assertEqual
              ""
              Nothing
              (bnode "_foo"),
          testCase "invalid-bnode2" $
            assertEqual
              ""
              Nothing
              (bnode ":foo")
        ],
      testGroup
        "blankGen-node-property-tests"
        [ testProperty "bnodes-uniq" (newBNodeGenUnique _rdf),
          testProperty "bnodes-1st-gen-zero" (firstBNodeGenZero _rdf)
        ]
    ]

firstBNodeGenZero ::
  Rdf rdf =>
  RDF rdf ->
  Bool
firstBNodeGenZero _emptyRdf =
  let (n, _g1) = bnodeGen _emptyRdf
   in n == BNodeGen 0

newBNodeGenUnique ::
  Rdf rdf =>
  RDF rdf ->
  Bool
newBNodeGenUnique rdf =
  noSameBNodes rdf rdf1
  where
    (_generatedBNode, rdf1) = bnodeGen rdf
    noSameBNodes :: (Rdf rdf) => RDF rdf -> RDF rdf -> Bool
    noSameBNodes g1 g2 =
      let bnodes1 = bnodes g1
          bnodes2 = bnodes g2
       in null (filter (\x -> x `notElem` bnodes2) bnodes1)
    bnodes g =
      concat (map bnodesFromTriple (triplesOf g))
    bnodesFromTriple :: Triple -> [Text]
    bnodesFromTriple (Triple s p o) =
      case s of
        BNode t -> [t]
        BNodeGen i -> [T.pack ("gen" <> show i)]
        _ -> []
        <> case p of
          BNode t -> [t]
          BNodeGen i -> [T.pack ("gen" <> show i)]
          _ -> []
        <> case o of
          BNode t -> [t]
          BNodeGen i -> [T.pack ("gen" <> show i)]
          _ -> []
