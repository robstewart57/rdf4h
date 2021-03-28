{-# LANGUAGE OverloadedStrings #-}

module Data.RDF.BlankNodeTests
  ( blankNodeTests,
  )
where

import Data.Either
import Data.RDF.IRI
import Data.RDF.Types
import Data.Text ()
import Test.Tasty
import Test.Tasty.HUnit

blankNodeTests :: TestTree
blankNodeTests =
  testGroup
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
    ]
