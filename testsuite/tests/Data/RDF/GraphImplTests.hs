{-# LANGUAGE OverloadedStrings #-}

module Data.RDF.GraphImplTests (graphImplTests) where

import Data.RDF

import Test.Tasty
import qualified Test.Tasty.HUnit as TU


----------------------------------------------------
--  unit test cases         --
----------------------------------------------------

graphImplTests
  :: TestTree
graphImplTests =
  testGroup "rdf-graph-impl"
  [ testGroup "adjHashMap" [adjHashMap1]
  , testGroup "tlist" [tList1]
  ]

-- https://github.com/robstewart57/rdf4h/issues/49
adjHashMap1 :: TestTree
adjHashMap1 =
  let content = "PREFIX ex: <ex:> ex:s1  ex:p1 ex:o1 ;  ex:p2 ex:o2  ."
      Right g = parseString (TurtleParser Nothing Nothing) content :: Either ParseFailure (RDF AdjHashMap)
      res = query g Nothing (Just $ unode "ex:p1") Nothing
  in TU.testCase "adjHashMap1" $
     TU.assertBool
     "adjHashMap1 mkRdf test"
     (res
      ==
      [Triple (UNode "ex:s1") (UNode "ex:p1") (UNode "ex:o1")])

-- https://github.com/robstewart57/rdf4h/issues/49
tList1 :: TestTree
tList1 =
  let content = "PREFIX ex: <ex:> ex:s1  ex:p1 ex:o1 ;  ex:p2 ex:o2  ."
      Right g = parseString (TurtleParser Nothing Nothing) content :: Either ParseFailure (RDF TList)
      res = query g Nothing (Just $ unode "ex:p1") Nothing
  in TU.testCase "tList1" $
     TU.assertBool
     "tList1 mkRdf test"
     (res
      ==
      [Triple (UNode "ex:s1") (UNode "ex:p1") (UNode "ex:o1")])
