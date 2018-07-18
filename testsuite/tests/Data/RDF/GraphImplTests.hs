{-# LANGUAGE OverloadedStrings #-}

module Data.RDF.GraphImplTests (graphImplTests) where

import Data.RDF

import qualified Data.Map as Map
import Test.Tasty
import qualified Test.Tasty.HUnit as TU


----------------------------------------------------
--  unit test cases         --
----------------------------------------------------

graphImplTests :: TestTree
graphImplTests =
  testGroup "graph-unit-tests"
  [
    testGroup "graph-impl-pred"
    [ testGroup "adjHashMap" [adjHashMap1,adjHashMap2]
    , testGroup "tlist" [tList1,tList2]
    ]
  ,
    testGroup "quickcheck-cases"
    [ testGroup "tlist" [tList3]
    ]
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
adjHashMap2 :: TestTree
adjHashMap2 =
  let g = mkRdf
            [ Triple (unode "ex:s1") (unode "ex:p1") (unode "ex:o1")
            , Triple (unode "ex:s1") (unode "ex:p2") (unode "ex:o2")
            ]
            Nothing
            (PrefixMappings Map.empty) :: RDF AdjHashMap
      res = query g Nothing (Just $ unode "ex:p1") Nothing
  in TU.testCase "adjHashMap2" $
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

-- https://github.com/robstewart57/rdf4h/issues/49
tList2 :: TestTree
tList2 =
  let g = mkRdf
            [ Triple (unode "ex:s1") (unode "ex:p1") (unode "ex:o1")
            , Triple (unode "ex:s1") (unode "ex:p2") (unode "ex:o2")
            ]
            Nothing
            (PrefixMappings Map.empty) :: RDF AdjHashMap
      res = query g Nothing (Just $ unode "ex:p1") Nothing
  in TU.testCase "tList1" $
     TU.assertBool
     "tList1 mkRdf test"
     (res
      ==
      [Triple (UNode "ex:s1") (UNode "ex:p1") (UNode "ex:o1")])

tList3 :: TestTree
tList3 =
  let ts =
        [ Triple (BNode ":_genid2") (UNode "http://www.example.org/bar1") (LNode (PlainLL "haskell" "en"))
        , Triple (UNode "ex:s1") (UNode "ex:o2") (LNode (TypedL "haskell" "http://www.w3.org/2001/XMLSchema#int"))
        , Triple (UNode "ex:p2") (UNode "http://www.example.org/bar1") (LNode (PlainLL "earth" "fr"))
        , Triple (UNode "ex:p3") (UNode "ex:s3") (LNode (PlainLL "earth" "fr"))
        , Triple (UNode "ex:p2") (UNode "http://www.example.org/bar1") (LNode (PlainLL "earth" "fr"))
        ]
      gr = mkRdf ts Nothing (PrefixMappings Map.empty) :: RDF TList
      res = query gr (Just (unode "ex:p2")) (Just (unode "http://www.example.org/bar1")) (Just (lnode (plainLL "earth" "fr")))
  in TU.testCase "tList2" $
     TU.assertBool
     "tList2"
     (res == [Triple (UNode "ex:p2") (UNode "http://www.example.org/bar1") (LNode (PlainLL "earth" "fr"))])
