{-# LANGUAGE OverloadedStrings #-}

module Data.RDF.TurtleSerializerTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Text.RDF.RDF4H.TurtleSerializer
import Data.RDF.Namespace
import Data.Coerce

tests :: TestTree
tests =
  testGroup "findMappings Tests"
  [ testCase "findMapping correctly finds a mapping" $
    assertEqual "" (findMapping (coerce standard_ns_mappings) "rdf:blah") (Just ("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "blah"))
  ]
