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
  [ testCase "findMapping correctly finds rdf mapping" $
    assertEqual "" (Just ("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "subject")) (findMapping (coerce standard_ns_mappings) "rdf:subject")
  , testCase "findMapping correctly finds rdfs mapping" $
    assertEqual "" (Just ("http://www.w3.org/2000/01/rdf-schema#", "domain")) (findMapping (coerce standard_ns_mappings) "rdfs:domain")
  ]
