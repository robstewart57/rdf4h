{-# LANGUAGE OverloadedStrings #-}

module Text.RDF.RDF4H.TurtleSerializerTest (tests) where

import Data.ByteString as BS
import Data.Coerce
import Data.RDF.Namespace
import System.IO
import System.IO.Temp (withSystemTempFile)
import Test.Tasty
import Test.Tasty.HUnit
import Text.RDF.RDF4H.TurtleSerializer

tests :: TestTree
tests = testGroup "Turtle serializer tests"
  [ testGroup "findMappings Tests"
    [ testCase "findMapping correctly finds rdf mapping" $
      assertEqual "" (Just ("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "subject")) (findMapping (coerce standard_ns_mappings) "rdf:subject")
    , testCase "findMapping correctly finds rdfs mapping" $
      assertEqual "" (Just ("http://www.w3.org/2000/01/rdf-schema#", "domain")) (findMapping standard_ns_mappings "rdfs:domain")]

  , testGroup "writeUNodeUri tests"
    [ testCase "should properly serialize a UNode" $
        withSystemTempFile "rdf4h-"
          (\_ h -> do
              writeUNodeUri h "rdf:subject" standard_ns_mappings
              hSeek h AbsoluteSeek 0
              contents <- BS.hGetContents h
              "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject" @=? contents)]
  ]
