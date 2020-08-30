{-# LANGUAGE OverloadedStrings #-}

module Text.RDF.RDF4H.TurtleSerializerTest (tests) where

import Data.ByteString as BS

import Data.RDF.Namespace
import Data.Function ((&))
import Data.Map as Map
import Data.RDF as RDF
import Text.RDF.RDF4H.TurtleSerializer.Internal
import System.IO
import System.IO.Temp (withSystemTempFile)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Turtle serializer tests"
  [ testGroup "findMappings Tests"
    [ testCase "findMapping correctly finds rdf mapping" $
      assertEqual "" (Just ("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "subject")) (findMapping standard_ns_mappings "rdf:subject")
    , testCase "findMapping correctly finds rdfs mapping" $
      assertEqual "" (Just ("http://www.w3.org/2000/01/rdf-schema#", "domain")) (findMapping standard_ns_mappings "rdfs:domain")]

  , testGroup "writeUNodeUri tests"
    [ testCase "Serialization of QName UNode where prefix exists in PrefixMappings should not contain < or >" $
      withSystemTempFile "rdf4h-"
      (\_ h -> do
          writeUNodeUri h "rdf:subject" standard_ns_mappings
          hSeek h AbsoluteSeek 0
          contents <- BS.hGetContents h
          "rdf:subject" @=? contents)
    , testCase "Serialization of QName UNode where prefix does not exist in PrefixMappings should not contain < or >" $
      withSystemTempFile "rdf4h-"
      (\_ h -> do
          writeUNodeUri h "foo:subject" standard_ns_mappings
          hSeek h AbsoluteSeek 0
          contents <- BS.hGetContents h
          "foo:subject" @=? contents)
    , testCase "Serialization of non-namespaced UNode should be wrapped in < and >" $
      withSystemTempFile "rdf4h-"
      (\_ h -> do
          writeUNodeUri h "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject" standard_ns_mappings
          hSeek h AbsoluteSeek 0
          contents <- BS.hGetContents h
          "<http://www.w3.org/1999/02/22-rdf-syntax-ns#subject>" @=? contents)
    ]

  , testGroup "writeRdf tests"
    [ testCase "triples with the same subject should be grouped" $
      let g :: RDF TList
          g = RDF.empty
            & flip addTriple (triple (unode ":something") (unode "rdf:type") (unode "schema:Document"))
            & flip addTriple (triple (unode ":another") (unode "rdf:type") (unode "schema:Document"))
            & flip addTriple (triple (unode ":something") (unode "dc:title") (lnode (plainL "Some title")))
          mappings = PrefixMappings $ Map.fromList [ ("schema", "http://schema.org/")
                                                   , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                                                   , ("dc", "http://purl.org/dc/elements/1.1/")]
          serializer = TurtleSerializer Nothing mappings
      in
      withSystemTempFile "rdf4h-"
      (\_ h -> do
          expected <- BS.readFile "testsuite/tests/Text/RDF/RDF4H/data/common-subject.ttl"
          hWriteRdf serializer h g
          hSeek h AbsoluteSeek 0
          actual <- BS.hGetContents h
          expected @=? actual)
    ]
  ]
