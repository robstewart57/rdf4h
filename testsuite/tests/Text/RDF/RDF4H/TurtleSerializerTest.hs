{-# LANGUAGE OverloadedStrings #-}

module Text.RDF.RDF4H.TurtleSerializerTest (tests) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import           Data.Function ((&))
import           Data.List (sort, nub)
import           Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.RDF as RDF
import qualified Data.Text as T
import           System.IO
import           System.IO.Temp (withSystemTempFile)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Text.RDF.RDF4H.QuickCheck ()
import           Text.RDF.RDF4H.TurtleSerializer.Internal

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

  , testGroup "QuickCheck Tests"
    [ testProperty "Serialized graph should have only one instance of each subject" (ioProperty . (prop_SingleSubject :: RDF TList -> IO Bool))
    ]
  ]

-- |This property ensures that a Turtle file generated from a graph will only
-- have a single instance of a given subject as the serialzer is supposed to
-- group all triples with a given subject.
prop_SingleSubject :: (Rdf rdf) => RDF rdf -> IO Bool
prop_SingleSubject g = withSystemTempFile "rdf4h-"
                       (\_ h -> do
                           hWriteRdf serializer h g
                           hSeek h AbsoluteSeek 0
                           contents <- BS.hGetContents h
                           pure $ assertSingleSubjects contents)
  where mappings = PrefixMappings $ Map.fromList [ ("schema", "http://schema.org/")
                                                 , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                                                 , ("dc", "http://purl.org/dc/elements/1.1/")
                                                 ]
        serializer = TurtleSerializer Nothing mappings

        -- Convert a Node to a string if it is a UNode or a BNode. This is
        -- acceptable since the arbitrary instance for Node's generate only
        -- those constructors for subjects and that's what this function is
        -- being used on.
        toUriString :: Node -> Maybe String
        toUriString (UNode uriText) = Just $ T.unpack uriText
        toUriString (BNode bid) = Just $ T.unpack bid
        toUriString _ = Nothing

        -- Convert the subjects for the given graph, g, to Strings.
        subjects :: [String]
        subjects = nub $ sort $ catMaybes $ toUriString <$> subjectOf <$> triplesOf g

        -- Count the number of times the first ByteString is found in the second
        -- ByteString.
        countMatches :: ByteString  -- ^The prefix
                     -> ByteString  -- ^The ByteString to search
                     -> Int
        countMatches needle haystack = countMatches' 0 needle haystack
          where countMatches' :: Int -> ByteString -> ByteString -> Int
                countMatches' n needle' haystack' =
                  case BS.breakSubstring needle' haystack' of
                    (_, r) | BS.null r -> n
                           | otherwise -> countMatches' (n+1) needle' (BS.drop (BS.length needle') r)

        -- Assert that the graph serialization (first parameter) contains
        -- exactly one instance of the given subject (second parameter).
        assertSingleSubject :: BS.ByteString -> String -> Bool
        assertSingleSubject bs subject = countMatches (Char8.pack subject) bs == 1

        -- Assert that each subject in the graph, g, is found in the
        -- serialization only once.
        assertSingleSubjects :: BS.ByteString -> Bool
        assertSingleSubjects bs = case subjects of
          [] -> True  -- Test should succeed for empty maps
          _ -> or $ assertSingleSubject bs <$> subjects
