{-# LANGUAGE OverloadedStrings #-}

module Data.RDF.IRITests
  ( iriTests,
  )
where

import Data.Either
import Data.RDF.IRI
import Data.Text ()
import Test.Tasty
import Test.Tasty.HUnit

iriTests :: TestTree
iriTests =
  testGroup
    "iri-unit-tests"
    [ testCase "No scheme 01" $
        assertEqual
          ""
          (Right (IRIRef Nothing authority02 mempty Nothing Nothing))
          (parseRelIRI "//duckduckgo.com"),
      testCase "No scheme 02" $
        assertBool "" (isRight (parseRelIRI "duckduckgo.com")),
      testCase "No path" $
        assertEqual
          ""
          (Right (IRIRef https authority01 mempty Nothing Nothing))
          (parseIRI "https://en.wikipedia.org"),
      testCase "Scheme case" $
        assertEqual
          ""
          (Right (IRIRef https authority01 mempty Nothing Nothing))
          (parseIRI "htTpS://en.wikipedia.org"),
      testCase "Empty query" $
        assertEqual
          ""
          (Right (IRIRef https authority02 mempty (Just mempty) Nothing))
          (parseIRI "https://duckduckgo.com?"),
      testCase "Empty fragment" $
        assertEqual
          ""
          (Right (IRIRef https authority02 mempty Nothing (Just mempty)))
          (parseIRI "https://duckduckgo.com#"),
      testCase "Empty query & fragment" $
        assertEqual
          ""
          (Right (IRIRef https authority02 mempty (Just mempty) (Just mempty)))
          (parseIRI "https://duckduckgo.com?#"),
      testCase "Simple query without path" $
        assertEqual
          ""
          (Right (IRIRef https authority02 mempty query01 Nothing))
          (parseIRI "https://duckduckgo.com?q=Uniform+Resource+Identifier"),
      testCase "Simple query with path" $
        assertEqual
          ""
          (Right (IRIRef https authority02 (Path "/") query01 Nothing))
          (parseIRI "https://duckduckgo.com/?q=Uniform+Resource+Identifier"),
      testCase "Japanese characters 01" $
        assertEqual
          ""
          (Right (IRIRef https authority03 mempty Nothing Nothing))
          (parseIRI "https://www.ほんとうにながいわけのわからないどめいんめいのらべるまだながくしないとたりない.w3.mag.keio.ac.jp"),
      testCase "Empty Filepath" $
        assertEqual
          ""
          (Right (IRIRef file emptyAuthority mempty Nothing Nothing))
          (parseIRI "file://"),
      testCase "Simple Filepath 01" $
        assertEqual
          ""
          (Right (IRIRef file emptyAuthority (Path "/") Nothing Nothing))
          (parseIRI "file:///"),
      testCase "Simple Filepath 02" $
        assertEqual
          ""
          (Right (IRIRef file emptyAuthority (Path "/temp") Nothing Nothing))
          (parseIRI "file:///temp"),
      testCase "Simple Filepath 03" $
        assertEqual
          ""
          (Right (IRIRef file emptyAuthority (Path "/temp/test.txt") Nothing Nothing))
          (parseIRI "file:///temp/test.txt"),
      testCase "Example for IRI resolution 1" $
        assertEqual
          ""
          (Right (IRIRef http authority04 (Path "/bb/ccc/d;p") (Just $ IRIQuery "q") Nothing))
          (parseIRI "http://a/bb/ccc/d;p?q"),
      testCase "Example for IRI resolution 2" $
        assertEqual
          ""
          (Right (IRIRef (Just $ Scheme "g") Nothing (Path "h") Nothing Nothing))
          (parseIRI "g:h"),
      testCase "Example for IRI resolution 3a" $
        assertBool "" (isLeft (parseIRI "g")),
      testCase "Example for IRI resolution 3b" $
        assertEqual
          ""
          (Right (IRIRef Nothing Nothing (Path "g") Nothing Nothing))
          (parseRelIRI "g"),
      testCase "Example for IRI resolution 4" $
        assertEqual
          ""
          (Right (IRIRef Nothing Nothing (Path "..") Nothing Nothing))
          (parseRelIRI ".."),
      testCase "IRI resolution 0" $
        assertEqual
          ""
          (Right "http://a/bb/ccc/d;p?q")
          (resolveIRI "http://a/bb/ccc/d;p?q" ""),
      testCase "IRI resolution 1" $
        assertEqual
          ""
          (Right "g:h")
          (resolveIRI "http://a/bb/ccc/d;p?q" "g:h"),
      testCase "IRI resolution 2" $
        assertEqual
          ""
          (Right "http://a/bb/ccc/g")
          (resolveIRI "http://a/bb/ccc/d;p?q" "g"),
      testCase "IRI resolution 3" $
        assertEqual
          ""
          (Right "http://a/bb/ccc/g")
          (resolveIRI "http://a/bb/ccc/d;p?q" "./g"),
      testCase "IRI resolution 4" $
        assertEqual
          ""
          (Right "http://a/bb/ccc/g/")
          (resolveIRI "http://a/bb/ccc/d;p?q" "g/"),
      testCase "IRI resolution 5" $
        assertEqual
          ""
          (Right "http://a/g")
          (resolveIRI "http://a/bb/ccc/d;p?q" "/g"),
      testCase "IRI resolution 6" $
        assertEqual
          ""
          (Right "http://a/bb/ccc/g/")
          (resolveIRI "http://a/bb/ccc/d;p?q" "./g/."),
      testCase "IRI resolution 7" $
        assertEqual
          ""
          (Right "http://a/bb/")
          (resolveIRI "http://a/bb/ccc/d;p?q" ".."),
      testCase "IRI resolution 8" $
        assertEqual
          ""
          (Right "http://a/g")
          (resolveIRI "http://a/bb/ccc/d;p?q" "../../../g"),
      testCase "IRI resolution 9" $
        assertEqual
          ""
          (Right "http://a.com/bb/ccc/test.ttl#")
          (resolveIRI "http://a.com/bb/ccc/test.ttl" "#"),
      testCase "IRI resolution 10" $
        assertEqual
          ""
          (Right "http://a/bb/ccc/d;p?q#")
          (resolveIRI "http://a/bb/ccc/d;p?q" "#")
    ]

http :: Maybe Scheme
http = Just $ Scheme "http"

https :: Maybe Scheme
https = Just $ Scheme "https"

file :: Maybe Scheme
file = Just $ Scheme "file"

emptyAuthority :: Maybe Authority
emptyAuthority = Just $ Authority Nothing (Host mempty) Nothing

authority01 :: Maybe Authority
authority01 = Just $ Authority Nothing host01 Nothing

authority02 :: Maybe Authority
authority02 = Just $ Authority Nothing host02 Nothing

authority03 :: Maybe Authority
authority03 = Just $ Authority Nothing host03 Nothing

authority04 :: Maybe Authority
authority04 = Just $ Authority Nothing (Host "a") Nothing

host01 :: Host
host01 = Host "en.wikipedia.org"

host02 :: Host
host02 = Host "duckduckgo.com"

host03 :: Host
host03 = Host "www.ほんとうにながいわけのわからないどめいんめいのらべるまだながくしないとたりない.w3.mag.keio.ac.jp"

query01 :: Maybe IRIQuery
query01 = Just $ IRIQuery "q=Uniform+Resource+Identifier"

{-
fragment01 :: Maybe Fragment
fragment01 = Just $ Fragment "top"
-}
