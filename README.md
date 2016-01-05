rdf4h - An RDF library for Haskell
=====

[![Available on Hackage][badge-hackage]][hackage]
[![License BSD3][badge-license]][license]
[![Build Status][badge-travis]][travis]

[badge-travis]: https://travis-ci.org/robstewart57/rdf4h.png?branch=master
[travis]: https://travis-ci.org/robstewart57/rdf4h
[badge-hackage]: https://img.shields.io/hackage/v/rdf4h.svg
[hackage]: http://hackage.haskell.org/package/rdf4h
[badge-license]: https://img.shields.io/badge/license-BSD3-green.svg?dummy
[license]: https://github.com/robstewart57/rdf4h/blob/master/LICENSE.txt

rdf4h is a library for working with RDF in Haskell. It includes RDF
serializers for the N-Triples and Turtle formats, and parsing support
for RDF/XML, N-Triples and Turtle formats. The library provides an API
for querying for triples containing a particular subject, predicate,
or object, or selecting triples that satisfy an arbitrary predicate
function.

RDF graph implementations
-------------------------

The `RDF` type class provides a way to implement RDF graphs with a
variety of different concrete data structures. The idea is that the
selection of `RDF` implementation instances provides a trade off to
meet application requirements, e.g. querying mostly for subjects in an
RDF grpah, or querying mostly predicates and objects in an RDF
graph. The current `RDF` instances are:

* `HashMapSP`, which is a map from (S,P) pairs to a list of O's.
* `HashMapS`, which is a map from S to a map from P to a list of O's.
* `MapSP`, same as `HashMapSP`, but uses a `Data.HashMap` rather than a `Data.Map`.
* `TriplesList`, which is a standard list of `Triple` elements.
* `TriplesPatriciaTree`, which stores triples in a patricia tree from the `fgl` library.

The `RDF` class is:

```haskell
class RDF rdf where
  baseUrl           :: rdf -> Maybe BaseUrl
  prefixMappings    :: rdf -> PrefixMappings
  addPrefixMappings :: rdf -> PrefixMappings -> Bool -> rdf
  empty             :: rdf
  mkRdf             :: Triples -> Maybe BaseUrl -> PrefixMappings -> rdf
  triplesOf         :: rdf -> Triples
  uniqTriplesOf     :: rdf -> Triples
  select            :: rdf -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
  query             :: rdf -> Maybe Node -> Maybe Node -> Maybe Node -> Triples
```

How to parse RDF data
---------------------

The `RdfParser` is a type class for parsing RDF text in RDF/XML,
N-Triples and Turtle formats using instances `XmlParser`
`NTriplesParser` and `TurtleParser`. The API supports parsing from
Haskell strings, files and URLs.

```haskell
class RdfParser p where
  parseString :: RDF rdf => p -> Text   -> Either ParseFailure rdf
  parseFile   :: RDF rdf => p -> String -> IO (Either ParseFailure rdf)
  parseURL    :: RDF rdf => p -> String -> IO (Either ParseFailure rdf)
```

For example, to parse an NTriple file:

```haskell
Right (rdf::TriplesList) <- parseFile NTriplesParser "literal.nt"
```

To parse Turtle at a URL:

```haskell
Right (rdf::TriplesList) <- parseURL (TurtleParser Nothing Nothing)
                               "http://www.w3.org/2013/TurtleTests/LITERAL1.ttl"
```

How to serialise RDF data
---------------------

The `RdfSerializer` is a type class for serialising RDF graphs in
N-Triples and Turtle formats using instances `NTriplesSerializer` and
`TurtleSerializer`. The API supports writing RDF serialisations to
handlers for writing to files or sockets.

```haskell
class RdfSerializer s where
  hWriteRdf :: RDF rdf => s -> Handle -> rdf -> IO ()
```

For example, to write to an NTriples file:

```haskell
withFile "out.nt" WriteMode (\h -> hWriteRdf NTriplesSerializer h rdf)
```


RDF instance benchmarks
-----------------------

The following benchmark results were obtained running
bench/MainCriterion.hs at commit
https://github.com/robstewart57/rdf4h/commit/3d86295b085ecfb9a944dc7b40bb668df38c2777
on 28 December 2015. The complete output from criterion are in
`benchmark-results/criterion-results-28.12.2015.txt`.

To run the benchmark suite with cabal or stack:

    $ cabal bench
    $ stack bench

**Results:** mapping the `[Triple]` stream from the Turtle parser into
each RDF instance:

|                     | parse |
|---------------------|-------|
| HashMapS            | 19.7s |
| HashMapSP           | 19.5s
| MapSP               | 19.6s |
| TriplesList         | 18.6s |
| TriplesPatriciaTree | 22.0s |

Benchmarking `query` results in microseconds:

|                     | S | P | O | SP | PO | SO | SPO |
|---------------------|---|---|---|----|----|----|-----|
| HashMapS            | 29 | 2254 | 25640 | 0.5 | 0.3 | 4 | 0.5 |
| HashMapSP           | 20080 | 10170 | 13120 | 0.4 | 10130 | 19810 | 20080 |
| MapSP               | 9379 | 8518 | 1458 | 8 | 8639 | 9221 | 9379 |
| TriplesList         | 11100 | 7600 | 9600 | 11100 | 7600 | 11100 | 1100 |
| TriplesPatriciaTree | 206600 | 178700 | 188300 | 206800 |187400 | 174200 | 173600 |

Benchmarking `select` results in microseconds:

|                     | S | P | O | SP | PO | SO | SPO |
|---------------------|---|---|---|----|----|----|-----|
| HashMapS            | 275100 | 326400 | 112900 | 263700 | 111100 | 96680 | 95630 |
| HashMapSP           | 22080 | 24310 | 11830 | 24030 | 12210 | 15460 | 15690 |
| MapSP               | 12820 | 14610 | 13230 | 14950 | 14270 | 13420 | 14160 |
| TriplesList         | 12620 | 10730 | 9961 | 12760 | 11080 | 11790 | 13080 |
| TriplesPatriciaTree | 243300 | 238600 | 204100 | 251000 | 210300 | 209400 | 211400 |


Development wishlist
--------------------

* To pass 100% of the W3C parser unit tests for the XML, Turtle and
  NTriples parsers. The current pass rate is here:
  https://travis-ci.org/robstewart57/rdf4h

* To improve the benchmarked performance of the existing `RDF`
  instances, and to add new optimised instances of `RDF`. Pull
  requests of this kind especially welcome!

* Add new benchmarks to the criterion benchmark suite in the `bench/`
  directory. It currently benchmarks all `RDF` instances for `query`
  and `select` functions in the library. The benchmarks could cover
  more the library's API.

* The current `RDF` type class does not currently support the addition
  and removal of triples. The introduction of an API for allowing this
  would need a careful design, and a lot of new tests in the
  testsutie.

When adding a new RDF implementation, put your `RDF` type class
instance in an appropriately named Haskell module in
`Data/RDF/Graph/`. Now add your instance to the library testsuite and
the criterion benchmarks.

1. **Testsuite** add your instance to the testsuite by adding an
   appropriate file in `testsuite/tests/Data/RDF/Graph/` and hook in
   your instance to `Tests.hs`
   [here](https://github.com/robstewart57/rdf4h/blob/master/testsuite/tests/Test.hs).

2. **Benchmarks** add your instance to the criterion benchmarks in
   `MainCriterion.hs`
   [here](https://github.com/robstewart57/rdf4h/blob/master/bench/MainCriterion.hs).


Example
-------

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.RDF

rdfGraph1 :: IO TriplesList
rdfGraph1 = fmap fromEither (parseFile NTriplesParser "test1.nt")

rdfGraph2 :: IO TriplesList
rdfGraph2 = fmap fromEither (parseFile NTriplesParser "test2.nt")

example :: IO ()
example = do
  g1 <- rdfGraph1
  g2 <- rdfGraph2
  let node1 = lnode $ PlainL "foo"
  putStrLn $ "Subjects of g1: " ++ show (map subjectOf (triplesOf g1))
  putStrLn $ "RDF contains literal 'foo': " ++ show (rdfContainsNode g1 node1)
  putStrLn $ "Isomorphism test: " ++ show (isIsomorphic g1 g2)
  putStrLn $ "Unsorted triples: " ++ show (triplesOf g2)
  putStrLn $ "Sorted triples: "   ++ show ((sort . triplesOf) g2)
  putStrLn $ "Query: " ++ show (query g1 Nothing Nothing (Just node1))
```


Installation
------------

If you use the cabal-install Haskell library installer, then:

    $ cabal update
    $ cabal install rdf4h

Or just supply `rdf4h` in the `build-depends` field in the `.cabal`
file of your executable or library. The same applies for users of the
`stack` tool.


Running tests
-------------

Writing tests is highly encouraged. The library contains two kinds of
tests:

1. **W3C parser unit tests** for the XML, Turtle and NTriples formats
   using HUint. These tests remain in sync with
   https://github.com/w3c/rdf-tests .

2. **Property based API tests** for RDF graph querying using
   QuickCheck.

These tests are unifed with the
[tasty](https://hackage.haskell.org/package/tasty) test framework. It
provides a way of running all rdf4h library tests or just specific
test groups.

If you've never initialised the rdf-tests repository in your rdf4h clone:

    $ git submodule update --init --recursive

To run the parser tests against the latest W3C test files:

    $ git submodule foreach git pull origin gh-pages

To run all library tests with stack or cabal:

    $ stack test
    $ cabal configure --enable-tests && cabal build && cabal test

To run specific test groups:

    $ stack test --test-arguments="--pattern HashMapSP"
    $ cabal test --test-options="--pattern HashMapSP"

To list the available tests that can be run in isolation using a
pattern:

    $ stack test --test-arguments "--list-tests"
    $ cabal test --test-option "--list-tests" --show-details=streaming

For this outputted entry `rdf4h tests/N-Triples tests/nt-syntax-file-02`

    $ stack test --test-arguments="--pattern \"N-Triples tests/nt-syntax-file-02\""
    rdf4h-2.0.0: test (suite: test-rdf4h, args: --pattern "N-Triples tests/nt-syntax-file-02")
    rdf4h tests
      N-Triples tests
        nt-syntax-file-02: OK (0.02s)
    All 1 tests passed (1.06s)


Issues
------

Please use the GitHub [issue
tracker](https://github.com/robstewart57/rdf4h/issues) to report any
bugs you might find. Also feel free to use the GitHub issue tracker to
raise new discussions about the API design or proposals for additions
to the library.


Related Haskell Packages
----------------

* [hsparql](http://hackage.haskell.org/package/hsparql/) is a DSL for
  programmatic creation and execution of SPARQL queries. It makes use
  of the `RDF` type class in `rdf4h`, allowing the two packages to be
  combined easily.
* [swish](http://hackage.haskell.org/package/swish) is a toolkit for
  RDF inference and for implementing RDF file processors. It explores
  Haskell as "a scripting language for the semantic web".
