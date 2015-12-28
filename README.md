RDF For Haskell
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

rdf4h is a library for working with RDF in Haskell. At present it
includes parsers and serializers for RDF in the N-Triples and Turtle
formats, and parsing support for RDF/XML. It provides abilities such
as querying for triples containing a particular subject, predicate, or
object, or selecting triples that satisfy an arbitrary predicate
function.


RDF instance benchmarks
-----------------------

The following benchmark results were obtained running
bench/MainCriterion.hs at commit
3d86295b085ecfb9a944dc7b40bb668df38c2777 on 28 December 2015. The
complete output from criterion are in
`benchmark-results/criterion-results-28.12.2015.txt`.

Mapping the `[Triple]` stream from the Turtle parser into each RDF
instance:

|                     | parse |
|---------------------|-------|
| HashMapS            | 19.7s |
| MapSP               | 19.6s |
| TriplesList         | 18.6s |
| TriplesPatriciaTree | 22.0s |

Benchmarking `query` results in microseconds:

|                     | S | P | O | SP | PO | SO | SPO |
|---------------------|-------|
| HashMapS            | 29 | 2254 | 25640 | 0.5 | 0.3 | 4 | 0.5 | 
| MapSP               | 9379 | 8518 | 1458 | 8 | 8639 | 9221 | 9379 |
| TriplesList         | 11100 | 7600 | 9600 | 11100 | 7600 | 11100 | 1100 |
| TriplesPatriciaTree | 206600 | 178700 | 188300 | 206800 |187400 | 174200 | 173600 |

Benchmarking `select` results in microseconds:

|                     | S | P | O | SP | PO | SO | SPO |
|---------------------|-------|
| HashMapS            | 275100 | 326400 | 112900 | 263700 | 111100 | 96680 | 95630 |
| MapSP               | 12820 | 14610 | 13230 | 14950 | 14270 | 13420 | 14160 |
| TriplesList         | 12620 | 10730 | 9961 | 12760 | 11080 | 11790 | 13080 | 
| TriplesPatriciaTree | 243300 | 238600 | 204100 | 251000 | 210300 | 209400 | 211400 |


Installation
------------

Once the Haskell platform has been installed, simply:

    $ cabal update
    $ cabal install rdf4h


Usage
---

The `rdf4h` library is split in to two parts.

* `Data.RDF` defines the RDF, RdfSerializer and RdfParser
  type classes. It also provides an API for RDF graph inspection.
* `Text.RDF.RDF4H.*` provides the parsers and serializers for
  supported RDF formats.

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


Development
-----------

Contributions are welcome. A rewrite of the RDF type class API will
happen in the future. Additions to `Data.RDF.Types` and
`Data.RDF.Query`, fixes to the three parsers, and an RDF/XML
serialiser would be great. Writing tests is highly encouraged.


Issues
------

Please use the GitHub [issue
tracker](https://github.com/robstewart57/rdf4h/issues) to report any
bugs you might find. New contributors are most welcome! See the
TODO.org file for some ideas on how to contribute.


Related Haskell Packages
----------------

* [hsparql](http://hackage.haskell.org/package/hsparql/) is a DSL for
  programmatic creation and execution of SPARQL queries. It makes use
  of the `RDF` type class in `rdf4h`, allowing the two packages to be
  combined easily.
* [swish](http://hackage.haskell.org/package/swish) is a toolkit for
  RDF inference and for implementing RDF file processors. It explores
  Haskell as "a scripting language for the semantic web".
