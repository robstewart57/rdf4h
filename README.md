RDF For Haskell
=====

rdf4h is a library for working with RDF in Haskell. At present it
  includes parsers and serializers for RDF in the N-Triples and
  Turtle formats, and parsing support for RDF/XML. It provides abilities such
  as querying for triples containing a particular subject, predicate,
  or object, or selecting triples that satisfy an arbitrary predicate
  function.

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

    import Data.RDF
    import Data.RDF.TriplesGraph
    import Text.RDF.RDF4H.NTriplesParser

    rdfGraph1 :: IO TriplesGraph
    rdfGraph1 = fmap fromEither (parseFile NTriplesParser "test1.nt")

    rdfGraph2 :: IO TriplesGraph
    rdfGraph2 = fmap fromEither (parseFile NTriplesParser "test2.nt")

    example :: IO ()
    example = do
      g1 <- rdfGraph1
      g2 <- rdfGraph2
      let node1 = lnode $ PlainL (s2b "foo")
      putStrLn $ "Subjects of g1: " ++ show (map subjectOf (triplesOf g1))
      putStrLn $ "RDF contains literal 'foo': " ++ show (rdfContainsNode g1 node1)
      putStrLn $ "Isomorphism test: " ++ show (isIsomorphic g1 g2)
      putStrLn $ "Unsorted triples: " ++ show (triplesOf g2)
      putStrLn $ "Sorted triples: "   ++ show ((sortTriples . triplesOf) g2)
      putStrLn $ "Query: " ++ show (query g1 Nothing Nothing (Just node1))


TODOs
-----

* RDF/XML Serialization; Add RdfParser instance for XmlParser
* `Data.RDF` API to match that of the
[Model](http://jena.apache.org/documentation/javadoc/jena/com/hp/hpl/jena/rdf/model/Model.html)
class in the Jena framework (Java)
* Switch to `OverloadedStrings` language extension for conversions
between Strings and ByteStrings.
* Complete QuickCheck tests for `Text.RDF.RDF4H.TurtleParser_ConformanceTest`


Issues
------

Please use the GitHub [issue
tracker](https://github.com/robstewart57/rdf4h/issues) to report any
bugs you might find. New contributors are most welcome, and pull
requests will be favourably received.


Related Haskell Packages
----------------

* [hsparql](http://hackage.haskell.org/package/hsparql/) is a DSL for
  programmatic creation and execution of SPARQL queries. It makes use
  of the `RDF` type class in `rdf4h`, allowing the two packages to be
  combined easily.
* [swish](http://hackage.haskell.org/package/swish) is a toolkit for
  RDF inference and for implementing RDF file processors. It explores
  Haskell as "a scripting language for the semantic web".
