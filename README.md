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

rdf4h is a library for working with RDF in Haskell.

For details see the GitHub project page:

http://robstewart57.github.io/rdf4h/

### RDF formats

The coverage of the W3C RDF standards are:

Format | Parsing | Serialising
--- | --- | ---
NTriples | complete | complete
Turtle | partial (532/596 W3C tests) | complete
RDF/XML | partial (116/162 W3C tests) | not supported

This is based on tests run on 15.04.2018 on commit 6a26142bc727993bce5a9030b055a6f5b8ddbadd .

These tests are run on the W3C unit tests for RDF formats: https://github.com/w3c/rdf-tests.

Pull requests to increase coverage of the Turtle and RDF/XML parsers are welcome!

### Running tests

To run all the tests (parsers and the library API):

```shell
$ git submodule update --init --recursive
$ git submodule foreach git pull origin gh-pages
$ stack test --test-arguments="--quickcheck-tests 1000"
```

To run specific parser tests when bug fixing:

```shell
$ stack test --test-arguments="--pattern parser-w3c-tests-ntriples"
$ stack test --test-arguments="--pattern parser-w3c-tests-turtle"
$ stack test --test-arguments="--pattern parser-w3c-tests-xml"
```

### Running benchmarks

To run the bencharks:

```shell
$ wget https://www.govtrack.us/data/rdf/bills.099.actions.rdf.gz
$ gzip -d bills.099.actions.rdf.gz
$ stack bench
```
