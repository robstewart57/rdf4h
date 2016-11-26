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

For details see the GitHub project pages:

http://robstewart57.github.io/rdf4h/

To run the tests:

```shell
$ git submodule update --init --recursive
$ git submodule foreach git pull origin gh-pages
$ stack test
```

To run the bencharks:

```shell
$ wget https://www.govtrack.us/data/rdf/bills.099.actions.rdf.gz
$ gzip -d bills.099.actions.rdf.gz
$ stack bench
```
