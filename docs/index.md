---
layout: default
---
<header class="masthead">        
  <h1 class="masthead-title">
    <a href="{{ site.baseurl }}/">rdf4h<span> RDF for Haskell</span></a>
  </h1>
  <!-- <nav class="masthead-nav"> -->
  <!--   {% for nav in site.nav %} -->
  <!--   <a href="{{ nav.href }}">{{ nav.name }}</a> -->
  <!--   {% endfor %} -->
  <!-- </nav> -->
</header>

<a href="https://github.com/robstewart57/rdf4h"><img style="position:
absolute; top: 0; right: 0; border: 0;"
src="https://camo.githubusercontent.com/a6677b08c955af8400f44c6298f40e7d19cc5b2d/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f677261795f3664366436642e706e67"
alt="Fork me on GitHub"
data-canonical-src="https://s3.amazonaws.com/github/ribbons/forkme_right_gray_6d6d6d.png"></a>

### Overview

The _rdf4h_ library lets Haskell programmers query structured data
described with the the __Resource Description Framework__ model. _RDF_
models data as a collection of _<subject,predicate,object>_ triples,
e.g.

![The RDF model]({{ site.baseurl }}/RDF_example.png)

### Supported RDF serialisation formats

The rdf4h library supports three RDF serialisations:

| Serialisation        | Reading           | Writing  |
| ------------- |:-------------:|:-----:|
| NTriples     | &#10003; | &#10003; |
| Turtle      | &#10003;      | &#10003; |
| RDF/XML | &#10003;    | &#10007; |

### Type level RDF graph representations

The `RDF` type is a data family, for which there are a number of
instances. Those instances represent type level indexes that provide
the programmer with the choice of underlying in-memory graph
representation. 

{% highlight haskell %}
data family RDF a

-- function provided a hash based adjacency map RDF graph.
foo :: RDF AdjHashMap -> [Triple]
foo rdfGraph = ...

-- function provided a (s,p,o) triples based RDF graph.
bar :: RDF TList -> Bool
bar rdfGraph = ...
{% endhighlight %}

 Those implementations
differ in their in-memory representation of RDF graphs.

* `RDF AdjHashMap` is an adjacency map with each subject mapping to a
  mapping from a predicate node to to the adjacent nodes via that
  predicate.

* `RDF TList` is a simple representation that represents (s,p,o)
  triples as Haskell lists.

These data family instance represent application specific tradeoffs in
terms of space and runtime performance. See
[these criterion results](http://robstewart57.github.io/rdf4h/rdf4h-bench-12092016.html)
for `query` and `select` performance for all graph implementations,
taken in September 2016.


### Rdf type class

{% highlight haskell %}
class Rdf a where
  baseUrl           :: RDF a -> Maybe BaseUrl
  prefixMappings    :: RDF a -> PrefixMappings
  addPrefixMappings :: RDF a -> PrefixMappings -> Bool -> RDF a
  empty             :: RDF a
  mkRdf             :: Triples -> Maybe BaseUrl -> PrefixMappings -> RDF a
  triplesOf         :: RDF a -> Triples
  uniqTriplesOf     :: RDF a -> Triples
  select            :: RDF a -> NodeSelector -> NodeSelector -> NodeSelector -> Triples
  query             :: RDF a -> Maybe Node -> Maybe Node -> Maybe Node -> Triples
  showGraph         :: RDF a -> String
{% endhighlight %}

See the `Data.RDF.Query` module for more query functions,
[here](http://hackage.haskell.org/package/rdf4h-3.0.0/docs/Data-RDF-Query.html).

### RDF parsing and writing

To parse and write RDF:

{% highlight haskell %}
class RdfParser p where
  parseString :: (Rdf a) => p -> T.Text -> Either ParseFailure (RDF a)
  parseFile   :: (Rdf a) => p -> String -> IO (Either ParseFailure (RDF a))
  parseURL    :: (Rdf a) => p -> String -> IO (Either ParseFailure (RDF a))

class RdfSerializer s where
  hWriteRdf     :: (Rdf a) => s -> Handle -> RDF a -> IO ()
{% endhighlight %}

To write an RDF graph to a file:

{% highlight haskell %}
withFile "out.nt" WriteMode (\h -> hWriteRdf NTriplesSerializer h rdfGraph)
{% endhighlight %}


### RDF query example

{% highlight haskell %}
module Main where

import Data.RDF
import qualified Data.Text as T

eswcCommitteeURI, heldByProp :: T.Text
eswcCommitteeURI =
  "http://data.semanticweb.org/conference/eswc/2015/program-committee-member"
heldByProp       = "swc:heldBy"

{- the RDF triples model for ESWC PC members is:
S: <http://data.semanticweb.org/conference/eswc/2015/program-committee-member>
P: <swc:heldBy>
O: <memberURI>

S: <memberURI>
P: <foaf:firstName>
O: "persons first name"

S: <memberURI>
P: <foaf:lastName>
O: "persons last name"
-}

-- returns a list of full names of people who served as
-- members on the ESWC 2015 conference programme committee.
eswcCommitteeMembers :: RDF TList -> [T.Text]
eswcCommitteeMembers graph =
  let triples = query
                  graph
                  (Just (unode eswcCommitteeURI))
                  (Just (unode heldByProp))
                  Nothing
      memberURIs = map objectOf triples
  in map
     (\memberURI ->
              let (LNode (PlainL firstName)) =
                    objectOf $ head $
                      query
                        graph
                        (Just memberURI)
                        (Just (unode "foaf:firstName"))
                        Nothing
                  (LNode (PlainL lastName))  =
                    objectOf $ head $
                      query
                      graph
                      (Just memberURI)
                      (Just (unode "foaf:lastName"))
                      Nothing
              in (T.append firstName (T.append (T.pack  " ") lastName)))
     memberURIs
        
main :: IO ()
main = do
  result <- parseURL
    (XmlParser Nothing Nothing)
    "http://data.semanticweb.org/dumps/conferences/eswc-2015-complete.rdf"
  case result of
    Left err -> error "Unable to parse RDF content from that URL"
    Right rdfGraph -> do
      let eswcMemberNames = eswcCommitteeMembers rdfGraph
      mapM_ (putStrLn . T.unpack) eswcMemberNames
{% endhighlight %}

The list of the _Extended Semantic Web Conference 2015_ programme
committee members is printed to standard out:

{% highlight shell %}
Vadim Ermolayev
Karl Aberer
Giorgos Stoilos
Birgitta König-Ries
Giuseppe Rizzo
...
{% endhighlight %}

### Please contribute!

Pull requests should be submitted to the rdf4h GitHub repository:
[https://github.com/robstewart57/rdf4h](https://github.com/robstewart57/rdf4h)

<br>

#### High performance RDF graph implementations

__Wanted:__ new `Rdf` type class instances providing new high
performance RDF graph representations, beyond the three instances the
library currently has.

<br>

The library includes a criterion based benchmarking suite, which
compares the querying performance for the different type indexed graph
representations.

See
[criterion results from October 2016](http://robstewart57.github.io/rdf4h/rdf-bench-04102016.html).

To run the criterion benchmarks locally:

{% highlight shell %}
$ wget https://www.govtrack.us/data/rdf/bills.099.actions.rdf.gz
$ gzip -d bills.099.actions.rdf.gz
$ stack bench
{% endhighlight %}

Or to run just the benchmarks for a specific RDF implementation, use
the `--benchmark-arguments` flag, e.g.:

{% highlight shell %}
$ stack bench --benchmark-arguments 'query/HashS'
{% endhighlight %}

<br>

#### Fixes for failing W3C parser unit tests 

__Wanted:__ fix failing W3C parsing tests for the Turtle and RDF/XML
serialisation formats.
<br><br>

The library does not pass all W3C RDF parsing specification tests. See
the rdf4h library's current pass rate
[on TravisCI](https://travis-ci.org/robstewart57/rdf4h).

To see what currently fails:

{% highlight shell %}
$ git submodule update --init --recursive
$ git submodule foreach git pull origin gh-pages
$ stack test
{% endhighlight %}

To list the available tests that can be run in isolation using a
pattern:

{% highlight shell %}
$ stack test --test-arguments "--list-tests"
{% endhighlight %}

To run specific test groups:

{% highlight shell %}
$ stack test --test-arguments="--pattern HashMapSP"
{% endhighlight %}


