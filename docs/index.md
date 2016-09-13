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

### Library features

rdf4h is a library for working with RDF in Haskell. It support three
RDF serialisations:

| Serialisation        | reading           | writing  |
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

-- function provided a hash based adjacency map.
foo :: RDF HashS -> [Triple]
foo rdfGraph = ...

-- function provided graph mapping (S,P) to O.
bar :: RDF SP -> Bool
bar rdfGraph = ...
{% endhighlight %}

 Those implementations
differ in their in-memory representation of RDF graphs.

* `RDF HashS` is an adjacency map with each subject mapping to a
  mapping from a predicate node to to the adjacent nodes via that
  predicate.

* `RDF SP` mapping (S,P) pairs to O, backed by `Data.Map`.

* `RDF TList` is a simple representation that represents triples as
  Haskell lists.

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

See the `Data.RDF.Query` module for more query functions.

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

<!-- ### Performance benchmarks -->

<!-- The library includes a criterion based benchmarking suite, which -->
<!-- compares the querying performance for the different type indexed graph -->
<!-- representations. See -->
<!-- [these results from September 2016](http://robstewart57.github.io/rdf4h/rdf4h-bench-12092016.html). -->

### Library RDF W3C tests and performance contributions

<!-- The library does not pass all W3C RDF parsing specification tests. See -->
<!-- the rdf4h library's current pass rate -->
<!-- [on TravisCI](https://travis-ci.org/robstewart57/rdf4h). -->

Pull requests are very welcome, especially:

1. New `Rdf` type class instances providing __new high performance RDF
   graph representations__, beyond the three instances the library
   currently has. The library includes a criterion based benchmarking
   suite, which compares the querying performance for the different
   type indexed graph representations. See
   [these results from September 2016](http://robstewart57.github.io/rdf4h/rdf4h-bench-12092016.html).

2. __Fix failing W3C parsing tests__ for the Turtle and RDF/XML
   serialisation formats. The library does not pass all W3C RDF
   parsing specification tests. See the rdf4h library's current pass
   rate [on TravisCI](https://travis-ci.org/robstewart57/rdf4h). To
   see what currently fails:
  
{% highlight shell %}
$ wget https://www.govtrack.us/data/rdf/bills.099.actions.rdf.gz
$ gzip -d bills.099.actions.rdf.gz
$ stack bench
{% endhighlight %}

Pull requests should be submitted to the rdf4h GitHub repository:
[https://github.com/robstewart57/rdf4h](https://github.com/robstewart57/rdf4h)

</div>
