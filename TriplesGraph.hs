{-
TriplesGraph is a naive and very simple instance of the Graph type class,
intended to serve as an initial implementation that will be supplemented
later by more efficient alternatives.
-}

module TriplesGraph(TriplesGraph) where

import RDF

-- |The simplest possible representation of a Graph.
newtype TriplesGraph = TriplesGraph [Triple]

instance Graph TriplesGraph where
  mkGraph                   = TriplesGraph
  triplesOf (TriplesGraph ts) = ts
  select sl (TriplesGraph ts) = filter (\t -> sl (s t) (p t) (o t)) ts
    where s = subjectOf
          p = predicateOf
          o = objectOf


