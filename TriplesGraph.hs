-- |A simple graph instance represented as a list of triples.

module TriplesGraph(TriplesGraph) where

import RDF

-- |The simplest possible representation of a Graph.
newtype TriplesGraph = TriplesGraph [Triple]

instance Graph TriplesGraph where
  empty                               = TriplesGraph []
  mkGraph                             = TriplesGraph
  triplesOf     (TriplesGraph ts)     = ts
  select sl     (TriplesGraph ts)     = filter (matchTriple sl) ts
  querySubj     (TriplesGraph ts) s   = filter (match1 s) ts
  querySubjPred (TriplesGraph ts) s p = filter (match2 s p) ts

s, p, o :: Triple -> Node
s = subjectOf
p = predicateOf
o = objectOf

match1 :: Node -> Triple -> Bool
match1 subj t      = subj == s t

match2 :: Node -> Node -> Triple -> Bool
match2 subj pred t = subj == s t && pred == p t

matchTriple :: Selector -> Triple -> Bool
matchTriple sl t = sl s p o
  where (s, p, o) = (subjectOf t, predicateOf t, objectOf t)