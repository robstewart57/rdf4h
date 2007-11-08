module Text.RDF.FglGraph

where

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Tree as GT
import qualified Data.Graph.Inductive.Query as GQ

n1, n2 :: Int
n1 = 1
n2 = 2
ln1, ln2 :: (G.Node, String)
ln1 = (n1, "N1")
ln2 = (n2, "N2")
e1 :: G.Edge
e1 = (n1, n2)
le1 :: G.LEdge String
le1 = (n1, n2, "E1")
g :: GT.Gr String String
g = G.mkGraph [ln1, ln2] [le1]

