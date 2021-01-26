module Data.RDF.Vocabulary.Utils where

import Data.RDF.Namespace
import Data.RDF.Types (Node, unode)
import Data.Text

-- | Make a IRI by combining the namespace and the portion denoting the localName to that namespace
mkUnode :: Namespace -> Text -> Node
mkUnode ns ln = unode $ uriOf ns <> ln
