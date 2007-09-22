module RDF (Graph,
            Triple, triple,
            Node(UNode, BNode, LNode),
            LValue(PlainL, TypedL))
where

import Text.Printf


newtype Graph = Graph [Triple]

data Node =  UNode String               -- a uri ref node
           | BNode String               -- a blank node
           | LNode LValue               -- a literal node
  deriving (Eq)

data Triple = Triple Node Node Node
  deriving (Eq)

data LValue = PlainL String (Maybe String) -- a plain literal w/ opt lang
            | TypedL String String         -- a typed literal w/ type uri

  deriving (Eq)

triple :: Node -> Node -> Node -> Triple
-- subject must be U or B
triple (LNode _) _         _    = error "subject cannot be LNode"
-- predicate must be U
triple _         (LNode _) _    = error "predicate cannot be LNode"
triple _         (BNode _) _    = error "predicate cannot be BNode"
-- no other constraints
triple subj      pred      obj  = Triple subj pred obj


-- String representations

instance Show Triple where
  show (Triple subj pred obj) = 
    printf "%s %s %s ." (show subj) (show pred) (show obj)

instance Show Node where
  show (UNode uri) = printf "<%s>" uri
  show (BNode  id) = show id
  show (LNode (PlainL lit Nothing))     = printf "\"%s\"" lit
  show (LNode (PlainL lit (Just lang))) = printf "\"%s\"@\"%s\"" lit lang
  show (LNode (TypedL lit uri)) = printf "\"%s\"^^<%s>" lit uri
