import RDF
import TriplesGraph
import Namespace

import Data.Char
import Data.List
import Control.Monad
import Test.QuickCheck

languages = [Nothing, Just "fr", Just "en"]
datatypes = [makeUri xsd "string", makeUri xsd "int", makeUri xsd "token"]
uris = map (makeUri ex) ["foo", "bar", "quz", "zak"]
plainliterals = [PlainL lit lang | lit <- litvalues, lang <- languages]
typedliterals = [TypedL lit dtype | lit <- litvalues, dtype <- datatypes]
litvalues = ["hello", "world", "peace", "earth", "", "haskell"]

unodes = map UNode uris
bnodes = map (\s -> BNode $ ":_anon" ++ show s) [1..5]
lnodes = [LNode lit | lit <- plainliterals ++ typedliterals]

test_triples :: [Triple]
test_triples = [triple subj pred obj | subj <- unodes ++ bnodes, 
                                       pred <- unodes,
                                       obj <- unodes ++ bnodes ++ lnodes]

instance Arbitrary Triple where
  arbitrary = oneof $ map return test_triples


prop_mkGraph :: [Triple] -> Bool
prop_mkGraph ts = triplesOf (mkGraph ts :: TriplesGraph) == ts

