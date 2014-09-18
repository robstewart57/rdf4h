module W3C.Manifest (
  loadManifest,

  Manifest(..)
) where

import Data.RDF.TriplesGraph
import Data.RDF.Query
import Data.RDF.Types
import Data.RDF.Namespace
import Text.RDF.RDF4H.TurtleParser

import qualified Data.Text as T
import qualified Data.List as L (find)
import Data.Maybe (fromJust)

data Manifest =
    Manifest {
      description :: T.Text,
      entries :: [TestEntry]
    }

data TestEntry =
    TestTurtleEval {
      name :: T.Text,
      comment :: T.Text,
      approval :: Node,
      action :: Node,
      result :: Node
    } |
    TestTurtleNegativeEval {
      name :: T.Text,
      comment :: T.Text,
      approval :: Node,
      action :: Node
    }

-- TODO: Perhaps these should be pulled from the manifest graph
rdfType = unode $ mkUri rdf "type"
rdfsComment = unode $ mkUri rdfs "comment"
mfManifest = unode "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#Manifest"
rdftTestTurtleEval = unode "http://www.w3.org/ns/rdftest#TestTurtleEval"
rdftTestTurtleNegativeEval = unode "http://www.w3.org/ns/rdftest#TestTurtleNegativeEval"
mfName = unode "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#name"
rdftApproval = unode "http://www.w3.org/ns/rdftest#approval"
mfAction = unode "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#action"
mfResult = unode "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#result"

-- | Load the manifest from the given file;
-- apply the given namespae as the base IRI of the manifest.
loadManifest :: T.Text -> T.Text -> IO Manifest
loadManifest manifestPath baseIRI = do
  parseFile testParser (T.unpack manifestPath) >>= return . ttlToManifest . fromEither
  where testParser = TurtleParser (Just (BaseUrl baseIRI)) (Just baseIRI)

ttlToManifest :: TriplesGraph -> Manifest
ttlToManifest rdf = Manifest desc tpls
  where desc = lnodeText $ objectOf $ head $ query rdf (Just $ head $ manifestSubjectNodes rdf) (Just rdfsComment) Nothing
        tpls = ttlToTestEntries rdf

ttlToTestEntries :: TriplesGraph -> [TestEntry]
ttlToTestEntries rdf = map (\n -> triplesToTestEntry $ query rdf (Just n) Nothing Nothing) $ testEntrySubjectNodes rdf

triplesToTestEntry :: Triples -> TestEntry
triplesToTestEntry ts = case objectByPredicate rdfType ts of
                          (UNode "http://www.w3.org/ns/rdftest#TestTurtleEval") -> mkTestTurtleEval ts
                          (UNode "http://www.w3.org/ns/rdftest#TestTurtleNegativeEval") -> mkTestTurtleNegativeEval ts
                          _ -> error "Unknown test case"

mkTestTurtleEval :: Triples -> TestEntry
mkTestTurtleEval ts = TestTurtleEval {
                        name = lnodeText $ objectByPredicate mfName ts,
                        comment = lnodeText $ objectByPredicate rdfsComment ts,
                        approval = objectByPredicate rdftApproval ts,
                        action = objectByPredicate mfAction ts,
                        result = objectByPredicate mfResult ts
                      }

mkTestTurtleNegativeEval :: Triples -> TestEntry
mkTestTurtleNegativeEval ts = TestTurtleNegativeEval {
                                name = lnodeText $ objectByPredicate mfName ts,
                                comment = lnodeText $ objectByPredicate rdfsComment ts,
                                approval = objectByPredicate rdftApproval ts,
                                action = objectByPredicate mfAction ts
                              }

objectByPredicate :: Predicate -> Triples -> Object
objectByPredicate p ts = objectOf $ fromJust $ L.find (\t -> predicateOf t == p) ts

manifestSubjectNodes :: TriplesGraph -> [Subject]
manifestSubjectNodes rdf = subjectNodes rdf [mfManifest]

testEntrySubjectNodes :: TriplesGraph -> [Subject]
testEntrySubjectNodes rdf = subjectNodes rdf [rdftTestTurtleEval, rdftTestTurtleNegativeEval]

subjectNodes :: TriplesGraph -> [Object] -> [Subject]
subjectNodes rdf ns = map subjectOf $ concatMap queryType ns
  where queryType = (\n -> query rdf Nothing (Just rdfType) (Just n))

lnodeText :: Node -> T.Text
lnodeText (LNode(PlainL t)) = t
lnodeText (LNode(PlainLL t _)) = t
lnodeText (LNode(TypedL t _)) = t
lnodeText _ = error "Not a literal node"
