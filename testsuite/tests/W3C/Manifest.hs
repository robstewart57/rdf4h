module W3C.Manifest (
  loadManifest,

  Manifest(..),
  TestEntry(..)
) where

import Data.RDF.TriplesGraph
import Data.RDF.Query
import Data.RDF.Types
import Data.RDF.Namespace
import Text.RDF.RDF4H.TurtleParser

import qualified Data.Text.Lazy as T
import qualified Data.List as L (find)
import Data.Maybe (fromJust)

-- | Manifest data as represented in W3C test files.
data Manifest =
    Manifest {
      description :: T.Text,
      entries :: [TestEntry]
    }

-- TODO: Fields `name` and `action` are mandatory for all tests,
-- `result` is mandatory for positive *Eval tests,
-- the rest are optional, so we should use "Maybe" for them.
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
    } |
    TestTurtlePositiveSyntax {
      name :: T.Text,
      comment :: T.Text,
      approval :: Node,
      action :: Node
    } |
    TestTurtleNegativeSyntax {
      name :: T.Text,
      comment :: T.Text,
      approval :: Node,
      action :: Node
    } |
    PositiveEntailmentTest {
      name :: T.Text,
      comment :: T.Text,
      approval :: Node,
      action :: Node,
      result :: Node,
      entailmentRegime :: T.Text,
      recognizedDatatypes :: [Node],
      unrecognizedDatatypes :: [Node]
    } |
    NegativeEntailmentTest {
      name :: T.Text,
      comment :: T.Text,
      approval :: Node,
      action :: Node,
      result :: Node,
      entailmentRegime :: T.Text,
      recognizedDatatypes :: [Node],
      unrecognizedDatatypes :: [Node]
    } |
    TestXMLEval {
      name :: T.Text,
      comment :: T.Text,
      approval :: Node,
      action :: Node,
      result :: Node
    } |
    TestXMLNegativeSyntax {
      name :: T.Text,
      comment :: T.Text,
      approval :: Node,
      action :: Node
    } |
    TestNTriplesPositiveSyntax {
      name :: T.Text,
      comment :: T.Text,
      approval :: Node,
      action :: Node
    } |
    TestNTriplesNegativeSyntax {
      name :: T.Text,
      comment :: T.Text,
      approval :: Node,
      action :: Node
    }
    deriving (Show)

-- TODO: Perhaps these should be pulled from the manifest graph
rdfType = unode $ mkUri rdf "type"
rdfsComment = unode $ mkUri rdfs "comment"
rdftTestTurtleEval = unode "http://www.w3.org/ns/rdftest#TestTurtleEval"
rdftTestTurtleNegativeEval = unode "http://www.w3.org/ns/rdftest#TestTurtleNegativeEval"
rdftApproval = unode "http://www.w3.org/ns/rdftest#approval"
rdfsApproval = unode $ mkUri rdfs "approval" -- FIXME: incorrect namespace "rdfs:approval" in rdf-mt/manifest.ttl, must be "rdft:approval"
mfName = unode "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#name"
mfManifest = unode "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#Manifest"
mfAction = unode "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#action"
mfResult = unode "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#result"
mfEntries = unode "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#entries"
mfEntailmentRegime = unode "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#entailmentRegime"
mfRecognizedDatatypes = unode "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#recognizedDatatypes"
mfUnrecognizedDatatypes = unode "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#unrecognizedDatatypes"

-- | Load the manifest from the given file;
-- apply the given namespace as the base IRI of the manifest.
loadManifest :: T.Text -> T.Text -> IO Manifest
loadManifest manifestPath baseIRI = do
  parseFile testParser (T.unpack manifestPath) >>= return . rdfToManifest . fromEither
  where testParser = TurtleParser (Just $ BaseUrl baseIRI) Nothing

rdfToManifest :: TriplesGraph -> Manifest
rdfToManifest rdf = Manifest desc tpls
  where desc = lnodeText $ objectOf $ head descNode
        -- FIXME: Inconsistent use of nodes for describing the manifest (W3C bug)
        descNode = query rdf (Just manifestNode) (Just rdfsComment) Nothing
                   ++ query rdf (Just manifestNode) (Just mfName) Nothing
        tpls = map (rdfToTestEntry rdf) $ rdfCollectionToList rdf collectionHead
        collectionHead = objectOf $ head $ query rdf (Just manifestNode) (Just mfEntries) Nothing
        manifestNode = head $ manifestSubjectNodes rdf

rdfToTestEntry :: TriplesGraph -> Node -> TestEntry
rdfToTestEntry rdf teSubject = triplesToTestEntry rdf $ query rdf (Just teSubject) Nothing Nothing

triplesToTestEntry :: TriplesGraph -> Triples -> TestEntry
triplesToTestEntry rdf ts =
  case objectByPredicate rdfType ts of
    (UNode "http://www.w3.org/ns/rdftest#TestTurtleEval") -> mkTestTurtleEval ts
    (UNode "http://www.w3.org/ns/rdftest#TestTurtleNegativeEval") -> mkTestTurtleNegativeEval ts
    (UNode "http://www.w3.org/ns/rdftest#TestTurtlePositiveSyntax") -> mkTestTurtlePositiveSyntax ts
    (UNode "http://www.w3.org/ns/rdftest#TestTurtleNegativeSyntax") -> mkTestTurtleNegativeSyntax ts
    (UNode "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#PositiveEntailmentTest") -> mkPositiveEntailmentTest ts rdf
    (UNode "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#NegativeEntailmentTest") -> mkNegativeEntailmentTest ts rdf
    (UNode "http://www.w3.org/ns/rdftest#TestXMLEval") -> mkTestXMLEval ts
    (UNode "http://www.w3.org/ns/rdftest#TestXMLNegativeSyntax") -> mkTestXMLNegativeSyntax ts
    (UNode "http://www.w3.org/ns/rdftest#TestNTriplesPositiveSyntax") -> mkTestNTriplesPositiveSyntax ts
    (UNode "http://www.w3.org/ns/rdftest#TestNTriplesNegativeSyntax") -> mkTestNTriplesNegativeSyntax ts
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

mkTestTurtlePositiveSyntax :: Triples -> TestEntry
mkTestTurtlePositiveSyntax ts = TestTurtlePositiveSyntax {
                                  name = lnodeText $ objectByPredicate mfName ts,
                                  comment = lnodeText $ objectByPredicate rdfsComment ts,
                                  approval = objectByPredicate rdftApproval ts,
                                  action = objectByPredicate mfAction ts
                                }

mkTestTurtleNegativeSyntax :: Triples -> TestEntry
mkTestTurtleNegativeSyntax ts = TestTurtleNegativeSyntax {
                                  name = lnodeText $ objectByPredicate mfName ts,
                                  comment = lnodeText $ objectByPredicate rdfsComment ts,
                                  approval = objectByPredicate rdftApproval ts,
                                  action = objectByPredicate mfAction ts
                                }

mkPositiveEntailmentTest :: Triples -> TriplesGraph -> TestEntry
mkPositiveEntailmentTest ts rdf = PositiveEntailmentTest {
                                    name = lnodeText $ objectByPredicate mfName ts,
                                    comment = lnodeText $ objectByPredicate rdfsComment ts,
                                    -- FIXME: incorrect namespace "rdfs:approval" in rdf-mt/manifest.ttl
                                    -- approval = objectByPredicate rdftApproval ts,
                                    approval = objectByPredicate rdfsApproval ts,
                                    action = objectByPredicate mfAction ts,
                                    result = objectByPredicate mfResult ts,
                                    entailmentRegime = lnodeText $ objectByPredicate mfEntailmentRegime ts,
                                    recognizedDatatypes = rDT,
                                    unrecognizedDatatypes = uDT
                                  }
    where rDT = rdfCollectionToList rdf rDTCollectionHead
          rDTCollectionHead = objectByPredicate mfRecognizedDatatypes ts
          uDT = rdfCollectionToList rdf uDTCollectionHead
          uDTCollectionHead = objectByPredicate mfUnrecognizedDatatypes ts

mkNegativeEntailmentTest :: Triples -> TriplesGraph -> TestEntry
mkNegativeEntailmentTest ts rdf = NegativeEntailmentTest {
                                    name = lnodeText $ objectByPredicate mfName ts,
                                    comment = lnodeText $ objectByPredicate rdfsComment ts,
                                    -- FIXME: incorrect namespace "rdfs:approval" in rdf-mt/manifest.ttl
                                    -- approval = objectByPredicate rdftApproval ts,
                                    approval = objectByPredicate rdfsApproval ts,
                                    action = objectByPredicate mfAction ts,
                                    result = objectByPredicate mfResult ts,
                                    entailmentRegime = lnodeText $ objectByPredicate mfEntailmentRegime ts,
                                    recognizedDatatypes = rDT,
                                    unrecognizedDatatypes = uDT
                                  }
    where rDT = rdfCollectionToList rdf rDTCollectionHead
          rDTCollectionHead = objectByPredicate mfRecognizedDatatypes ts
          uDT = rdfCollectionToList rdf uDTCollectionHead
          uDTCollectionHead = objectByPredicate mfUnrecognizedDatatypes ts

mkTestXMLEval :: Triples -> TestEntry
mkTestXMLEval ts = TestXMLEval {
                     name = lnodeText $ objectByPredicate mfName ts,
                     comment = lnodeText $ objectByPredicate rdfsComment ts,
                     -- FIXME: incorrect namespace "rdfs:approval" in rdf-mt/manifest.ttl
                     -- approval = objectByPredicate rdftApproval ts,
                     approval = objectByPredicate rdfsApproval ts,
                     action = objectByPredicate mfAction ts,
                     result = objectByPredicate mfResult ts
                   }

mkTestXMLNegativeSyntax :: Triples -> TestEntry
mkTestXMLNegativeSyntax ts = TestXMLNegativeSyntax {
                               name = lnodeText $ objectByPredicate mfName ts,
                               comment = lnodeText $ objectByPredicate rdfsComment ts,
                               -- FIXME: incorrect namespace "rdfs:approval" in rdf-mt/manifest.ttl
                               -- approval = objectByPredicate rdftApproval ts
                               approval = objectByPredicate rdfsApproval ts,
                               action = objectByPredicate mfAction ts
                             }

mkTestNTriplesPositiveSyntax :: Triples -> TestEntry
mkTestNTriplesPositiveSyntax ts = TestNTriplesPositiveSyntax {
                                    name = lnodeText $ objectByPredicate mfName ts,
                                    comment = lnodeText $ objectByPredicate rdfsComment ts,
                                    approval = objectByPredicate rdftApproval ts,
                                    action = objectByPredicate mfAction ts
                                  }

mkTestNTriplesNegativeSyntax :: Triples -> TestEntry
mkTestNTriplesNegativeSyntax ts = TestNTriplesPositiveSyntax {
                                    name = lnodeText $ objectByPredicate mfName ts,
                                    comment = lnodeText $ objectByPredicate rdfsComment ts,
                                    approval = objectByPredicate rdftApproval ts,
                                    action = objectByPredicate mfAction ts
                                  }

-- Filter the triples by given predicate and return the object of the first found triple.
-- Raises an exception on errors.
objectByPredicate :: Predicate -> Triples -> Object
objectByPredicate p = objectOf . fromJust . L.find (\t -> predicateOf t == p)

manifestSubjectNodes :: TriplesGraph -> [Subject]
manifestSubjectNodes rdf = subjectNodes rdf [mfManifest]

subjectNodes :: TriplesGraph -> [Object] -> [Subject]
subjectNodes rdf = (map subjectOf) . concatMap queryType
  where queryType n = query rdf Nothing (Just rdfType) (Just n)

-- | Text of the literal node.
-- Note that it doesn't perform type conversion for TypedL.
-- TODO: Looks useful. Move it to RDF4H lib?
lnodeText :: Node -> T.Text
lnodeText (LNode(PlainL t)) = t
lnodeText (LNode(PlainLL t _)) = t
lnodeText (LNode(TypedL t _)) = t
lnodeText _ = error "Not a literal node"

-- | Convert an RDF collection to a List of its objects.
-- | Given a list of RDF triples as shown:
-- |   <x> <collection> <c1>
-- |   <c1> <rdf:first> <i1>
-- |   <c1> <rdf:rest> <c2>
-- |   <c2> <rdf:first> <i2>
-- |   <c2> <rdf:rest> <rdf:nil>
-- | ... it extracts a list of nodes [i1, i2].
-- | First argument (`rdf`) is the RDF graph;
-- | second argument (`tip`) is the "collection head" (<c1> in the example above),
-- | (all triples with <rdf:first> and <rdf:rest> pairs).
-- TODO: Looks useful. Move it to RDF4H lib?
rdfCollectionToList :: TriplesGraph -> Node -> [Node]
rdfCollectionToList _ (UNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")) = []
rdfCollectionToList rdf tip = concatMap (tripleToList rdf) $ nextCollectionTriples rdf tip

tripleToList :: TriplesGraph -> Triple -> [Node]
tripleToList _ (Triple _ (UNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#first")) n@(UNode _)) = [n]
tripleToList rdf (Triple _ (UNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest")) tip) = rdfCollectionToList rdf tip
tripleToList _ _ = error "Invalid collection format"

nextCollectionTriples :: TriplesGraph -> Node -> Triples
nextCollectionTriples rdf tip@(BNodeGen _) = query rdf (Just tip) Nothing Nothing
nextCollectionTriples _ _ = error "Invalid collection format"
