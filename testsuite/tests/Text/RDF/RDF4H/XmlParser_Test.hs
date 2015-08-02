module Text.RDF.RDF4H.XmlParser_Test where

-- todo: QuickCheck tests

-- Testing imports
import Test.Framework (Test,testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion,assertBool,assertFailure)

-- Import common libraries to facilitate tests
import qualified Data.Map as Map
import Data.RDF.Query
import Data.RDF.TriplesGraph (TriplesGraph)
import Data.RDF.Types
import qualified Data.Text.Lazy as T (Text, unlines)
import Text.RDF.RDF4H.XmlParser
 
tests :: [Test]
tests = [ testGroup "XmlParser:parseXmlRDF" [ testCase "simpleStriping1" test_simpleStriping1
                                            , testCase "simpleStriping2" test_simpleStriping2
                                            , testCase "simpleSingleton1" test_simpleSingleton1
                                            , testCase "simpleSingleton2" test_simpleSingleton2
                                            , testCase "example07" test_parseXmlRDF_example07
                                            , testCase "example08" test_parseXmlRDF_example08
                                            , testCase "example09" test_parseXmlRDF_example09
                                            , testCase "example10" test_parseXmlRDF_example10
                                            , testCase "example11" test_parseXmlRDF_example11
                                            , testCase "example12" test_parseXmlRDF_example12
                                            , testCase "example13" test_parseXmlRDF_example13
                                            , testCase "example14" test_parseXmlRDF_example14
                                            , testCase "example15" test_parseXmlRDF_example15
                                            , testCase "example16" test_parseXmlRDF_example16
                                            , testCase "example17" test_parseXmlRDF_example17
                                            , testCase "example18" test_parseXmlRDF_example18
                                            , testCase "example19" test_parseXmlRDF_example19
                                            , testCase "example20" test_parseXmlRDF_example20
                                            , testCase "vCardPersonal" test_parseXmlRDF_vCardPersonal
                                            , testCase "NML" test_parseXmlRDF_NML
                                            , testCase "NML2" test_parseXmlRDF_NML2
                                            , testCase "NML3" test_parseXmlRDF_NML3
                                            ]
        ]


mkTextNode :: T.Text -> Node
mkTextNode = lnode . plainL

testParse :: T.Text -> TriplesGraph -> Assertion
testParse exRDF ex =
    case parsed of
      Right result ->
          assertBool
            ("expected: " ++ show ex ++ "but got: " ++ show result)
            (isIsomorphic (result :: TriplesGraph) (ex :: TriplesGraph))
      Left (ParseFailure err) ->
          assertFailure err
  where parsed = parseString (XmlParser Nothing Nothing) exRDF

test_simpleStriping1 :: Assertion
test_simpleStriping1 = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
            \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\
      \<rdf:Description rdf:about=\"http://www.w3.org/TR/rdf-syntax-grammar\">\
        \<dc:title>RDF/XML Syntax Specification (Revised)</dc:title>\
      \</rdf:Description>\
    \</rdf:RDF>"
    ( mkRdf [ Triple (unode "http://www.w3.org/TR/rdf-syntax-grammar")
                     (unode "dc:title")
                     (mkTextNode "RDF/XML Syntax Specification (Revised)") ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("dc", "http://purl.org/dc/elements/1.1/")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

test_simpleStriping2 :: Assertion
test_simpleStriping2 = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
            \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\
      \<rdf:Description rdf:about=\"http://www.w3.org/TR/rdf-syntax-grammar\">\
        \<dc:title>RDF/XML Syntax Specification (Revised)</dc:title>\
      \</rdf:Description>\
      \<rdf:Description rdf:about=\"http://example.org/buecher/baum\">\
        \<dc:title>Der Baum</dc:title>\
      \</rdf:Description>\
    \</rdf:RDF>"
    ( mkRdf [ Triple (unode "http://www.w3.org/TR/rdf-syntax-grammar")
                     (unode "dc:title")
                     (mkTextNode "RDF/XML Syntax Specification (Revised)") 
            , Triple (unode "http://example.org/buecher/baum")
                     (unode "dc:title")
                     (mkTextNode "Der Baum")
            ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("dc", "http://purl.org/dc/elements/1.1/")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

test_simpleSingleton1 :: Assertion
test_simpleSingleton1 = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
            \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\
      \<rdf:Description rdf:about=\"http://www.w3.org/TR/rdf-syntax-grammar\"\
                      \ dc:title=\"RDF/XML Syntax Specification (Revised)\"/>\
    \</rdf:RDF>"
    ( mkRdf [ Triple (unode "http://www.w3.org/TR/rdf-syntax-grammar")
                     (unode "dc:title")
                     (mkTextNode "RDF/XML Syntax Specification (Revised)") ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("dc", "http://purl.org/dc/elements/1.1/")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

test_simpleSingleton2 :: Assertion
test_simpleSingleton2 = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
            \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\
      \<rdf:Description rdf:about=\"http://www.w3.org/TR/rdf-syntax-grammar\"\
                      \ dc:title=\"RDF/XML Syntax Specification (Revised)\"\
                      \ dc:subject=\"RDF\"/>\
    \</rdf:RDF>"
    ( mkRdf [ Triple (unode "http://www.w3.org/TR/rdf-syntax-grammar")
                     (unode "dc:title")
                     (mkTextNode "RDF/XML Syntax Specification (Revised)")
            , Triple (unode "http://www.w3.org/TR/rdf-syntax-grammar")
                     (unode "dc:subject")
                     (mkTextNode "RDF") ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("dc", "http://purl.org/dc/elements/1.1/")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

-- * Document Element and XML Declaration
test_parseXmlRDF_example07 :: Assertion
test_parseXmlRDF_example07 = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
            \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\
            \ xmlns:ex=\"http://example.org/stuff/1.0/\">\
      \<rdf:Description rdf:about=\"http://www.w3.org/TR/rdf-syntax-grammar\"\
                      \ dc:title=\"RDF/XML Syntax Specification (Revised)\">\
        \<ex:editor>\
          \<rdf:Description ex:fullName=\"Dave Beckett\">\
            \<ex:homePage rdf:resource=\"http://purl.org/net/dajobe/\" />\
          \</rdf:Description>\
        \</ex:editor>\
      \</rdf:Description>\
    \</rdf:RDF>"
    ( mkRdf [ Triple (unode "http://www.w3.org/TR/rdf-syntax-grammar")
                     (unode "dc:title")
                     (mkTextNode "RDF/XML Syntax Specification (Revised)")
            , Triple (BNodeGen 1) (unode "ex:fullName") (mkTextNode "Dave Beckett")
            , Triple (BNodeGen 1) (unode "ex:homePage") (unode "http://purl.org/net/dajobe/")
            , Triple (unode "http://www.w3.org/TR/rdf-syntax-grammar") (unode "ex:editor") (BNodeGen 1)
            ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("dc", "http://purl.org/dc/elements/1.1/")
                                           , ("ex", "http://example.org/stuff/1.0/")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

-- * Languages: xml:lang
test_parseXmlRDF_example08 :: Assertion
test_parseXmlRDF_example08 = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
            \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\
      \<rdf:Description rdf:about=\"http://www.w3.org/TR/rdf-syntax-grammar\">\
        \<dc:title>RDF/XML Syntax Specification (Revised)</dc:title>\
        \<dc:title xml:lang=\"en\">RDF/XML Syntax Specification (Revised)</dc:title>\
        \<dc:title xml:lang=\"en-US\">RDF/XML Syntax Specification (Revised)</dc:title>\
      \</rdf:Description>\
      \<rdf:Description rdf:about=\"http://example.org/buecher/baum\" xml:lang=\"de\">\
        \<dc:title>Der Baum</dc:title>\
        \<dc:description>Das Buch ist außergewöhnlich</dc:description>\
        \<dc:title xml:lang=\"en\">The Tree</dc:title>\
      \</rdf:Description>\
    \</rdf:RDF>"
    ( mkRdf [ Triple (unode "http://www.w3.org/TR/rdf-syntax-grammar")
                     (unode "dc:title")
                     (mkTextNode "RDF/XML Syntax Specification (Revised)")
            , Triple (unode "http://www.w3.org/TR/rdf-syntax-grammar")
                     (unode "dc:title")
                     (lnode (plainLL ("RDF/XML Syntax Specification (Revised)") ("en")))
            , Triple (unode "http://www.w3.org/TR/rdf-syntax-grammar")
                     (unode "dc:title")
                     (lnode (plainLL ("RDF/XML Syntax Specification (Revised)") ("en-US")))
            , Triple (unode "http://example.org/buecher/baum")
                     (unode "dc:title")
                     (lnode (plainLL ("Der Baum") ("de")))
            , Triple (unode "http://example.org/buecher/baum")
                     (unode "dc:description")
                     (lnode (plainLL ("Das Buch ist außergewöhnlich") ("de")))
            , Triple (unode "http://example.org/buecher/baum")
                     (unode "dc:title")
                     (lnode (plainLL ("The Tree") ("en")))
            ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("dc", "http://purl.org/dc/elements/1.1/")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

-- * XML Literals: rdf:parseType="Literal"
test_parseXmlRDF_example09 :: Assertion
test_parseXmlRDF_example09 = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
            \ xmlns:ex=\"http://example.org/stuff/1.0/\">\
      \<rdf:Description rdf:about=\"http://example.org/item01\">\
        \<ex:prop rdf:parseType=\"Literal\"\
                \ xmlns:a=\"http://example.org/a#\"><a:Box required=\"true\">\
          \<a:widget size=\"10\" />\
          \<a:grommit id=\"23\" /></a:Box>\
        \</ex:prop>\
      \</rdf:Description>\
    \</rdf:RDF>"
    ( mkRdf [ Triple (unode "http://example.org/item01")
                     (unode "ex:prop")
                     (lnode (typedL ("<a:Box required=\"true\">\
                                           \<a:widget size=\"10\"/>\
                                           \<a:grommit id=\"23\"/>\
                                         \</a:Box>")
                            ("http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral")))
            ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("ex", "http://example.org/stuff/1.0/")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

-- * Typed Literals: rdf:datatype
test_parseXmlRDF_example10 :: Assertion
test_parseXmlRDF_example10 = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
            \ xmlns:ex=\"http://example.org/stuff/1.0/\">\
      \<rdf:Description rdf:about=\"http://example.org/item01\">\
        \<ex:size rdf:datatype=\"http://www.w3.org/2001/XMLSchema#int\">123</ex:size>\
      \</rdf:Description>\
    \</rdf:RDF>"
    ( mkRdf [ Triple (unode "http://example.org/item01")
                     (unode "ex:size")
                     (lnode (typedL ("123")
                            ("http://www.w3.org/2001/XMLSchema#int")))
            ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("ex", "http://example.org/stuff/1.0/")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

-- * Identifying Blank Nodes: rdf:nodeID
test_parseXmlRDF_example11 :: Assertion
test_parseXmlRDF_example11 = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
            \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\
            \ xmlns:ex=\"http://example.org/stuff/1.0/\">\
      \<rdf:Description rdf:about=\"http://www.w3.org/TR/rdf-syntax-grammar\"\
                      \ dc:title=\"RDF/XML Syntax Specification (Revised)\">\
        \<ex:editor rdf:nodeID=\"abc\"/>\
      \</rdf:Description>\
      \<rdf:Description rdf:nodeID=\"abc\"\
                      \ ex:fullName=\"Dave Beckett\">\
        \<ex:homePage rdf:resource=\"http://purl.org/net/dajobe/\"/>\
      \</rdf:Description>\
    \</rdf:RDF>"
    ( mkRdf [ Triple (unode "http://www.w3.org/TR/rdf-syntax-grammar")
                     (unode "dc:title")
                     (mkTextNode "RDF/XML Syntax Specification (Revised)")
            , Triple (mkBNode "abc") (unode "ex:fullName") (mkTextNode "Dave Beckett")
            , Triple (mkBNode "abc") (unode "ex:homePage") (unode "http://purl.org/net/dajobe/")
            , Triple (unode "http://www.w3.org/TR/rdf-syntax-grammar")
                     (unode "ex:editor")
                     (mkBNode "abc")
            ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("dc", "http://purl.org/dc/elements/1.1/")
                                           , ("ex", "http://example.org/stuff/1.0/")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )
  where mkBNode = BNode

-- * Omitting Blank Nodes: rdf:parseType="Resource"
test_parseXmlRDF_example12 :: Assertion
test_parseXmlRDF_example12 = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
            \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\
            \ xmlns:ex=\"http://example.org/stuff/1.0/\">\
      \<rdf:Description rdf:about=\"http://www.w3.org/TR/rdf-syntax-grammar\"\
                      \ dc:title=\"RDF/XML Syntax Specification (Revised)\">\
        \<ex:editor rdf:parseType=\"Resource\">\
          \<ex:fullName>Dave Beckett</ex:fullName>\
          \<ex:homePage rdf:resource=\"http://purl.org/net/dajobe/\"/>\
        \</ex:editor>\
      \</rdf:Description>\
    \</rdf:RDF>"
    ( mkRdf [ Triple (unode "http://www.w3.org/TR/rdf-syntax-grammar")
                     (unode "dc:title")
                     (mkTextNode "RDF/XML Syntax Specification (Revised)")
            , Triple (BNodeGen 1) (unode "ex:fullName") (mkTextNode "Dave Beckett")
            , Triple (BNodeGen 1) (unode "ex:homePage") (unode "http://purl.org/net/dajobe/")
            , Triple (unode "http://www.w3.org/TR/rdf-syntax-grammar")
                     (unode "ex:editor")
                     (BNodeGen 1)
            ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("dc", "http://purl.org/dc/elements/1.1/")
                                           , ("ex", "http://example.org/stuff/1.0/")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

-- * Omitting Nodes: Property Attributes on an empty Property Element
test_parseXmlRDF_example13 :: Assertion
test_parseXmlRDF_example13 = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
            \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\
            \ xmlns:ex=\"http://example.org/stuff/1.0/\">\
      \<rdf:Description rdf:about=\"http://www.w3.org/TR/rdf-syntax-grammar\"\
                      \ dc:title=\"RDF/XML Syntax Specification (Revised)\">\
        \<ex:editor ex:fullName=\"Dave Beckett\" />\
      \</rdf:Description>\
    \</rdf:RDF>"
    ( mkRdf [ Triple (unode "http://www.w3.org/TR/rdf-syntax-grammar")
                     (unode "dc:title")
                     (mkTextNode "RDF/XML Syntax Specification (Revised)")
            , Triple (BNodeGen 1)
                     (unode "ex:fullName")
                     (mkTextNode "Dave Beckett")
            , Triple (unode "http://www.w3.org/TR/rdf-syntax-grammar")
                     (unode "ex:editor")
                     (BNodeGen 1)
            ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("dc", "http://purl.org/dc/elements/1.1/")
                                           , ("ex", "http://example.org/stuff/1.0/")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

-- * Typed Node Elements
test_parseXmlRDF_example14 :: Assertion
test_parseXmlRDF_example14 = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
            \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\
            \ xmlns:ex=\"http://example.org/stuff/1.0/\">\
      \<rdf:Description rdf:about=\"http://example.org/thing\">\
        \<rdf:type rdf:resource=\"http://example.org/stuff/1.0/Document\"/>\
        \<dc:title>A marvelous thing</dc:title>\
      \</rdf:Description>\
    \</rdf:RDF>"
    ( mkRdf [ Triple (unode "http://example.org/thing")
                     (unode "rdf:type")
                     (unode "http://example.org/stuff/1.0/Document")
            , Triple (unode "http://example.org/thing")
                     (unode "dc:title")
                     (mkTextNode "A marvelous thing")
            ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("dc", "http://purl.org/dc/elements/1.1/")
                                           , ("ex", "http://example.org/stuff/1.0/")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

test_parseXmlRDF_example15 :: Assertion
test_parseXmlRDF_example15 = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
            \ xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\
            \ xmlns:ex=\"http://example.org/stuff/1.0/\">\
      \<ex:Document rdf:about=\"http://example.org/thing\">\
        \<dc:title>A marvelous thing</dc:title>\
      \</ex:Document>\
    \</rdf:RDF>"
    ( mkRdf [ Triple (unode "http://example.org/thing")
                     (unode "rdf:type")
                     (unode "ex:Document")
            , Triple (unode "http://example.org/thing")
                     (unode "dc:title")
                     (mkTextNode "A marvelous thing")
            ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("dc", "http://purl.org/dc/elements/1.1/")
                                           , ("ex", "http://example.org/stuff/1.0/")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

-- * Abbreviating URIs: rdf:ID and xml:base
test_parseXmlRDF_example16 :: Assertion
test_parseXmlRDF_example16 = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
            \ xmlns:ex=\"http://example.org/stuff/1.0/\"\
            \ xml:base=\"http://example.org/here/\">\
      \<rdf:Description rdf:ID=\"snack\">\
        \<ex:prop rdf:resource=\"fruit/apple\"/>\
      \</rdf:Description>\
    \</rdf:RDF>"
    ( mkRdf [ Triple (unode "http://example.org/here/#snack") (unode "ex:prop") (unode "http://example.org/here/fruit/apple") ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("base", "http://example.org/here/")
                                           , ("ex", "http://example.org/stuff/1.0/")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

-- * Container Membership Property Elements: rdf:li and rdf:_n
test_parseXmlRDF_example17 :: Assertion
test_parseXmlRDF_example17 = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">\
      \<rdf:Seq rdf:about=\"http://example.org/favourite-fruit\">\
        \<rdf:_1 rdf:resource=\"http://example.org/banana\"/>\
        \<rdf:_2 rdf:resource=\"http://example.org/apple\"/>\
        \<rdf:_3 rdf:resource=\"http://example.org/pear\"/>\
      \</rdf:Seq>\
    \</rdf:RDF>"
    ( mkRdf [ Triple (unode "http://example.org/favourite-fruit")
                     (unode "rdf:type")
                     (unode "rdf:Seq")
            , Triple (unode "http://example.org/favourite-fruit")
                     (unode "rdf:_1")
                     (unode "http://example.org/banana")
            , Triple (unode "http://example.org/favourite-fruit")
                     (unode "rdf:_2")
                     (unode "http://example.org/apple")
            , Triple (unode "http://example.org/favourite-fruit")
                     (unode "rdf:_3")
                     (unode "http://example.org/pear")
            ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

test_parseXmlRDF_example18 :: Assertion
test_parseXmlRDF_example18 = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">\
      \<rdf:Seq rdf:about=\"http://example.org/favourite-fruit\">\
        \<rdf:li rdf:resource=\"http://example.org/banana\"/>\
        \<rdf:li rdf:resource=\"http://example.org/apple\"/>\
        \<rdf:li rdf:resource=\"http://example.org/pear\"/>\
      \</rdf:Seq>\
    \</rdf:RDF>"
    ( mkRdf [ Triple (unode "http://example.org/favourite-fruit")
                     (unode "rdf:type")
                     (unode "rdf:Seq")
            , Triple (unode "http://example.org/favourite-fruit")
                     (unode "rdf:_1")
                     (unode "http://example.org/banana")
            , Triple (unode "http://example.org/favourite-fruit")
                     (unode "rdf:_2")
                     (unode "http://example.org/apple")
            , Triple (unode "http://example.org/favourite-fruit")
                     (unode "rdf:_3")
                     (unode "http://example.org/pear")
            ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

-- * Collections: rdf:parseType="Collection"
test_parseXmlRDF_example19 :: Assertion
test_parseXmlRDF_example19 = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
            \ xmlns:ex=\"http://example.org/stuff/1.0/\">\
      \<rdf:Description rdf:about=\"http://example.org/basket\">\
        \<ex:hasFruit rdf:parseType=\"Collection\">\
          \<rdf:Description rdf:about=\"http://example.org/banana\"/>\
          \<rdf:Description rdf:about=\"http://example.org/apple\"/>\
          \<rdf:Description rdf:about=\"http://example.org/pear\"/>\
        \</ex:hasFruit>\
      \</rdf:Description>\
    \</rdf:RDF>"
    ( mkRdf [ Triple (unode "http://example.org/basket") (unode "ex:hasFruit") (BNodeGen 1)
            , Triple (BNodeGen 1)
                     (unode "rdf:first")
                     (unode "http://example.org/banana")
            , Triple (BNodeGen 1)
                     (unode "rdf:rest")
                     (BNodeGen 2)
            , Triple (BNodeGen 2)
                     (unode "rdf:first")
                     (unode "http://example.org/apple")
            , Triple (BNodeGen 2)
                     (unode "rdf:rest")
                     (BNodeGen 3)
            , Triple (BNodeGen 3)
                     (unode "rdf:first")
                     (unode "http://example.org/pear")
            , Triple (BNodeGen 3)
                     (unode "rdf:rest")
                     (unode "rdf:nil")
            ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("ex", "http://example.org/stuff/1.0/")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

-- * Reifying Statements: rdf:ID
test_parseXmlRDF_example20 :: Assertion
test_parseXmlRDF_example20 = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
            \ xmlns:ex=\"http://example.org/stuff/1.0/\"\
            \ xml:base=\"http://example.org/triples/\">\
      \<rdf:Description rdf:about=\"http://example.org/\">\
        \<ex:prop rdf:ID=\"triple1\">blah</ex:prop>\
      \</rdf:Description>\
    \</rdf:RDF>"
    ( mkRdf [ Triple (unode "http://example.org/")
                     (unode "ex:prop")
                     (mkTextNode "blah")
            , Triple (unode "http://example.org/triples/#triple1")
                     (unode "rdf:type")
                     (unode "rdf:Statement")
            , Triple (unode "http://example.org/triples/#triple1")
                     (unode "rdf:subject")
                     (unode "http://example.org/")
            , Triple (unode "http://example.org/triples/#triple1")
                     (unode "rdf:predicate")
                     (unode "ex:prop")
            , Triple (unode "http://example.org/triples/#triple1")
                     (unode "rdf:object")
                     (mkTextNode "blah")
            ]
            ( Just (BaseUrl ("http://example.org/here/")) )
            ( PrefixMappings (Map.fromList [ ("ex", "http://example.org/stuff/1.0/")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

test_parseXmlRDF_vCardPersonal :: Assertion
test_parseXmlRDF_vCardPersonal = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
            \ xmlns:v=\"http://www.w3.org/2006/vcard/ns#\">\
      \<v:VCard rdf:about = \"http://example.com/me/corky\" >\
        \<v:fn>Corky Crystal</v:fn>\
        \<v:nickname>Corks</v:nickname>\
        \<v:tel>\
          \<rdf:Description>\
            \<rdf:value>+61 7 5555 5555</rdf:value>\
            \<rdf:type rdf:resource=\"http://www.w3.org/2006/vcard/ns#Home\"/>\
            \<rdf:type rdf:resource=\"http://www.w3.org/2006/vcard/ns#Voice\"/>\
          \</rdf:Description>\
        \</v:tel>\
        \<v:email rdf:resource=\"mailto:corky@example.com\"/>\
        \<v:adr>\
          \<rdf:Description>\
            \<v:street-address>111 Lake Drive</v:street-address>\
            \<v:locality>WonderCity</v:locality>\
            \<v:postal-code>5555</v:postal-code>\
            \<v:country-name>Australia</v:country-name>\
            \<rdf:type rdf:resource=\"http://www.w3.org/2006/vcard/ns#Home\"/>\
          \</rdf:Description>\
        \</v:adr>\
      \</v:VCard>\
    \</rdf:RDF>"
    ( mkRdf [ Triple (unode "http://example.com/me/corky")
                     (unode "rdf:type")
                     (unode "v:VCard")
            , Triple (unode "http://example.com/me/corky")
                     (unode "v:fn")
                     (mkTextNode "Corky Crystal")
            , Triple (unode "http://example.com/me/corky")
                     (unode "v:nickname")
                     (mkTextNode "Corks")
            , Triple (unode "http://example.com/me/corky")
                     (unode "v:tel")
                     (BNodeGen 1)
            , Triple (BNodeGen 1)
                     (unode "rdf:value")
                     (mkTextNode "+61 7 5555 5555")
            , Triple (BNodeGen 1)
                     (unode "rdf:type")
                     (unode "http://www.w3.org/2006/vcard/ns#Home")
            , Triple (BNodeGen 1)
                     (unode "rdf:type")
                     (unode "http://www.w3.org/2006/vcard/ns#Voice")
            , Triple (unode "http://example.com/me/corky")
                     (unode "v:email")
                     (unode "mailto:corky@example.com")
            , Triple (unode "http://example.com/me/corky")
                     (unode "v:adr")
                     (BNodeGen 2)
            , Triple (BNodeGen 2)
                     (unode "v:street-address")
                     (mkTextNode "111 Lake Drive")
            , Triple (BNodeGen 2)
                     (unode "v:locality")
                     (mkTextNode "WonderCity")
            , Triple (BNodeGen 2)
                     (unode "v:postal-code")
                     (mkTextNode "5555")
            , Triple (BNodeGen 2)
                     (unode "v:country-name")
                     (mkTextNode "Australia")
            , Triple (BNodeGen 2)
                     (unode "rdf:type")
                     (unode "http://www.w3.org/2006/vcard/ns#Home")
            ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("v", "http://www.w3.org/2006/vcard/ns#")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

test_parseXmlRDF_NML :: Assertion
test_parseXmlRDF_NML = testParse
    (T.unlines
    ["<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    ,"<rdf:RDF"
    ,"  xmlns:nml=\"http://schemas.ogf.org/nml/2013/05/base#\""
    ,"  xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\""
    ,">"
    ,"  <nml:Node rdf:about=\"urn:ogf:network:example.org:2014:foo\">"
    ,"    <nml:hasInboundPort>"
    ,"      <nml:Port rdf:about=\"urn:ogf:network:example.org:2014:foo:A1:in\">"
    ,"        <nml:isSink rdf:resource=\"urn:ogf:network:example.org:2014:link:1\"/>"
    ,"      </nml:Port>"
    ,"    </nml:hasInboundPort>"
    ,"  </nml:Node>"
    ,"</rdf:RDF>"
    ])
    ( mkRdf [ Triple (unode "urn:ogf:network:example.org:2014:foo")
                     (unode "rdf:type")
                     (unode "nml:Node")
            , Triple (unode "urn:ogf:network:example.org:2014:foo")
                     (unode "nml:hasInboundPort")
                     (unode "urn:ogf:network:example.org:2014:foo:A1:in")
            , Triple (unode "urn:ogf:network:example.org:2014:foo:A1:in")
                     (unode "rdf:type")
                     (unode "nml:Port")
            , Triple (unode "urn:ogf:network:example.org:2014:foo:A1:in")
                     (unode "nml:isSink")
                     (unode "urn:ogf:network:example.org:2014:link:1")
            ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("nml", "http://schemas.ogf.org/nml/2013/05/base#")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

test_parseXmlRDF_NML2 :: Assertion
test_parseXmlRDF_NML2 = testParse
    (T.unlines
    ["<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    ,"<rdf:RDF"
    ,"  xmlns:nml=\"http://schemas.ogf.org/nml/2013/05/base#\""
    ,"  xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\""
    ,">"
    ,"  <nml:Node rdf:about=\"urn:ogf:network:example.org:2014:foo\">"
    ,"    <nml:hasInboundPort rdf:resource=\"urn:ogf:network:example.org:2014:foo:A1:in\"/>"
    ,"  </nml:Node>"
    ,"  <nml:Port rdf:about=\"urn:ogf:network:example.org:2014:foo:A1:in\">"
    ,"    <nml:isSink rdf:resource=\"urn:ogf:network:example.org:2014:link:1\"/>"
    ,"  </nml:Port>"
    ,"</rdf:RDF>"
    ])
    ( mkRdf [ Triple (unode "urn:ogf:network:example.org:2014:foo")
                     (unode "rdf:type")
                     (unode "nml:Node")
            , Triple (unode "urn:ogf:network:example.org:2014:foo")
                     (unode "nml:hasInboundPort")
                     (unode "urn:ogf:network:example.org:2014:foo:A1:in")
            , Triple (unode "urn:ogf:network:example.org:2014:foo:A1:in")
                     (unode "rdf:type")
                     (unode "nml:Port")
            , Triple (unode "urn:ogf:network:example.org:2014:foo:A1:in")
                     (unode "nml:isSink")
                     (unode "urn:ogf:network:example.org:2014:link:1")
            ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("nml", "http://schemas.ogf.org/nml/2013/05/base#")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )

test_parseXmlRDF_NML3 :: Assertion
test_parseXmlRDF_NML3 = testParse
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\
     \<rdf:RDF\
     \ xmlns:nml=\"http://schemas.ogf.org/nml/2013/05/base#\"\
     \ xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
     \>\
     \ <nml:Node rdf:about=\"urn:ogf:network:example.org:2014:foo\">\
     \   <nml:hasInboundPort rdf:resource=\"urn:ogf:network:example.org:2014:foo:A1:in\"/>\
     \ </nml:Node>\
     \ <nml:Port rdf:about=\"urn:ogf:network:example.org:2014:foo:A1:in\">\
     \   <nml:isSink rdf:resource=\"urn:ogf:network:example.org:2014:link:1\"/>\
     \ </nml:Port>\
     \</rdf:RDF>"
    ( mkRdf [ Triple (unode "urn:ogf:network:example.org:2014:foo")
                     (unode "rdf:type")
                     (unode "nml:Node")
            , Triple (unode "urn:ogf:network:example.org:2014:foo")
                     (unode "nml:hasInboundPort")
                     (unode "urn:ogf:network:example.org:2014:foo:A1:in")
            , Triple (unode "urn:ogf:network:example.org:2014:foo:A1:in")
                     (unode "rdf:type")
                     (unode "nml:Port")
            , Triple (unode "urn:ogf:network:example.org:2014:foo:A1:in")
                     (unode "nml:isSink")
                     (unode "urn:ogf:network:example.org:2014:link:1")
            ]
            Nothing
            ( PrefixMappings (Map.fromList [ ("nml", "http://schemas.ogf.org/nml/2013/05/base#")
                                           , ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#") ]) )
    )
