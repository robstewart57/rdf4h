{-# LANGUAGE OverloadedStrings #-}

module Text.RDF.RDF4H.XmlParser_Test
  (
    tests
  ) where

-- todo: QuickCheck tests

import Data.Semigroup ((<>))
-- Testing imports
import Test.Tasty
import Test.Tasty.HUnit as TU

-- Import common libraries to facilitate tests
import qualified Data.Map as Map
import Data.RDF.Query
import Data.RDF.Graph.TList (TList)
import Data.RDF.Types
import qualified Data.Text.IO as TIO
import qualified Data.Text as T (Text, pack, unlines)
import Text.RDF.RDF4H.XmlParser
import Text.RDF.RDF4H.NTriplesParser
import Text.Printf

tests :: [TestTree]
tests =
 [ testCase "simpleStriping1" test_simpleStriping1
 , testCase "simpleStriping2" test_simpleStriping2
 , testCase "simpleSingleton1" test_simpleSingleton1
 , testCase "simpleSingleton2" test_simpleSingleton2
 , testCase "vCardPersonal" test_parseXmlRDF_vCardPersonal
 , testCase "NML" test_parseXmlRDF_NML
 , testCase "NML2" test_parseXmlRDF_NML2
 , testCase "NML3" test_parseXmlRDF_NML3
 ]
 <>
 fmap (uncurry checkGoodOtherTest) otherTestFiles

otherTestFiles :: [(String, String)]
otherTestFiles = [ ("data/xml", "example07")
                 , ("data/xml", "example08")
                 -- https://gitlab.com/k0001/xmlbf/merge_requests/9
                 -- , ("data/xml", "example09")
                 , ("data/xml", "example10")
                 , ("data/xml", "example11")
                 , ("data/xml", "example12")
                 , ("data/xml", "example13")
                 , ("data/xml", "example14")
                 , ("data/xml", "example15")
                 , ("data/xml", "example16")
                 , ("data/xml", "example17")
                 , ("data/xml", "example18")
                 , ("data/xml", "example19")
                 , ("data/xml", "example20")

                 -- https://github.com/robstewart57/rdf4h/issues/48
                 , ("data/xml", "example22")
                 ]

checkGoodOtherTest :: String -> String -> TestTree
checkGoodOtherTest dir fname =
    let expGr = loadExpectedGraph1 (printf "%s/%s.out" dir fname :: String)
        inGr  = loadInputGraph1 dir fname
    in doGoodConformanceTest expGr inGr $ printf "xml-%s" fname

loadExpectedGraph1 :: String -> IO (Either ParseFailure (RDF TList))
loadExpectedGraph1 fname = do
  content <- TIO.readFile fname
  return $ parseString NTriplesParser content

loadInputGraph1 :: String -> String -> IO (Either ParseFailure (RDF TList))
loadInputGraph1 dir fname =
  (parseString (XmlParser Nothing (mkDocUrl1 testBaseUri dir fname)) <$>
     TIO.readFile (printf "%s/%s.rdf" dir fname :: String))

doGoodConformanceTest   :: IO (Either ParseFailure (RDF TList)) ->
                           IO (Either ParseFailure (RDF TList)) ->
                           String -> TestTree
doGoodConformanceTest expGr inGr testname =
    let t1 = assertLoadSuccess (printf "expected (%s): " testname) expGr
        t2 = assertLoadSuccess (printf "   input (%s): " testname) inGr
        t3 = assertEquivalent testname expGr inGr
    in testGroup (printf "conformance-%s" testname) $ fmap (uncurry testCase) [("loading-expected-graph-data", t1), ("loading-input-graph-data", t2), ("comparing-graphs", t3)]

mkTextNode :: T.Text -> Node
mkTextNode = lnode . plainL

testParse :: T.Text -> RDF TList -> Assertion
testParse exRDF ex =
    case parsed of
      Right result ->
          assertBool
            ("expected: " <> show ex <> "but got: " <> show result)
            (isIsomorphic (result :: RDF TList) (ex :: RDF TList))
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


test_parseXmlRDF_vCardPersonal :: Assertion
test_parseXmlRDF_vCardPersonal = testParse
    "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
            \ xmlns:v=\"http://www.w3.org/2006/vcard/ns#\">\
      \<v:VCard rdf:about=\"http://example.com/me/corky\" >\
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

-- TODO: refactor out the following functions, since these are copied from TurtleParser_ConformanceTest

assertEquivalent :: Rdf a => String -> IO (Either ParseFailure (RDF a)) -> IO (Either ParseFailure (RDF a)) -> TU.Assertion
assertEquivalent testname r1 r2 = do
  gr1 <- r1
  gr2 <- r2
  case equivalent gr1 gr2 of
    Nothing    -> return ()
    (Just msg) -> fail $ "Graph " <> testname <> " not equivalent to expected:\n" <> msg

-- Determines if graphs are equivalent, returning Nothing if so or else a diagnostic message.
-- First graph is expected graph, second graph is actual.
equivalent :: Rdf a => Either ParseFailure (RDF a) -> Either ParseFailure (RDF a) -> Maybe String
equivalent (Left _) _                = Nothing
equivalent _        (Left _)         = Nothing
equivalent (Right gr1) (Right gr2)   = test $! zip gr1ts gr2ts
  where
    gr1ts = uordered $ uniqTriplesOf gr1 -- triplesOf gr1
    gr2ts = uordered $ uniqTriplesOf gr2 -- triplesOf gr2
    test []           = Nothing
    test ((t1,t2):ts) =
      case compareTriple t1 t2 of
        Nothing -> test ts
        err     -> err
    compareTriple t1 t2 =
      if equalNodes s1 s2 && equalNodes p1 p2 && equalNodes o1 o2
        then Nothing
        else Just ("Expected:\n  " <> show t1 <> "\nFound:\n  " <> show t2 <> "\n")
      where
        (s1, p1, o1) = f t1
        (s2, p2, o2) = f t2
        f t = (subjectOf t, predicateOf t, objectOf t)
    -- equalNodes (BNode fs1) (BNodeGen i) = T.reverse fs1 == T.pack ("_:genid" <> show i)
    -- equalNodes (BNode fs1) (BNodeGen i) = fs1 == T.pack ("_:genid" <> show i)

    -- I'm not sure it's right to compare blank nodes with generated
    -- blank nodes. This is because parsing an already generated blank
    -- node is parsed as a blank node. Moreover, a parser is free to
    -- generate the blank node how ever they wish. E.g. parsing [] could be:
    --
    -- _:genid1
    --
    -- or
    --
    -- _:Bb71dd4e4b81c097db8d7f79078bbc7c0
    --
    -- which just so happens to be what Apache Jena just created when
    -- [] was parsed.
    equalNodes (BNode _) (BNodeGen _) = True
    equalNodes (BNodeGen _) (BNode _) = True
    equalNodes (BNodeGen _) (BNodeGen _) = True
    equalNodes (BNode _) (BNode _) = True
    equalNodes n1          n2           = n1 == n2

assertLoadSuccess :: String -> IO (Either ParseFailure (RDF TList)) -> TU.Assertion
assertLoadSuccess idStr exprGr = do
  g <- exprGr
  case g of
    Left (ParseFailure err) -> TU.assertFailure $ idStr  <> err
    Right _ -> return ()

-- assertLoadFailure idStr exprGr = do
--   g <- exprGr
--   case g of
--     Left _ -> return ()
--     Right _ -> TU.assertFailure $ "Bad test " <> idStr <> " loaded successfully."

handleLoad :: Either ParseFailure (RDF TList) -> Either ParseFailure (RDF TList)
handleLoad res =
  case res of
    l@(Left _)  -> l
    (Right gr)  -> Right $ mkRdf (fmap normalize (triplesOf gr)) (baseUrl gr) (prefixMappings gr)

normalize :: Triple -> Triple
normalize t = let s' = normalizeN $ subjectOf t
                  p' = normalizeN $ predicateOf t
                  o' = normalizeN $ objectOf t
              in  triple s' p' o'
normalizeN :: Node -> Node
normalizeN (BNodeGen i) = BNode (T.pack $ "_:genid" <> show i)
normalizeN n            = n

-- The Base URI to be used for all conformance tests:
testBaseUri :: String
testBaseUri  = "http://www.w3.org/2001/sw/DataAccess/df1/tests/"

mkDocUrl1 :: String -> String -> String -> Maybe T.Text
mkDocUrl1 baseDocUrl dir fname = Just . T.pack $ printf "%s/%s/%s.rdf" baseDocUrl dir fname
