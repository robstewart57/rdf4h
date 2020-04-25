{-# LANGUAGE OverloadedStrings, LambdaCase, RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude hiding (readFile)
#if MIN_VERSION_base(4,9,0)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#else
#endif
#else
#endif
import Criterion
import Criterion.Types
import Criterion.Main
import Data.RDF
import Text.RDF.RDF4H.ParserUtils
import qualified Data.Text as T
import Control.DeepSeq (NFData)

-- The `bills.102.rdf` XML file is needed to run this benchmark suite
--
-- $ wget https://www.govtrack.us/data/rdf/bills.099.actions.rdf.gz
-- $ gzip -d bills.099.actions.rdf.gz

parseXmlRDF :: Rdf a => T.Text -> RDF a
parseXmlRDF = either (error . show) id . parseString (XmlParser Nothing Nothing)
{-# INLINE parseXmlRDF #-}

parseNtRDF :: Rdf a => T.Text -> RDF a
parseNtRDF = either (error . show) id . parseString NTriplesParser
{-# INLINE parseNtRDF #-}

parseTtlRDF :: Rdf a => T.Text -> RDF a
parseTtlRDF = either (error . show) id . parseString (TurtleParser Nothing Nothing)
{-# INLINE parseTtlRDF #-}

queryGr :: Rdf a => (Maybe Node,Maybe Node,Maybe Node,RDF a) -> [Triple]
queryGr (maybeS,maybeP,maybeO,rdf) = query rdf maybeS maybeP maybeO

selectGr :: Rdf a => (NodeSelector,NodeSelector,NodeSelector,RDF a) -> [Triple]
selectGr (selectorS,selectorP,selectorO,rdf) = select rdf selectorS selectorP selectorO

xmlFile :: FilePath
xmlFile = "bills.099.actions.rdf"

main :: IO ()
main = defaultMainWith
    (defaultConfig {resamples = 100})
    [ env
        -- [FIXME] Do not rely on system's defaults to read files.
        (do fawltyContentTurtle <- readFile "data/ttl/fawlty1.ttl"
            fawltyContentNTriples <- readFile "data/nt/all-fawlty-towers.nt"
            xmlContent <- readFile xmlFile
            let rdf1' = parseString (XmlParser Nothing Nothing) xmlContent
                rdf2' = parseString (XmlParser Nothing Nothing) xmlContent
                rdf3' =parseString (XmlParser Nothing Nothing) xmlContent
                rdf1 = either (error . show) id rdf1' :: RDF TList
                rdf2 = either (error . show) id rdf2' :: RDF AdjHashMap
                rdf3 = either (error . show) id rdf3' :: RDF AlgebraicGraph
                triples = triplesOf rdf1
            return (rdf1, rdf2, rdf3, triples, fawltyContentNTriples, fawltyContentTurtle, xmlContent)) $
            \ ~(triplesList, adjMap, algGraph, triples, fawltyContentNTriples, fawltyContentTurtle, xmlContent) ->
        bgroup
          "rdf4h"
           [ bgroup
              "parsers"
              [ bench "ntriples-parsec" $
                nf (\t ->
                      let res = parseString (NTriplesParserCustom Parsec) t :: Either ParseFailure (RDF TList)
                      in either (error . show) id res
                   ) fawltyContentNTriples
              , bench "ntriples-attoparsec" $
                nf (\t ->
                      let res = parseString (NTriplesParserCustom Attoparsec) t :: Either ParseFailure (RDF TList)
                      in either (error . show) id res
                   ) fawltyContentNTriples
              , bench "turtle-parsec" $
                nf (\t ->
                      let res = parseString (TurtleParserCustom Nothing Nothing Parsec) t :: Either ParseFailure (RDF TList)
                      in either (error . show) id res
                   ) fawltyContentTurtle
              , bench "turtle-attoparsec" $
                nf (\t ->
                      let res = parseString (TurtleParserCustom Nothing Nothing Attoparsec)  t :: Either ParseFailure (RDF TList)
                      in either (error . show) id res
                   ) fawltyContentTurtle
              , bench "xml-xmlbf" $
                nf (\t ->
                      let res = parseString (XmlParser Nothing Nothing) t :: Either ParseFailure (RDF TList)
                      in either (error . show) id res
                   ) xmlContent
              -- , bench "xml-xht" $
              --   nf (\t ->
              --         let res = parseString (XmlParserHXT Nothing Nothing) t :: Either ParseFailure (RDF TList)
              --         in either (error . show) id res
              --      ) xmlContent
              ]
          ,
            bgroup
              "query"
              (queryBench "TList" triplesList <>
               queryBench "AdjHashMap" adjMap <>
               queryBench "AlgebraicGraph" algGraph
               -- queryBench "SP" mapSP <> queryBench "HashSP" hashMapSP
              )
          , bgroup
              "select"
              (selectBench "TList" triplesList <>
               selectBench "AdjHashMap" adjMap <>
               selectBench "AlgebraicGraph" algGraph
               -- selectBench "SP" mapSP <> selectBench "HashSP" hashMapSP
              )
          , bgroup
              "add-remove-triples"
              (addRemoveTriples "TList" triples (empty :: RDF TList) triplesList <>
               addRemoveTriples "AdjHashMap" triples (empty :: RDF AdjHashMap) adjMap <>
               addRemoveTriples "AlgebraicGraph" triples (empty :: RDF AlgebraicGraph) algGraph
              )
          , bgroup
            "count_triples"
            [ bench "TList" (nf (length . triplesOf) triplesList)
            , bench "AdjHashMap" (nf (length . triplesOf) adjMap)
            , bench "AlgebraicGraph" (nf (length . triplesOf) algGraph)
            ]
          ]
    ]

selectBench :: Rdf a => String -> RDF a -> [Benchmark]
selectBench label gr =
   [ bench (label <> " SPO") $ nf selectGr (subjSelect,predSelect,objSelect,gr)
   , bench (label <> " SP")  $ nf selectGr (subjSelect,predSelect,selectNothing,gr)
   , bench (label <> " S")   $ nf selectGr (subjSelect,selectNothing,selectNothing,gr)
   , bench (label <> " PO")  $ nf selectGr (selectNothing,predSelect,objSelect,gr)
   , bench (label <> " SO")  $ nf selectGr (subjSelect,selectNothing,objSelect,gr)
   , bench (label <> " P")   $ nf selectGr (selectNothing,predSelect,selectNothing,gr)
   , bench (label <> " O")   $ nf selectGr (selectNothing,selectNothing,objSelect,gr)
   ]

subjSelect, predSelect, objSelect, selectNothing :: Maybe (Node -> Bool)
subjSelect = Just (\case { (UNode n) -> T.length n > 12 ; _ -> False })
predSelect = Just (\case { (UNode n) -> T.length n > 12 ; _ -> False })
objSelect  = Just (\case { (UNode n) -> T.length n > 12 ; _ -> False })
selectNothing = Nothing

subjQuery, predQuery, objQuery, queryNothing :: Maybe Node
subjQuery = Just (UNode "http://www.rdfabout.com/rdf/usgov/congress/99/bills/h4")
predQuery = Just (UNode "bill:hadAction")
objQuery  = Just (BNodeGen 1)
queryNothing = Nothing

queryBench :: Rdf a => String -> RDF a -> [Benchmark]
queryBench label gr =
   [ bench (label <> " SPO") $ nf queryGr (subjQuery,predQuery,objQuery,gr)
   , bench (label <> " SP")  $ nf queryGr (subjQuery,predQuery,queryNothing,gr)
   , bench (label <> " S")   $ nf queryGr (subjQuery,queryNothing,queryNothing,gr)
   , bench (label <> " PO")  $ nf queryGr (queryNothing,predQuery,objQuery,gr)
   , bench (label <> " SO")  $ nf queryGr (subjQuery,queryNothing,objQuery,gr)
   , bench (label <> " P")   $ nf queryGr (queryNothing,predQuery,queryNothing,gr)
   , bench (label <> " O")   $ nf queryGr (queryNothing,queryNothing,objQuery,gr)
   ]

addRemoveTriples :: (NFData (RDF a), Rdf a) => String -> Triples -> RDF a -> RDF a -> [Benchmark]
addRemoveTriples lbl triples emptyGr populatedGr =
   [ bench (lbl <> "-add-triples") $ nf addTriples (triples,emptyGr)
   , bench (lbl <> "-remove-triples") $ nf removeTriples (triples,populatedGr)
   ]

addTriples ::  Rdf a => (Triples,RDF a) -> RDF a
addTriples (triples,emptyGr) =
  foldr (flip addTriple) emptyGr triples

removeTriples ::  Rdf a => (Triples,RDF a) -> RDF a
removeTriples (triples,populatedGr) =
  foldr (flip removeTriple) populatedGr triples
