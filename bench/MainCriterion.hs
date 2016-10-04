{-# LANGUAGE OverloadedStrings, LambdaCase, RankNTypes #-}

module Main where

import Criterion
import Criterion.Types
import Criterion.Main
import Data.RDF
import qualified Data.Text as T

-- The `bills.102.rdf` XML file is needed to run this benchmark suite
--
-- $ wget https://www.govtrack.us/data/rdf/bills.099.actions.rdf.gz
-- $ gzip -d bills.099.actions.rdf.gz

parseXmlRDF :: Rdf a => T.Text -> RDF a
parseXmlRDF s =
  let (Right rdf) = parseString (XmlParser Nothing Nothing) s
  in rdf
{-# INLINE parseXmlRDF #-}

parseNtRDF :: Rdf a => T.Text -> RDF a
parseNtRDF s =
  let (Right rdf) = parseString NTriplesParser s
  in rdf
{-# INLINE parseNtRDF #-}

parseTtlRDF :: Rdf a => T.Text -> RDF a
parseTtlRDF s =
  let (Right rdf) = parseString (TurtleParser Nothing Nothing) s
  in rdf
{-# INLINE parseTtlRDF #-}

queryGr :: Rdf a => (Maybe Node,Maybe Node,Maybe Node,RDF a) -> [Triple]
queryGr (maybeS,maybeP,maybeO,rdf) = query rdf maybeS maybeP maybeO

selectGr :: Rdf a => (NodeSelector,NodeSelector,NodeSelector,RDF a) -> [Triple]
selectGr (selectorS,selectorP,selectorO,rdf) = select rdf selectorS selectorP selectorO

main :: IO ()
main =
  defaultMainWith
    (defaultConfig {resamples = 100})
    [ env
      -- fawltyContent <- T.pack <$> readFile "data/ttl/fawlty1.ttl"
        (do rdfContent <- T.pack <$> readFile "bills.099.actions.rdf"
            let (Right rdf1) =
                  parseString (XmlParser Nothing Nothing) rdfContent
            let (Right rdf2) =
                  parseString (XmlParser Nothing Nothing) rdfContent
            -- let (Right rdf3) =
            --       parseString (XmlParser Nothing Nothing) rdfContent
            -- let (Right rdf4) =
            --       parseString (XmlParser Nothing Nothing) rdfContent
            return
              ( rdf1 :: RDF TList
              , rdf2 :: RDF AdjHashMap
              )) $ \ ~(triplesList, adjMap) ->
        bgroup
          "rdf4h"
          --  bgroup
          --     "parsers"
          --     [ bench "AdjHashMap" $
          --       nf (parseTtlRDF :: T.Text -> RDF AdjHashMap) fawlty_towers
          --     , bench "HashSP" $
          --       nf (parseTtlRDF :: T.Text -> RDF HashSP) fawlty_towers
          --     , bench "SP" $ nf (parseTtlRDF :: T.Text -> RDF SP) fawlty_towers
          --     , bench "TList" $
          --       nf (parseTtlRDF :: T.Text -> RDF TList) fawlty_towers
          --     ]
          -- ,
          [ bgroup
              "query"
              (queryBench "TList" triplesList ++
               queryBench "AdjHashMap" adjMap
               -- queryBench "SP" mapSP ++ queryBench "HashSP" hashMapSP
              )
          , bgroup
              "select"
              (selectBench "TList" triplesList ++
               selectBench "AdjHashMap" adjMap
               -- selectBench "SP" mapSP ++ selectBench "HashSP" hashMapSP
              )
          , bgroup
            "count_triples"
            [ bench "TList" (nf (length . triplesOf) triplesList)
            , bench "AdjHashMap" (nf (length . triplesOf) adjMap)
            ]
          ]
    ]

        

selectBench :: Rdf a => String -> RDF a -> [Benchmark]
selectBench label gr =
   [ bench (label ++ " SPO") $ nf selectGr (subjSelect,predSelect,objSelect,gr)
   , bench (label ++ " SP")  $ nf selectGr (subjSelect,predSelect,selectNothing,gr)
   , bench (label ++ " S")   $ nf selectGr (subjSelect,selectNothing,selectNothing,gr)
   , bench (label ++ " PO")  $ nf selectGr (selectNothing,predSelect,objSelect,gr)
   , bench (label ++ " SO")  $ nf selectGr (subjSelect,selectNothing,objSelect,gr)
   , bench (label ++ " P")   $ nf selectGr (selectNothing,predSelect,selectNothing,gr)
   , bench (label ++ " O")   $ nf selectGr (selectNothing,selectNothing,objSelect,gr)
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
   [ bench (label ++ " SPO") $ nf queryGr (subjQuery,predQuery,objQuery,gr)
   , bench (label ++ " SP")  $ nf queryGr (subjQuery,predQuery,queryNothing,gr)
   , bench (label ++ " S")   $ nf queryGr (subjQuery,queryNothing,queryNothing,gr)
   , bench (label ++ " PO")  $ nf queryGr (queryNothing,predQuery,objQuery,gr)
   , bench (label ++ " SO")  $ nf queryGr (subjQuery,queryNothing,objQuery,gr)
   , bench (label ++ " P")   $ nf queryGr (queryNothing,predQuery,queryNothing,gr)
   , bench (label ++ " O")   $ nf queryGr (queryNothing,queryNothing,objQuery,gr)
   ]
