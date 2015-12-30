{-# LANGUAGE OverloadedStrings, LambdaCase, RankNTypes #-}

module Main where

import Criterion
import Criterion.Main
import Data.RDF
import qualified Data.Text as T

-- The `bills.102.rdf` XML file is needed to run this benchmark suite
--
-- $ wget https://www.govtrack.us/data/rdf/bills.102.rdf.gz
-- $ gzip -d bills.102.rdf.gz

parseTurtle :: RDF rdf => String -> rdf
parseTurtle s =
    let (Right rdf) = parseString (TurtleParser Nothing Nothing) (T.pack s)
    in rdf

queryGr :: RDF rdf => (Maybe Node,Maybe Node,Maybe Node,rdf) -> [Triple]
queryGr (maybeS,maybeP,maybeO,rdf) = query rdf maybeS maybeP maybeO

selectGr :: RDF rdf => (NodeSelector,NodeSelector,NodeSelector,rdf) -> [Triple]
selectGr (selectorS,selectorP,selectorO,rdf) = select rdf selectorS selectorP selectorO

main :: IO ()
main = defaultMain [
   env (readFile "bills.102.ttl") $ \ ~(ttl_countries) ->
   bgroup "parse" [
     bench "HashMapS" $
       nf (parseTurtle  :: String -> HashMapS) ttl_countries
   , bench "HashMapSP" $
       nf (parseTurtle  :: String -> HashMapSP) ttl_countries
   , bench "MapSP" $
       nf (parseTurtle  :: String -> MapSP) ttl_countries
   , bench "TriplesList" $
       nf (parseTurtle  :: String -> TriplesList) ttl_countries
   , bench "ListPatriciaTree" $
       nf (parseTurtle  :: String -> TriplesPatriciaTree) ttl_countries
   ]

   ,
   env (do ttl_countries <- readFile "bills.102.ttl"
           let (Right rdf1) = parseString (TurtleParser Nothing Nothing) (T.pack ttl_countries)
           let (Right rdf2) = parseString (TurtleParser Nothing Nothing) (T.pack ttl_countries)
           let (Right rdf3) = parseString (TurtleParser Nothing Nothing) (T.pack ttl_countries)
           let (Right rdf4) = parseString (TurtleParser Nothing Nothing) (T.pack ttl_countries)
           let (Right rdf5) = parseString (TurtleParser Nothing Nothing) (T.pack ttl_countries)
           return (rdf1 :: TriplesPatriciaTree,rdf2 :: TriplesList,rdf3 :: HashMapS,rdf4 :: MapSP,rdf5::HashMapSP) )
     $ \ ~(triplesPatriciaTree,triplesList,hashMapS,mapSP,hashMapSP) ->
   bgroup "query"
     (queryBench "TriplesList" triplesList
     ++ queryBench "HashMapS" hashMapS
     ++ queryBench "MapSP" mapSP
     ++ queryBench "HashMapSP" hashMapSP
     ++ queryBench "TriplesPatriciaTree" triplesPatriciaTree)

   ,
   env (do ttl_countries <- readFile "bills.102.ttl"
           let (Right rdf1) = parseString (TurtleParser Nothing Nothing) (T.pack ttl_countries)
           let (Right rdf2) = parseString (TurtleParser Nothing Nothing) (T.pack ttl_countries)
           let (Right rdf3) = parseString (TurtleParser Nothing Nothing) (T.pack ttl_countries)
           let (Right rdf4) = parseString (TurtleParser Nothing Nothing) (T.pack ttl_countries)
           let (Right rdf5) = parseString (TurtleParser Nothing Nothing) (T.pack ttl_countries)
           return (rdf1 :: TriplesPatriciaTree,rdf2 :: TriplesList,rdf3 :: HashMapS,rdf4 :: MapSP,rdf5 :: HashMapSP) )
     $ \ ~(triplesPatriciaTree,triplesList,hashMapS,mapSP,hashMapSP) ->
   bgroup "select"
     (selectBench "TriplesList" triplesList
     ++ selectBench "HashMapS" hashMapS
     ++ selectBench "MapSP" mapSP
     ++ selectBench "HashMapSP" hashMapSP
     ++ selectBench "TriplesPatriciaTree" triplesPatriciaTree)
 ]

selectBench :: forall rdf. RDF rdf => String -> rdf -> [Benchmark]
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
subjQuery = Just (UNode "http://www.rdfabout.com/rdf/usgov/congress/102/bills/h5694")
predQuery = Just (UNode "bill:congress")
objQuery  = Just (LNode (PlainL (T.pack "102")))
queryNothing = Nothing

queryBench :: forall rdf. RDF rdf => String -> rdf -> [Benchmark]
queryBench label gr =
   [ bench (label ++ " SPO") $ nf queryGr (subjQuery,predQuery,objQuery,gr)
   , bench (label ++ " SP")  $ nf queryGr (subjQuery,predQuery,queryNothing,gr)
   , bench (label ++ " S")   $ nf queryGr (subjQuery,queryNothing,queryNothing,gr)
   , bench (label ++ " PO")  $ nf queryGr (queryNothing,predQuery,objQuery,gr)
   , bench (label ++ " SO")  $ nf queryGr (subjQuery,queryNothing,objQuery,gr)
   , bench (label ++ " P")   $ nf queryGr (queryNothing,predQuery,queryNothing,gr)
   , bench (label ++ " O")   $ nf queryGr (queryNothing,queryNothing,objQuery,gr)
   ]
