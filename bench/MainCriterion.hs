{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import Criterion
import Criterion.Main
import Data.RDF
import qualified Data.Text as T

-- The `countries.ttl` Turtle file is needed to run this benchmark suite
--
-- $ wget http://telegraphis.net/data/countries/countries.ttl

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
   env (readFile "countries.ttl") $ \ ~(ttl_countries) ->
   bgroup "parse" [
     bench "TriplesGraph" $
       nf (parseTurtle  :: String -> TriplesGraph) ttl_countries
   , bench "MGraph" $
       nf (parseTurtle  :: String -> MGraph) ttl_countries
   , bench "PatriciaTreeGraph" $
       nf (parseTurtle  :: String -> PatriciaTreeGraph) ttl_countries
   ]

   ,
   env (do ttl_countries <- readFile "countries.ttl"
           let (Right rdf1) = parseString (TurtleParser Nothing Nothing) (T.pack ttl_countries)
           let (Right rdf2) = parseString (TurtleParser Nothing Nothing) (T.pack ttl_countries)
           let (Right rdf3) = parseString (TurtleParser Nothing Nothing) (T.pack ttl_countries)
           return (rdf1 :: PatriciaTreeGraph,rdf2 :: TriplesGraph,rdf3 :: MGraph) )
     $ \ ~(patriciaTreeCountries,triplesGraph,mGraph) ->
   bgroup "query" [
     bench "TriplesGraph" $
       nf (queryGr  :: (Maybe Node,Maybe Node,Maybe Node,TriplesGraph) -> [Triple])
              (Just (UNode "http://telegraphis.net/data/countries/AF#AF"),
               Just (UNode "geographis:capital"),
               Just (UNode "http://telegraphis.net/data/capitals/AF/Kabul#Kabul"),
               triplesGraph)
   , bench "MGraph" $
       nf (queryGr  :: (Maybe Node,Maybe Node,Maybe Node,MGraph) -> [Triple])
              (Just (UNode "http://telegraphis.net/data/countries/AF#AF"),
               Just (UNode "geographis:capital"),
               Just (UNode "http://telegraphis.net/data/capitals/AF/Kabul#Kabul"),
               mGraph)
   , bench "PatriciaTreeGraph" $
       nf (queryGr  :: (Maybe Node,Maybe Node,Maybe Node,PatriciaTreeGraph) -> [Triple])
              (Just (UNode "http://telegraphis.net/data/countries/AF#AF"),
               Just (UNode "geographis:capital"),
               Just (UNode "http://telegraphis.net/data/capitals/AF/Kabul#Kabul"),
               patriciaTreeCountries)
   ]

   ,
   env (do ttl_countries <- readFile "countries.ttl"
           let (Right rdf1) = parseString (TurtleParser Nothing Nothing) (T.pack ttl_countries)
           let (Right rdf2) = parseString (TurtleParser Nothing Nothing) (T.pack ttl_countries)
           let (Right rdf3) = parseString (TurtleParser Nothing Nothing) (T.pack ttl_countries)
           return (rdf1 :: PatriciaTreeGraph,rdf2 :: TriplesGraph,rdf3 :: MGraph) )
     $ \ ~(patriciaTreeCountries,triplesGraph,mGraph) ->
   bgroup "select" [
     bench "TriplesGraph" $
       nf (selectGr  :: (NodeSelector,NodeSelector,NodeSelector,TriplesGraph) -> [Triple])
              (Just (\case { (UNode n) -> T.length n > 12 ; _ -> False } ),
               Just (\case { (UNode n) -> T.length n > 12 ; _ -> False } ),
               Just (\case { (UNode n) -> T.length n > 12 ; _ -> False } ),
               triplesGraph)
   , bench "MGraph" $
       nf (selectGr  :: (NodeSelector,NodeSelector,NodeSelector,MGraph) -> [Triple])
              (Just (\case { (UNode n) -> T.length n > 12 ; _ -> False } ),
               Just (\case { (UNode n) -> T.length n > 12 ; _ -> False } ),
               Just (\case { (UNode n) -> T.length n > 12 ; _ -> False } ),
               mGraph)
   , bench "PatriciaTreeGraph" $
       nf (selectGr  :: (NodeSelector,NodeSelector,NodeSelector,PatriciaTreeGraph) -> [Triple])
              (Just (\case { (UNode n) -> T.length n > 12 ; _ -> False } ),
               Just (\case { (UNode n) -> T.length n > 12 ; _ -> False } ),
               Just (\case { (UNode n) -> T.length n > 12 ; _ -> False } ),
               patriciaTreeCountries)
   ]
 ]
