{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.RDF

main :: IO ()
main = do
  -- create an empty RDF graph to be backed by a list based
  -- implementation of the graph
  let myEmptyGraph = empty :: RDF TList
  -- add a triple to the empty graph
      triple1 = triple
        (unode "http://www.example.com/rob")
        (unode "http://xmlns.com/foaf/0.1/interest")
        (unode "http://dbpedia.org/resource/Scotch_whisky")
      graph1 = addTriple myEmptyGraph triple1
  -- add another triple to the graph
      triple2 = triple
        (unode "http://www.example.com/rob")
        (unode "http://xmlns.com/foaf/0.1/interest")
        (unode "http://dbpedia.org/resource/Haskell_(programming_language)")
      graph2 = addTriple graph1 triple2
  -- remove one of my interests
      graph3 = removeTriple graph2 triple1
  putStrLn (showGraph graph3)
