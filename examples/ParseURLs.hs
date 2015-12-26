{-# LANGUAGE ScopedTypeVariables,
             OverloadedStrings #-}

module Main where

import Data.RDF

-- | looks up Tim Berners Lee card.rdf file for talks he has given.
--   returns a single String element: [\"Designing the Web for an Open Society\"].
timBernersLee :: IO ()
timBernersLee = do
    Right (rdf::TriplesList) <- parseURL (XmlParser Nothing Nothing) "http://www.w3.org/People/Berners-Lee/card.rdf"
    let ts = query rdf (Just (UNode "http://www.w3.org/2011/Talks/0331-hyderabad-tbl/data#talk")) (Just (UNode "dct:title")) Nothing
    let talks = map (\(Triple _ _ (LNode (PlainL s))) -> s) ts
    print talks

main :: IO ()
main = timBernersLee
