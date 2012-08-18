module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Text.RDF.RDF4H.TurtleParser_ConformanceTest as TurtleParser
import qualified Data.RDF.TriplesGraph_Test as TriplesGraph
import qualified Data.RDF.MGraph_Test as MGraph
import qualified Text.RDF.RDF4H.XmlParser_Test as XmlParser

main :: IO () 
main = defaultMain (  TriplesGraph.tests
                   ++ MGraph.tests
                   -- ++ TurtleParser.test -- TODO: Implement TurtleParses `tests'
                   ++ XmlParser.tests
                   )

