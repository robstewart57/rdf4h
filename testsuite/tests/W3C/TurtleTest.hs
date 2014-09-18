module W3C.TurtleTest where

-- 1. Load manifest.ttl
-- 2. Map every test to its result
-- (Determine the test type and call appropriate test function)

import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as T

import W3C.Manifest

manifests :: [(String, String)]
manifests = [("data/w3c/turtle/manifest.ttl", "http://www.w3.org/2013/TurtleTests/")]

tests :: [Test]
tests = [ testGroup "W3C Turtle Tests" allTurtleTests ]

allTurtleTests :: [Test]
allTurtleTests = undefined
