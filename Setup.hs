import Distribution.Simple
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, buildDir)
import System.Exit
import Control.Monad
import System.Process

main = defaultMain

--main :: IO ()
--main = defaultMainWithHooks (simpleUserHooks { runTests = test })

-- TODO: there should be a better way of doing this.
-- The conformance tests would take too long if interpreted, which is
-- why we use ghc to compile a test executable and then run it.
test :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
test _ _ _ _ = 
  runCommand runQuickCheckTests >>= waitForProcess >> 
    runCommand compileConformanceTests >>= waitForProcess >> 
    runCommand runConformanceTests >>= waitForProcess >> return ()
  where 
    runQuickCheckTests = "./quickcheck +names -i.:src:testsuite/tests testsuite/tests/Data/RDF/TriplesGraph_Test.hs testsuite/tests/Data/RDF/MGraph_Test.hs"
    compileConformanceTests = "ghc -O -fglasgow-exts -odir dist/build -hidir dist/build -isrc:testsuite/tests -o test --make testsuite/tests/Text/RDF/RDF4H/TurtleParser_ConformanceTest.hs"
    runConformanceTests = "./test"
