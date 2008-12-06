import Distribution.Simple
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, buildDir)
import System.Exit
import Control.Monad
import System.Process

main :: IO ()
main = defaultMainWithHooks (simpleUserHooks { runTests = test })

-- TODO: there should be a better way of doing this.
-- The conformance tests would take too long if interpreted, which is
-- why use ghc to compile a test executable and then run it.
test :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
test _ _ _ _ = 
  runCommand runQuickCheckTests >>= waitForProcess >> 
    runCommand compileConformanceTests >>= waitForProcess >> 
    runCommand runConformanceTests >>= waitForProcess >> return ()
  where 
    runQuickCheckTests = "./quickcheck +names Text/RDF/TriplesGraph_Test.hs Text/RDF/MGraph_Test.hs"
    compileConformanceTests = "ghc -O -fglasgow-exts -odir dist/build -hidir dist/build -o test --make Text/RDF/TurtleParser_ConformanceTest.hs"
    runConformanceTests = "./test"
