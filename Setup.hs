import Distribution.Simple
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, buildDir)
import System.Exit
import Control.Monad
import System.Process

main :: IO ()
main = defaultMainWithHooks (simpleUserHooks { runTests = test })

test :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
test _ _ _ _ = 
  runCommand runQuickCheckTests >>= waitForProcess >> 
    runCommand compileConformanceTests >>= waitForProcess >> 
    runCommand runConformanceTests >>= waitForProcess >> return ()
  where 
    runQuickCheckTests = "./quickcheck +names -fbang-patterns Text/RDF/TriplesGraph_Test.hs Text/RDF/MGraph_Test.hs"
    compileConformanceTests = "ghc -O2 -fglasgow-exts -fbang-patterns -odir dist/build -hidir dist/build -o test --make Text/RDF/TurtleParser_ConformanceTest.hs"
    runConformanceTests = "./test"
