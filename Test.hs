import Data.Geo.WKT
import Text.Trifecta
import Test.Tasty
import Test.Tasty.Golden
import System.FilePath

testVsFile :: String -> TestTree
testVsFile name =
    goldenVsFile name expectedFile outFile run
  where
    expectedFile = "tests"</>name<.>"expected"
    outFile      = "tests"</>name<.>"out"
    run = parseFromFile projectedCS ("tests"</>name) >>= writeFile outFile . show

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ testVsFile "test.prj"
    ]
