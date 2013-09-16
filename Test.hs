import Data.Geo.WKT
import Text.ParserCombinators.Parsec

main = do
    parseFromFile projectedCS "test.prj" >>= print
