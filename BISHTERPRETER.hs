module BISHTERPRETER where
import BISH
import BISHTOKENS
import Data.List.Split

splitLines = fmap (splitOn "\n")
splitCommas = fmap $ map $ splitOn ","
getCSV x = splitCommas $ splitLines $ readFile x

resolve :: Program -> IO [[String]]
resolve (File x) = getCSV x
resolve _ = getCSV "testing.csv"

splitStatements [] [] = []
splitStatements [] (x:xs) = []
splitStatements (MkToken x TokenEndPipe:xs) a = parseBish (a ++ [MkToken x TokenEndPipe]):splitStatements xs []
splitStatements (x:xs) a = splitStatements xs (a ++ [x])

interpret x = splitStatements (alexScanTokens x) []

calc :: [Program] -> [IO [[String]]]
calc = map resolve
