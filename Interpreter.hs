module Language where
import Tokens
import Grammar
import Data.List.Split

splitLines = fmap (splitOn "\n")
splitCommas = fmap $ map $ splitOn ","
getCSV x = splitCommas $ splitLines $ readFile x

interpretFromFile x = fmap interpret (readFile x)

resolve (File x) = getCSV x
resolve _ = getCSV "testing.csv"

splitStatements [] [] = []
splitStatements [] (x:xs) = []
splitStatements (MkToken x TokenEndPipe:xs) a = parseBish (a ++ [MkToken x TokenEndPipe]):splitStatements xs []
splitStatements (x:xs) a = splitStatements xs (a ++ [x])

interpret x = splitStatements (alexScanTokens x) []
