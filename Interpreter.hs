module Interpreter where
import Tokens
import Grammar
import Data.List.Split

splitLines = fmap (splitOn "\n")
splitCommas = fmap $ map $ splitOn ","
getCSV x = splitCommas $ splitLines $ readFile x

interpretFromFile x = fmap interpret (readFile x)

resolve (File x) = getCSV x
resolve (Take (x:xs) expression) = getCSV "testing.csv"

resolve' :: [Var] -> Condition -> Int
resolve' (x:xs) (Equals x1 x2) = 5
resolve' _ _ = -1

splitStatements [] [] = []
splitStatements [] (x:xs) = []
splitStatements (MkToken x TokenEndPipe:xs) a = parseBish (a ++ [MkToken x TokenEndPipe]):splitStatements xs []
splitStatements (x:xs) a = splitStatements xs (a ++ [x])

interpret x = splitStatements (alexScanTokens x) []

inList :: [Var] -> Var -> Bool
inList [] _ = False
inList (x:xs) y | x == y = True
                | otherwise = False || (inList xs y)

freeVariables :: Condition -> [Var]
freeVariables (Equals x1 x2) = x1:[x2]
freeVariables (Conjoin x1 x2) = freeVariables x1 ++ freeVariables x2
freeVariables (Not x) = freeVariables x
freeVariables _ = []

instance Eq Var where
  x == y = m x y
m :: Var -> Var -> Bool
m (Var x) (Var y) = x == y
m (Int a) (Int b) = a == b
m _ _ = False
