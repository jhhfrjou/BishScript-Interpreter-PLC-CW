module Interpreter where
import Tokens
import Grammar
import Data.List.Split
import Data.List
import System.IO

main x = (>>=) (fmap head (interpretFromFile x)) resolve

splitLines = fmap (splitOn "\n")
splitCommas = fmap $ map $ splitOn ","
getCSV x = splitCommas $ splitLines $ readFile x

interpretFromFile x = fmap interpret (readFile x)

resolve (File x) = getCSV x
resolve (Take x expression) | valid x expression == False = return [["Statement Not Valid"]]
                                               | otherwise = getCSV "testing.csv"

valid :: [Var] -> Condition -> Bool
valid x y = foldr (&&) True (map (inList $ freeVariables y []) x)

splitStatements [] [] = []
splitStatements [] (x:xs) = []
splitStatements ((TokenEndPipe x):xs) a = parse (a ++ [TokenEndPipe x]):splitStatements xs []
splitStatements (x:xs) a = splitStatements xs (a ++ [x])

interpret x = splitStatements (alexScanTokens x) []

inList :: [Var] -> Var -> Bool
inList [] _ = False
inList (x:xs) y | x == y = True
                | otherwise = False || (inList xs y)

freeVariables :: Condition -> [Var] -> [Var]
freeVariables (Equals x1 x2) y = x1:x2:y
freeVariables (Conjoin x1 x2) y = freeVariables x1 y ++ freeVariables x2 y
freeVariables (Disjunction x1 x2) y = freeVariables x1 y ++ freeVariables x2 y
freeVariables (Not x) y = freeVariables x y
freeVariables (Exists x1 x2) y = (freeVariables x2 y) \\ x1
freeVariables (Ref _ x) y  = x ++ y
