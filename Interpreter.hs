module Interpreter where
import Tokens
import Grammar
import Data.List.Split
import Data.List
import System.IO
import Data.Csv
--Calc from a file

full x = (>>=) (fmap head (interpretFromFile x)) resolve

--Reads file Returns AST

interpretFromFile x = fmap interpret (readFile x)

--Gets a List of ASTs from String

interpret x = splitStatements (alexScanTokens x) []

--Splitting File into multiple statements to evaluate
splitStatements [] [] = []
splitStatements [] (x:xs) = []
splitStatements (TokenEndPipe x:xs) a = parse (a ++ [TokenEndPipe x]):splitStatements xs []
splitStatements (x:xs) a = splitStatements xs (a ++ [x])

--CSV Reading
splitLines = fmap (splitOn "\n")
splitCommas = fmap $ map $ splitOn ","
getCSV x = splitCommas $ splitLines $ readFile x


multipleForNow x y = fmap (:y) (resolve x)
--Returns the result
resolve :: Program -> IO [[String]]
resolve (File x y) = getCSV x
resolve (Take x e) | valid x e = resolver x e
                   | otherwise = return [["Statement Not Valid"]]

resolver' :: [Program] -> [(String, IO [[String]])] -> IO [[String]]
resolver' [] variables = getCSV "testing.csv"
resolver' (File x y:xs) variables = resolver' xs ((y,getCSV x):variables)
resolver' _ _ = getCSV "testing.csv"

returnAllVariables ::  [(String, IO [[String]])] -> IO [[String]]
returnAllVariables [] = return []
returnAllVariables (x:xs) = snd x

findCSV :: String -> [(String, IO [[String]])] -> IO [[String]]
findCSV variable [] = return [["No Variable Found"]]
findCSV variable (x:xs) | fst x == variable = snd x
                        | otherwise = findCSV variable xs
--TODO Method to solve the expression

resolver x e = return [["TODO"]]

--Checks if the Takes are free in the condition
valid :: [Var] -> Condition -> Bool
valid x y = all (inList $ freeVariables y []) x

--Returns true if in a list
inList :: [Var] -> Var -> Bool
inList [] _ = False
inList (x:xs) y | x == y = True
                | otherwise = inList xs y

--Returns a list of free variables in an expression
freeVariables :: Condition -> [Var] -> [Var]
freeVariables (Equals x1 x2) y = x1:x2:y
freeVariables (Conjoin x1 x2) y = freeVariables x1 y ++ freeVariables x2 y
freeVariables (Disjunction x1 x2) y = freeVariables x1 y ++ freeVariables x2 y
freeVariables (Not x) y = freeVariables x y
freeVariables (Exists x1 x2) y = freeVariables x2 y \\ x1
freeVariables (Ref _ x) y  = x ++ y
