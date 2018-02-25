module Interpreter where
import Tokens
import Grammar
import Data.List.Split
import Data.List
import System.IO
--Calc from a file
main x = (>>=) (interpretFromFile x) resolve1

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

--Returns the result
resolve :: Program -> IO [[String]]
resolve (File x y) = getCSV x
resolve (Take x e) | valid x (freeVariables e []) = resolver' x e []
                               | otherwise = return [["Statement Not Valid"]]

resolve1 :: [Program] -> IO [[String]]
resolve1 x = resolver x []

resolver :: [Program] -> [(String, IO [[String]])] -> IO [[String]]
resolver [] variables = return [["No Statement"]]
resolver ((File x y):xs) variables = resolver xs ((y,getCSV x):variables)
resolver ((Take x y):xs) variables | valid x (freeVariables y []) = resolver' x y variables
                                                       | otherwise = return [["Statement Not Valid"]]

resolver' (x:xs) (Ref name list) variables = findCSV name variables
resolver' _ _ _ = return [["TODO"]]

returnAllVariables ::  [(String, IO [[String]])] -> IO [[String]]
returnAllVariables [] = return []
returnAllVariables (x:xs) = snd x

findCSV :: String -> [(String, IO [[String]])] -> IO [[String]]
findCSV variable [] = return [["No Variable Found"]]
findCSV variable (x:xs) | fst x == variable = snd x
                        | otherwise = findCSV variable xs
--TODO Method to solve the expression



--Checks if the Takes are free in the condition
valid :: [Var] -> [Var] -> Bool
valid x y = all (inList y) x && all (inList x) y
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
