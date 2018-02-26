module Main where
import Tokens
import Grammar
import Data.List.Split
import Data.List
import System.IO
import System.Environment
--Calc from a file

main = (>>=) (main1 getArgs) print

main1  x = (>>=) (fmap head x) eval

eval x = (>>=) (interpretFromFile x) resolve

--Reads file Returns AST

interpretFromFile x = fmap interpret (readFile x)

--Gets a List of ASTs from String

interpret x = splitStatements (alexScanTokens x) []

--Splitting File into multiple statements to evaluate
splitStatements :: [Token] -> [Token] -> [Program]
splitStatements [] [] = []
splitStatements [] (x:xs) = []
splitStatements (TokenEndPipe x:xs) a = parse (a ++ [TokenEndPipe x]):splitStatements xs []
splitStatements (x:xs) a = splitStatements xs (a ++ [x])

--CSV Reading
splitLines :: IO String -> IO [String]
splitLines = fmap (splitOn "\n")

splitCommas :: IO [String] -> IO [[String]]
splitCommas = fmap $ map $ splitOn ","
getCSV x = splitCommas $ splitLines $ readFile x

resolve :: [Program] -> IO [[String]]
resolve x = resolver x []

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
