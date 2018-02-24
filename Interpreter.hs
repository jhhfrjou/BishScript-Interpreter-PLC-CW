module Interpreter where
import Tokens
import Grammar
import Data.List.Split
import Data.List
import System.IO

--Calc from a file

main x = (>>=) (fmap head (interpretFromFile x)) resolve

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
resolve (File x) = getCSV x
resolve (Take x e) | valid x e = resolver x e
                            | otherwise = return [["Statement Not Valid"]]

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
