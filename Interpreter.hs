module Main where
import Tokens
import Grammar
import Data.List.Split
import Data.List
import System.IO
import System.Environment
import Control.Arrow
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
getCSV x = fmap (map (splitOn ",") . lines) (readFile x)

resolve :: [Program] -> IO [[String]]
resolve x = resolver x []

resolver :: [Program] -> [(String, IO [[String]])] -> IO [[String]]
resolver [] fileRef = return [["No Statement"]]
resolver (File name alias:xs) fileRefs = resolver xs ((alias,getCSV name):fileRefs)
resolver (Take variables condition:xs) fileRefs | valid variables (freeVariables condition) = resolver' variables condition fileRefs []
                                                                                 | otherwise = return [["Not all variables used/ Using Bound Variables"]]

resolver' :: [Var] -> Condition -> [(String, IO [[String]])] -> [(Var, Int)] -> IO [[String]]
resolver' (x:xs) (Ref name list) fileRefs variables = findCSV name fileRefs
resolver' _ _ _ _ = return [["TODO"]]

returnAllVariables ::  [(String, IO [[String]])] -> IO [[String]]
returnAllVariables [] = return []
returnAllVariables (x:xs) = snd x

findCSV :: String -> [(String, IO [[String]])] -> IO [[String]]
findCSV variable [] = return [["No Variable Found"]]
findCSV variable (x:xs) | fst x == variable = snd x
                                       | otherwise = findCSV variable xs

getColumns :: [Var] -> Int -> [(Var, Int)]
getColumns [] _ = []
getColumns (x:xs) y = (x,y):getColumns xs (y+1)

getVariables :: Condition ->  [(Var,Int)]
getVariables (Ref name list) = getColumns list 0
getVariables (Equals x1 x2) = []
getVariables (Conjoin x1 x2) = getVariables x1 ++ getVariables x2
getVariables (Disjunction x1 x2) = getVariables x1++ getVariables x2
getVariables (Not x) = getVariables x
getVariables (Exists x1 x2) = getVariables x2
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

function (Take x y) = freeVariables'' y ([],[])

freeVariables :: Condition -> [Var]
freeVariables x = freeVariables' $ freeVariables'' x ([],[])

freeVariables' :: ([Var],[Var]) -> [Var]
freeVariables' (x,y) = rmdups x \\ y

freeVariables'' :: Condition -> ([Var],[Var]) -> ([Var],[Var])
freeVariables'' (Equals x1 x2) (x,y) = (x1:x2:x,y)
freeVariables'' (Conjoin x1 x2) (x,y) = ((++) (fst (freeVariables'' x1 (x, y))) *** (++) (snd (freeVariables'' x1 (x, y)))) (freeVariables'' x2 (x, y))
freeVariables'' (Disjunction x1 x2) (x,y) = ((++) (fst (freeVariables'' x1 (x, y))) *** (++) (snd (freeVariables'' x1 (x, y)))) (freeVariables'' x2 (x, y))
freeVariables'' (Not x1) (x,y) = freeVariables'' x1 (x,y)
freeVariables'' (Exists x1 x2) (x,y) = freeVariables'' x2 (x,x1++y)
freeVariables'' (Ref _ x1) (x,y)  = (x1 ++ x, y)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs
