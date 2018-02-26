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
resolve x = resolve' x []

resolve' :: [Program] -> [(String, IO [[String]])] -> IO [[String]]
resolve' [] fileRef = return [["No Statement"]]
resolve' (File name alias:xs) fileRefs = resolve' xs ((alias,getCSV name):fileRefs)
resolve' (Take variables condition:xs) fileRefs | valid variables (freeVariables condition) = resolver' condition fileRefs (getVariables condition)
                                                | otherwise = return [["Not all variables used/ Returning Bound Variables"]]

getVariable :: Var -> [(String, Var, Int)] -> (String, Int)
getVariable x [] = ("" ,-1)
getVariable x ((a,b,c):xs) | x == b = (a,c)
                           | otherwise = getVariable x xs

--Returns from a list of variables, the variable mapping and the files to the lists of that column
printVariables x a b = map (printColumn a b) x

printprint [] = print ""
printprint (x:xs) = do
                    x >>= print
                    printprint xs

printColumn :: [(String, Var, Int)] -> [(String, IO [[String]])] -> Var -> IO [String]
printColumn variables files x= fmap (map (!! (snd $ getVariable x variables))) (findCSV (fst $ getVariable x variables) files)

resolver' :: Condition -> [(String, IO [[String]])] -> [(String, Var, Int)] -> IO [[String]]
resolver' (Ref name list) fileRefs variables = findCSV name fileRefs
resolver' (Equals x1 x2) fileRefs variables = return [["rip"]]
resolver' _ _ _ = return [["TODO"]]

findCSV :: String -> [(String, IO [[String]])] -> IO [[String]]
findCSV variable [] = return [["No Variable Found"]]
findCSV variable (x:xs) | fst x == variable = snd x
                                       | otherwise = findCSV variable xs

getColumns :: String -> [Var] -> Int -> [(String, Var, Int)]
getColumns _ [] _ = []
getColumns file (x:xs) y = (file,x,y):getColumns file xs (y+1)


getVariables :: Condition ->  [(String,Var,Int)]
getVariables (Ref name list) = getColumns name list 0
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

function (Take x y) = getVariables y

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
