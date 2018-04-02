module Main where
import Tokens
import Grammar
import Data.List.Split
import Data.List
import System.IO
import System.IO.Unsafe
import System.Environment
import Control.Arrow
import Control.Monad
import Data.Map
import Data.Function
--Calc from a file

main = (>>=) (main1 getArgs) (mapM_ putStrLn)

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

--Turn List of Programs into a set of output lines
resolve :: [Program] -> IO [String]
resolve x =fmap (Data.List.map $ intercalate ",") $ listResolver x empty

--Loops through, adds files to a list and does basic error handling
listResolver :: [Program] -> Map String (IO [[String]]) -> IO [[String]]
listResolver [] fileRef = return [["No Statement"]]
listResolver (File name alias:xs) fileRefs = listResolver xs (Data.Map.insert alias (getCSV name) fileRefs)
listResolver (Take variables condition:xs) fileRefs | Data.List.null variables = error "Nothing is outputted"
                                                    | not $ validFileRefs condition fileRefs = error "The references to a file are not connected to file"
                                                    | not $ valid' variables (freeVariables condition) = error "Not all variables used/ Returning Bound Variables/ Bound Variables not Initialised yet"
                                                    | not $ fileRefsSameLength condition fileRefs = error "The number of columns in the csv is not equal to number in references to that file"
                                                    | otherwise = createResult (createFinalColumns $ resolver' (changeData condition) fileRefs (return[[]],empty)) variables

--Outputting File
createResult :: Map Var (IO [String]) -> [Var] -> IO [[String]]
createResult mappings x | Data.Map.null mappings = return [[]]
                                            | otherwise =  fmap (sort . transpose) (mapM (mappings !) x)

--Gets the columns from the mapping
createFinalColumns :: (IO [[String]] ,Map Var Int) -> Map Var (IO [String])
createFinalColumns (finalCSV, mapping) |unsafePerformIO $ fmap (all Data.List.null) finalCSV = empty
                                                                        |otherwise =  Data.Map.map (helperFunction finalCSV) mapping
--Gets the column from a csv file
helperFunction ::  IO [[String]] -> Int -> IO [String]
helperFunction file column = fmap ((!! column) . transpose) file

--CSV Reading
getCSV x = fmap (Data.List.map (Data.List.map (dropWhile (== ' ')) . splitOn ",") . lines) (readFile x)

--Try to actually solve the thing
resolver' :: [Condition'] -> Map String (IO [[String]])-> (IO [[String]] , Map Var Int)  ->(IO [[String]] , Map Var Int)
resolver' [] _  (currentStatement, currentMapping) =  (currentStatement, currentMapping)
resolver' (Ref' name list:xs) fileRefs  (currentStatement, currentMapping) =  resolver' xs fileRefs $ updateMappings (currentStatement, currentMapping) (fileRefs ! name , variablesToMap list 0)
resolver' (Equals' x1 x2:xs) fileRefs  (currentStatement, currentMapping) =  resolver' xs fileRefs (fmap (removeNonEquals currentMapping x1 x2) currentStatement, currentMapping)
resolver' (NotEquals' x1 x2:xs) fileRefs  (currentStatement, currentMapping) =  resolver' xs fileRefs (fmap (removeEquals currentMapping x1 x2) currentStatement, currentMapping)
resolver' (Exists' var list:xs) fileRefs  (currentStatement, currentMapping) =  resolver' xs fileRefs $ resolver' list fileRefs (currentStatement, currentMapping)

--First part is just the 2 combined and the second files mappings altered by the number of maps in the first map
updateMappings :: (IO [[String]] ,Map Var Int) -> (IO [[String]] ,Map Var Int) -> (IO [[String]] ,Map Var Int)
updateMappings (file,vars)  (file2,vars2) |any (`Data.Map.member` vars2) $ keys $ Data.Map.mapKeys addLetter vars2 = updateMappings (file,vars) $ removeDuplicateColumns (fmap (removeNonEqualsInSameFile vars2) file2 , vars2)
                                                                      | size (intersection vars2 vars) == 0 =  (liftM2 combineArrays file file2, Data.Map.union vars (updateNums vars2 $ size vars))
                                                                     | otherwise = removeDuplicateColumns (fmap (removeUnequalRows ( duplicatePairs (toList $ intersection vars vars2) (toList $ onlyDups (combineMaps vars vars2)))) (liftM2 combineArrays file file2) , combineMaps vars vars2)

--combines 2 2d lists to form the cartian product
combineArrays :: [[a]] -> [[a]] -> [[a]]
combineArrays a b = [ x ++ y | x <- a , y <-b]

--combine 2 maps by increasing the numbers of the columns in the second map and handles for duplicates by adding a &
combineMaps :: Map Var Int  -> Map Var Int ->  Map Var Int
combineMaps vars vars2 = Data.Map.union vars $ Data.Map.union (updateNums (Data.Map.mapKeys addLetter (intersection vars2 vars)) $ size vars) (updateNums vars2 $ size vars)

--Adds a letter to symbalized the duplicates
addLetter :: Var -> Var
addLetter (Var x) = Var ('&':x)

--From 2 Maps of repeated columns returns t
duplicatePairs :: [(Var,Int)] -> [(Var,Int)] -> [(Int,Int)]
duplicatePairs [] [] = []
duplicatePairs ((var1,int1) : xs) ((var2, int2) : ys) = (int1,int2): duplicatePairs xs ys

--Checks if it starts with & so has a duplicate
hasDuplicate :: Var -> Bool
hasDuplicate (Var ('&':x)) = True
hasDuplicate _ = False

--Returns all values in a map that starts with &
onlyDups :: Map Var Int -> Map Var Int
onlyDups = filterWithKey (\k _ -> hasDuplicate k)

--Removes the duplicates from a file from an input of map and file
removeNonEqualsInSameFile ::Map Var Int ->  [[String]] ->  [[String]]
removeNonEqualsInSameFile mappings = removeUnequalRows (Data.List.map (getBindingsOfDuplicates (onlyDups mappings))  $ toList mappings)

--Returns the columns of duplicats
getBindingsOfDuplicates :: Map Var Int -> (Var, Int)  -> (Int,Int)
getBindingsOfDuplicates mappings (variable, column ) | Data.Map.member (addLetter variable) mappings = (column, mappings ! addLetter variable)
                                                                          | otherwise  = (0,0)
--Checks equality of each row
removeUnequalRows :: [(Int, Int)] -> [[String]] -> [[String]]
removeUnequalRows variables x = Data.List.filter (not . Data.List.null) $ Data.List.map (removeUnequalRow variables) x

--Checks all the equality of a row
removeUnequalRow :: [(Int, Int)] -> [String] -> [String]
removeUnequalRow [] file = file
removeUnequalRow ((x,y):xs) file | file !! x == file !! y = removeUnequalRow xs file
                                 | otherwise = []

--Adds a number to the answer of a the map
updateNums :: Map Var Int -> Int -> Map Var Int
updateNums a b = Data.Map.map (+b) a

--Remove non equals from 2 variables and the combo of 2 files
removeNonEquals :: Map Var Int -> Var -> Var -> [[String]] -> [[String]]
removeNonEquals _ _ _ [] = []
removeNonEquals columns var1 var2  (x:xs)| x !! (columns ! var1) == x !! (columns ! var2) = x:removeNonEquals columns var1 var2 xs
                                                                           | otherwise = removeNonEquals columns var1 var2 xs
--Removes equals from 2 variables and the combo of 2 files
removeEquals :: Map Var Int -> Var -> Var -> [[String]] -> [[String]]
removeEquals _ _ _ [] = []
removeEquals columns var1 var2  (x:xs)| x !! (columns ! var1) /= x !! (columns ! var2) = x:removeEquals columns var1 var2 xs
                                         | otherwise = removeNonEquals columns var1 var2 xs

--From a list returns a map of their position in the list in a map
variablesToMap :: [Var] -> Int -> Map Var Int
variablesToMap [] _ = empty
variablesToMap (x:xs) col | x `elem` xs = Data.Map.insert (addLetter x) col (variablesToMap xs (col +1))
                                      | otherwise  = Data.Map.insert x col (variablesToMap xs (col+1))

--Removes duplicate elements (&x) and their columns from the working file and map
removeDuplicateColumns :: (IO [[String]], Map Var Int) -> (IO [[String]], Map Var Int)
removeDuplicateColumns (file, mapping) = (fmap (Data.List.map (deleteElems $ sort $ elems $ onlyDups mapping)) file , variablesToMap (Data.List.map fst $ sortBy (compare `on` snd) $ toList $ Data.Map.difference mapping (onlyDups mapping)) 0)

--Function that removes a number of columns from the string
deleteElems :: [Int] -> [String] -> [String]
deleteElems [] zs = zs
deleteElems xs zs = foldr1 intersect $ Data.List.map (($ zs) . deleteElem) xs

--Removes a column from a list
deleteElem x zs | x > 0 = Data.List.take (x-1) zs ++ Data.List.drop x zs
                | otherwise = zs

---Validity Checking---

--Checks if the files referenced in the program are actually imported
validFileRefs :: Condition -> Map String (IO [[String]]) -> Bool
validFileRefs (Equals x1 x2) files = True
validFileRefs (NotEquals x1 x2) files = True
validFileRefs (Conjoin x1 x2) files = validFileRefs x1 files && validFileRefs x2 files
validFileRefs (Exists x1 x2) files = validFileRefs x2 files
validFileRefs (Ref name list) files | Data.Map.member name files = True
                                    | otherwise = False


--Checks if the files referenced in the program have the same number of variables as columns in the csv
fileRefsSameLength :: Condition -> Map String (IO [[String]]) -> Bool
fileRefsSameLength (Equals x1 x2) files = True
fileRefsSameLength (NotEquals x1 x2) files = True
fileRefsSameLength (Conjoin x1 x2) files = fileRefsSameLength x1 files && fileRefsSameLength x2 files
fileRefsSameLength (Exists x1 x2) files = fileRefsSameLength x2 files
fileRefsSameLength (Ref name list) files | checkLengthDiff (length list) (fmap (length . head) $ files ! name) = True
                                                                    | otherwise = False

--Gimmie a break I know its bad haskell but i couldnt think of another way without redoing the entire thing
checkLengthDiff :: Int -> IO Int -> Bool
checkLengthDiff list mapped = list == unsafePerformIO mapped

--Checks if the outputted variables are free in the condition
valid' :: [Var] -> [Var] -> Bool
valid' x y = all (inList y) x && all (inList x) y

--Returns true if in a list
inList :: [Var] -> Var -> Bool
inList [] _ = False
inList (x:xs) y | x == y = True
                | otherwise = inList xs y


---Free Variable functions---


--Returns a list of free variables in an expression
freeVariables :: Condition -> [Var]
freeVariables x = rmdups $ freeVariables' x

freeVariables' :: Condition -> [Var]
freeVariables' (Equals x1 x2) = [x1,x2]
freeVariables' (NotEquals x1 x2) = [x1,x2]
freeVariables' (Conjoin x1 x2) = freeVariables' x1 ++ freeVariables' x2
freeVariables' (Exists boundVars x2) = freeVariables x2 Data.List.\\ boundVars
freeVariables' (Ref _ list)  = list

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs

---Changing Data Type Functions---

changeData x = pushEqualsToEnd (firstDataChange x) []

firstDataChange :: Condition -> [Condition']
firstDataChange (Equals x1 x2) = [Equals' x1 x2]
firstDataChange (NotEquals x1 x2) = [NotEquals' x1 x2]
firstDataChange (Conjoin x1 x2) = firstDataChange x1 ++ firstDataChange x2
firstDataChange (Exists x1 x2) = [Exists' x1 $ firstDataChange x2]
firstDataChange (Ref name x1)  = [Ref' name x1]

pushEqualsToEnd :: [Condition'] -> [Condition'] -> [Condition']
pushEqualsToEnd [] stored = stored
pushEqualsToEnd (Equals' x1 x2:xs) stored = pushEqualsToEnd xs (stored ++ [Equals' x1 x2])
pushEqualsToEnd (NotEquals' x1 x2:xs) stored = pushEqualsToEnd xs (stored ++ [NotEquals' x1 x2])
pushEqualsToEnd (Exists' x1 x2:xs)  stored = stored ++ Exists' x1 (pushEqualsToEnd x2 []):pushEqualsToEnd xs []
pushEqualsToEnd (Ref' name x1:xs)  stored = Ref' name x1:pushEqualsToEnd xs stored

data Condition' = Exists' [Var] [Condition'] | Equals' Var Var | NotEquals' Var Var |  Ref' String [Var] deriving (Show, Read)
