import Grammar
import Tokens
calc :: Exp -> Int
calc (Minus (Int a) (Int b)) = a - b
calc (Plus (Int a) (Int b)) = a + b
calc (Times (Int a) (Int b)) = a * b
calc (Div (Int a) (Int b)) = a `div` b
calc (Negate (Int a)) = 0 - a
calc (Expo (Int a) (Int b)) = a ^ b
calc _ = 5

magic :: String -> Int
magic x  = calc $ parseCalc $ alexScanTokens x
