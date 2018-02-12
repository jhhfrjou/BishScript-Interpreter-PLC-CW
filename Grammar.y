{
module Grammar where
import Tokens
}

%name parseCalc
%tokentype { Token }
%error { parseError }
%token
    TAKE { MkToken _ TokenTake }
    WHERE  { MkToken _ TokenWhere }
    int { MkToken _ (TokenInt $$) }
    var {MkToken _ (TokenVar $$) }
    '=' { MkToken _ TokenEq }
    '+' { MkToken _ TokenPlus }
    '-' { MkToken _ TokenMinus }
    '*' {MkToken _  TokenTimes }
    '/' {MkToken _  TokenDiv }
    '(' { MkToken _ TokenLParen }
    ')' {MkToken _  TokenRParen }
    '^' { MkToken _ TokenExpo }

%right WHERE
%left '+' '-' '€'
%left '*' '/'
%left '^'
%left NEG
%%

Exp : TAKE var '=' Exp WHERE Exp { Let $2 $4 $6 }
    | Exp '+' Exp            { Plus $1 $3 }
    | Exp '-' Exp            { Minus $1 $3 }
    | Exp '*' Exp            { Times $1 $3 }
    | Exp '/' Exp            { Div $1 $3 }
    | Exp '^' Exp            { Expo $1 $3}
    | '(' Exp ')'            { $2 }
    | '-' Exp %prec NEG      { Negate $2 }
    | int                    { Int $1 }
    | var                    { Var $1 }

{

getPos :: Token -> (Int, Int)
getPos ( MkToken (AlexPn a b c) _ ) = (b,c)

parseError :: [Token] -> a
parseError _ = error "Parse error"
data Exp = Let String Exp Exp
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Negate Exp
         | Expo Exp Exp
         | Int Int
         | Var String
         deriving Show
}
