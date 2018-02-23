{
module Grammar where
import Tokens
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    IMPORT { TokenImport _  }
    TAKE { TokenTake _  }
    WHERE { TokenWhere _  }
    NOT {TokenNot _ }
    int { TokenInt _ $$ }
    string { TokenString _ $$ }
    '€' { TokenExists _  }
    '.' { TokenDot _  }
    '^' { TokenConjoin _  }
    V { TokenDisjunction _ }
    '=' { TokenEq _  }
    '[' { TokenStartList _  }
    ']' { TokenEndList _  }
    ',' { TokenComma _  }
    '|' { TokenEndPipe _  }
    '(' {TokenOpenBracket _ }
    ')' {TokenCloseBracket _ }



%right WHERE
%left '€' '='
%left '^' V
%right '.'
%right '[' '('
%left ']' ')'
%right IMPORT TAKE NOT
%%



Program : IMPORT string  '.' string '|'               { File ($2 ++ "." ++ $4) }
        | TAKE '[' List ']' WHERE Condition '|'             { Take $3 $6 }

Condition : '€' '[' List  ']' '.' Condition    { Exists $3 $6 }
          | Var '=' Var                        { Equals $1 $3}
          | string '[' List ']'                { Ref $1 $3 }
          | Condition '^' Condition            { Conjoin $1 $3 }
          | Condition V Condition              { Disjunction $1 $3 }
          |'(' Condition ')'                   { $2}
          | NOT Condition                      { Not $2}

List :: { [Var] }
List : Var                               { [$1] }
     | Var ',' List                      { $1 : $3 }
     | {- empty -}                        { [] }

Var : string                            { Var $1 }
    | int                                { Int $1 }

{
parseError :: [Token] -> a
parseError _ =  error "Nothing"

data Program = File String | Take [Var] Condition deriving Show
data Condition = Exists [Var] Condition |  Conjoin Condition Condition |  Disjunction Condition Condition | Equals Var Var | Ref String [Var] | Not Condition deriving Show
data Var = Var String | Int Int deriving (Show)

instance Eq Var where
  x == y = m x y
m :: Var -> Var -> Bool
m (Var x) (Var y) = x == y
m (Int a) (Int b) = a == b
m _ _ = False

}
