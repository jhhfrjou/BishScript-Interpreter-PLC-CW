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
    AS {TokenAs _ }
    int { TokenInt _ $$ }
    string { TokenString _ $$ }
    fileName { TokenFileName _ $$ }
    EXISTS { TokenExists _  }
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


%left EXISTS '=' '.'
%right '^' V
%right WHERE
%right '[' '('
%left ']' ')'
%nonassoc IMPORT TAKE NOT
%%



Program : IMPORT fileName  '.' string AS fileName '|'               { File ($2 ++ "." ++ $4) $6 }
                | IMPORT string  '.' string AS fileName '|'                    { File ($2 ++ "." ++ $4) $6 }
                | IMPORT fileName  '.' string '|'                                   { File ($2 ++ "." ++ $4) $2 }
                | IMPORT string  '.' string '|'                                          { File ($2 ++ "." ++ $4) $2 }
                | TAKE '[' List ']' WHERE Condition '|'             { Take $3 $6 }

Condition : EXISTS '[' List  ']' '.' Condition    { Exists $3 $6 }
          | Var '=' Var                        { Equals $1 $3}
          | fileName '[' List ']'              { Ref $1 $3 }
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

data Program = File String String | Take [Var] Condition deriving (Show, Read)
data Condition = Exists [Var] Condition |  Conjoin Condition Condition |  Disjunction Condition Condition | Equals Var Var | Ref String [Var] | Not Condition deriving (Show, Read)
data Var = Var String | Int Int deriving (Show, Read)
instance Eq Var where
  x == y = m x y
m :: Var -> Var -> Bool
m (Var x) (Var y) = x == y
m (Int a) (Int b) = a == b
m _ _ = False

}
