{
module BISH where
import BISHTOKENS
}

%name parseBish
%tokentype { Token }
%error { parseError }

%token
    IMPORT { MkToken _ TokenIMPORT }
    TAKE { MkToken _ TokenTAKE }
    WHERE { MkToken _ TokenWHERE }
    int { MkToken _ (TokenInt $$) }
    string { MkToken _ (TokenString $$) }
    '€' { MkToken _ TokenExists }
    '.' { MkToken _ TokenDot }
    '^' { MkToken _ TokenConjoin }
    '=' { MkToken _ TokenEq }
    '[' { MkToken _ TokenStartList }
    ']' { MkToken _ TokenEndList }
    ',' { MkToken _ TokenComma }
    '|' { MkToken _ TokenEndPipe }
    '(' {MkToken _ TokenOpenBracket}
    ')' {MkToken _ TokenCloseBracket}


%right WHERE
%left '€' '='
%left '^'
%right '['
%left ']'
%right '.'
%right '['
%left ']'
%right '('
%left ')'
%right IMPORT
%right TAKE
%%



Program : IMPORT string  '.' string '|'               { File ($2 ++ "." ++ $4) }
                | TAKE '[' List ']' WHERE Condition '|'             { Take $3 $6 }

Condition : '€' '[' List  ']' '.' Condition    { Exists $3 $6 }
                  | Var '=' Var                         { Equals $1 $3}
                  | string '[' List ']'                    { Ref $1 $3 }
                  | Condition '^' Condition  { Conjoin $1 $3 }
                  |'(' Condition ')'                  { Brackets $2}

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
data Condition = Exists [Var] Condition |  Conjoin Condition Condition | Equals Var Var | Ref String [Var] | Brackets Condition deriving Show
data Var = Var String | Int Int deriving Show

}
