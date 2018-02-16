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



%right WHERE
%left '€' '='
%left '^'
%right '['
%left ']'
%right '.'
%right IMPORT
%right TAKE
%%



Program : IMPORT string  '.' string '|'               { File ($2 ++ "." ++ $4) }
        | TAKE '[' List ']' WHERE Condition '|'             { Take $3 $6 }

Condition : '€' '[' List  ']' '.' Exp    { Exists $3 $6 }
          | Exp                          { Exp $1}

Reference : string '[' List ']'          {FRef $1 $3 }

Exp : Reference                           { ColRef $1 }
    | Exp '^' Exp                         { Conjoin $1 $3 }
    | Exp '=' Exp                         { Equals $1 $3 }
    | '[' List ']'                        { List $2 }
    | Data                                { Info $1}

List :: { [Data] }
List : Data                               { [$1] }
     | Data ',' List                      { $1 : $3 }
     | {- empty -}                        { [] }

Data : string                             { String $1 }
     | int                                { Int $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Program = File String | Take [Data] Condition deriving Show
data Condition = Exists [Data] Exp | Exp Exp deriving Show
data Reference = FRef String [Data] deriving Show
data Exp = ColRef Reference | Conjoin Exp Exp | Equals Exp Exp | List [Data] | Info Data deriving Show
data Data = String String | Int Int deriving Show

}
