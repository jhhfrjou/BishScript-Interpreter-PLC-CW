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
    AS {TokenAs _ }
    string { TokenString _ $$ }
    fileName { TokenFileName _ $$ }
    EXISTS { TokenExists _  }
    '.' { TokenDot _  }
    '^' { TokenConjoin _  }
    '=' { TokenEq _  }
    '[' { TokenStartList _  }
    ']' { TokenEndList _  }
    ',' { TokenComma _  }
    '|' { TokenEndPipe _  }
    '(' {TokenOpenBracket _ }
    ')' {TokenCloseBracket _ }
    notEq {TokenNotEq _}


%left EXISTS '=' '.'
%left '^' V
%right WHERE
%right '[' '('
%left ']' ')'
%nonassoc IMPORT TAKE NOT
%%



Program : IMPORT fileName  '.' string AS fileName '|'               { File ($2 ++ "." ++ $4) $6 }
        | IMPORT string  '.' string AS fileName '|'                    { File ($2 ++ "." ++ $4) $6 }
        | IMPORT fileName  '.' string '|'                                   { File ($2 ++ "." ++ $4) $2 }
        | TAKE '[' List ']' WHERE Condition '|'             { Take $3 $6 }

Condition : EXISTS '[' List  ']' '.' Condition    { Exists $3 $6 }
          | Var '=' Var                        { Equals $1 $3}
          | Var notEq Var                       { NotEquals $1 $3}
          | fileName '[' List ']'              { Ref $1 $3 }
          | Condition '^' Condition            { Conjoin $1 $3 }
          |'(' Condition ')'                   { $2}

List :: { [Var] }
List : Var                               { [$1] }
     | Var ',' List                      { $1 : $3 }
     | {- empty -}                        { [] }

Var : string                            { Var $1 }

{
parseError :: [Token] -> a
parseError (TokenImport (AlexPn _ line col):xs) = error $ "Wasn't Expecting an import at: (line " ++ (show line) ++ " , column " ++ (show col) ++ ")"
parseError (TokenTake (AlexPn _ line col):xs) = error $ "Wasn't Expecting a take at: (line " ++ (show line) ++ " , column " ++ (show col) ++ ")"
parseError (TokenWhere (AlexPn _ line col):xs) = error $ "Wasn't Expecting a where at: (line " ++ (show line) ++ " , column " ++ (show col) ++ ")"
parseError (TokenAs (AlexPn _ line col):xs) = error $ "Wasn't Expecting an as at: (line " ++ (show line) ++ " , column " ++ (show col) ++ ")"
parseError (TokenString (AlexPn _ line col) s:xs) = error $ "Wasn't Expecting a string at: (line " ++ (show line) ++ " , column " ++ (show col) ++ ")"
parseError (TokenFileName (AlexPn _ line col) s:xs) = error $ "Wasn't Expecting a file reference at: (line " ++ (show line) ++ " , column " ++ (show col) ++ ")"
parseError (TokenExists (AlexPn _ line col):xs) = error $ "Wasn't Expecting a exists at: (line " ++ (show line) ++ " , column " ++ (show col) ++ ")"
parseError (TokenDot (AlexPn _ line col):xs) = error $ "Wasn't Expecting a . at: (line " ++ (show line) ++ " , column " ++ (show col) ++ ")"
parseError (TokenConjoin (AlexPn _ line col):xs) = error $ "Wasn't Expecting a conjoin (^) at: (line " ++ (show line) ++ " , column " ++ (show col) ++ ")"
parseError (TokenEq (AlexPn _ line col):xs) = error $ "Wasn't Expecting an equals (=) at: (line " ++ (show line) ++ " , column " ++ (show col) ++ ")"
parseError (TokenStartList (AlexPn _ line col):xs) = error $ "Wasn't Expecting a square bracket ([) at: (line " ++ (show line) ++ " , column " ++ (show col) ++ ")"
parseError (TokenEndList (AlexPn _ line col):xs) = error $ "Wasn't Expecting a square bracket (]) at: (line " ++ (show line) ++ " , column " ++ (show col) ++ ")"
parseError (TokenComma (AlexPn _ line col):xs) = error $ "Wasn't Expecting a comma (,) at: (line " ++ (show line) ++ " , column " ++ (show col) ++ ")"
parseError (TokenEndPipe (AlexPn _ line col):xs) = error $ "Wasn't Expecting a pipe (|) at: (line " ++ (show line) ++ " , column " ++ (show col) ++ ")"
parseError (TokenOpenBracket (AlexPn _ line col):xs) = error $ "Wasn't Expecting an open bracket (  at: (line " ++ (show line) ++ " , column " ++ (show col) ++ ")"
parseError (TokenCloseBracket (AlexPn _ line col):xs) = error $ "Wasn't Expecting a close bracket )  at: (line " ++ (show line) ++ " , column " ++ (show col) ++ ")"
parseError (TokenNotEq (AlexPn _ line col):xs) = error $ "Wasn't Expecting a not equals (!=) at: (line " ++ (show line) ++ " , column " ++ (show col) ++ ")"



data Program = File String String | Take [Var] Condition deriving (Show, Read)
data Condition = Exists [Var] Condition | Equals Var Var | NotEquals Var Var |Ref String [Var] | Conjoin Condition Condition deriving (Show, Read)
data Var = Var String deriving (Show, Read)
instance Eq Var where
  (Var x) == (Var y) = x == y

instance Ord Var where
  (Var x) `compare` (Var y) = x `compare` y

}
