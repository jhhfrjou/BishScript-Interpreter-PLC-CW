{
module Tokens where
}

%wrapper "posn"
$digit = 0-9
-- digits
$alpha = [a-zA-Z]
-- alphabetic characters

tokens :-
$white+       ;
  "--".*      ;
  TAKE       { \pos s -> MkToken pos TokenTake }
  WHERE      { \pos s -> MkToken pos TokenWhere }
  $digit+    { \pos s -> MkToken pos (TokenInt (read s)) }
  \=         { \pos s -> MkToken pos  TokenEq }
  \+         { \pos s -> MkToken pos TokenPlus }
  \-         { \pos s -> MkToken pos TokenMinus }
  \*         { \pos s -> MkToken pos TokenTimes }
  \/         { \pos s -> MkToken pos TokenDiv }
  \(         { \pos s -> MkToken pos  TokenLParen }
  \)         { \pos s -> MkToken pos  TokenRParen }
  \^         { \pos s -> MkToken pos  TokenExpo }
  \€         { \pos s -> MkToken pos TokenExists}
  \[         { \pos s -> MkToken pos TokenStartList}
  \]         { \pos s -> MkToken pos TokenEndList}
  \,         { \pos s -> MkToken pos TokenListSeperations}
  \|         { \pos s -> MkToken pos TokenEndQuery}
  \:         { \pos s -> MkToken pos TokenConjuction}
  $alpha [$alpha $digit \_ \’]*   { \pos s -> MkToken pos (TokenVar s)}

{
data Token = MkToken AlexPosn TokenThing deriving (Show, Eq)
-- Each action has type :: String -> Token
-- The token type:
data TokenThing =
  TokenTake        |
  TokenWhere       |
  TokenInt Int     |
  TokenVar String  |
  TokenEq          |
  TokenPlus        |
  TokenMinus       |
  TokenTimes       |
  TokenDiv         |
  TokenLParen      |
  TokenRParen      |
  TokenExpo        |
  TokenExists      |
  TokenStartList   |
  TokenEndList     |
  TokenListSeperations |
  TokenEndQuery    |
  TokenConjuction

  deriving (Eq,Show)

}
