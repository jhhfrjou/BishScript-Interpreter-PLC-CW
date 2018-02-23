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
  "//".*       ;
  take       { \pos s -> MkToken pos TokenTAKE }
  where      { \pos s -> MkToken pos TokenWHERE }
  import     { \pos s -> MkToken pos TokenIMPORT }
  not        { \pos s -> MkToken pos TokenNOT }
  "\/"      { \pos s -> MkToken pos TokenDisjunction }
  $digit+    { \pos s -> MkToken pos (TokenInt (read s)) }
  \^         { \pos s -> MkToken pos  TokenConjoin }
  \€         { \pos s -> MkToken pos TokenExists}
  \[         { \pos s -> MkToken pos TokenStartList}
  \]         { \pos s -> MkToken pos TokenEndList}
  \,         { \pos s -> MkToken pos TokenComma}
  \|         { \pos s -> MkToken pos TokenEndPipe}
  \=         { \pos s -> MkToken pos TokenEq}
  \.         { \pos s -> MkToken pos TokenDot}
  \(         { \pos s -> MkToken pos TokenOpenBracket}
  \)        { \pos s -> MkToken pos TokenCloseBracket}

  $alpha [$alpha $digit \_ \’]*   { \pos s -> MkToken pos (TokenString s)}
{
data Token = MkToken AlexPosn TokenThing deriving (Show, Eq)
-- Each action has type :: String -> Token
-- The token type:
data TokenThing =
  TokenTAKE         |
  TokenWHERE        |
  TokenIMPORT       |
  TokenNOT          |
  TokenDisjunction  |
  TokenInt Int      |
  TokenString String|
  TokenEq           |
  TokenConjoin      |
  TokenExists       |
  TokenStartList    |
  TokenEndList      |
  TokenComma        |
  TokenEndPipe      |
  TokenDot |
  TokenCloseBracket |
  TokenOpenBracket

  deriving (Eq,Show)

}
