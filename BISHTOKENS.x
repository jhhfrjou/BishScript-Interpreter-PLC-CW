{
module BISHTOKENS where
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
  $digit+    { \pos s -> MkToken pos (TokenInt (read s)) }
  \^         { \pos s -> MkToken pos  TokenConjoin }
  \€         { \pos s -> MkToken pos TokenExists}
  \[         { \pos s -> MkToken pos TokenStartList}
  \]         { \pos s -> MkToken pos TokenEndList}
  \,         { \pos s -> MkToken pos TokenComma}
  \|         { \pos s -> MkToken pos TokenEndPipe}
  \=         { \pos s -> MkToken pos TokenEq}
  \.         { \pos s -> MkToken pos TokenDot}

  $alpha [$alpha $digit \_ \’]*   { \pos s -> MkToken pos (TokenString s)}
{
data Token = MkToken AlexPosn TokenThing deriving (Show, Eq)
-- Each action has type :: String -> Token
-- The token type:
data TokenThing =
  TokenTAKE         |
  TokenWHERE        |
  TokenIMPORT       |
  TokenInt Int      |
  TokenString String|
  TokenEq           |
  TokenConjoin      |
  TokenExists       |
  TokenStartList    |
  TokenEndList      |
  TokenComma        |
  TokenEndPipe      |
  TokenDot
  deriving (Eq,Show)

}