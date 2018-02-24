{
module Tokens where
}

%wrapper "posn"
$digit = 0-9
-- digits
$alpha = [a-zA-Z]
$fileName = [A-Z]
-- alphabetic characters

tokens :-
$white+       ;
  "//".*       ;
  take       { \pos s -> TokenTake pos  }
  where      { \pos s -> TokenWhere pos }
  import     { \pos s -> TokenImport pos  }
  not        { \pos s -> TokenNot pos  }
  "\/"      { \pos s -> TokenDisjunction pos  }
  $digit+    { \pos s -> TokenInt pos (read s) }
  \^         { \pos s -> TokenConjoin pos   }
  \€         { \pos s -> TokenExists pos }
  \[         { \pos s -> TokenStartList pos }
  \]         { \pos s -> TokenEndList pos }
  \,         { \pos s -> TokenComma pos }
  \|         { \pos s -> TokenEndPipe pos }
  \=         { \pos s -> TokenEq pos }
  \.         { \pos s -> TokenDot pos }
  \(         { \pos s -> TokenOpenBracket pos }
  \)        { \pos s -> TokenCloseBracket pos }

  $alpha [$alpha $digit \_ \’]*   { \pos s -> TokenString pos s}

{

data Token =
  TokenTake AlexPosn   |
  TokenWhere AlexPosn       |
  TokenImport AlexPosn      |
  TokenNot AlexPosn         |
  TokenDisjunction AlexPosn  |
  TokenInt AlexPosn Int   |
  TokenString AlexPosn String |
  TokenEq AlexPosn          |
  TokenConjoin AlexPosn     |
  TokenExists AlexPosn      |
  TokenStartList AlexPosn   |
  TokenEndList AlexPosn     |
  TokenComma AlexPosn        |
  TokenEndPipe AlexPosn      |
  TokenDot AlexPosn |
  TokenCloseBracket AlexPosn|
  TokenOpenBracket AlexPosn
  deriving (Eq,Show)

}
