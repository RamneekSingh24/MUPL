{
module Lexer where
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+			                          ;
  ";"     				                      { \p s -> EOF s }
  "("                                   { \p s -> LPAREN s }
  ")"                                   { \p s -> RPAREN s }
  -- int ops
  "+"                                   { \p s -> PLUS s }
  "-"                                   { \p s -> MINUS s }
  "*"                                   { \p s -> TIMES s }
  "~"                                   { \p s -> NEGATE s }
  "="                                   { \p s -> EQUALS s }
  "<"                                   { \p s -> LESSTHAN s }
  ">"                                   { \p s -> GREATERTHAN s }
  -- bool ops
  "!"                                   { \p s -> NOT s }
  "&&"                                  { \p s -> AND s }
  "||"                                  { \p s -> OR s }
  "^"                                   { \p s -> XOR s }
  --"=>"                                  { \p s -> IMPLIES s }
  -- ite
  "if"                                  { \p s -> IF s }
  "then"                                { \p s -> THEN s }
  "else"                                { \p s -> ELSE s }
  "fi"                                  { \p s -> FI s }
  -- let
  "let"                                 { \p s -> LET s }
  ":="                                  { \p s -> ASSIGN s }
  "in"                                  { \p s -> IN s }
  "end"                                 { \p s -> END s }
  -- atoms
  "false"                               { \p s -> FALSE s }
  "true"                                { \p s -> TRUE s }
  "=>"                                 { \p s -> MAP s }
  "->"                                  { \p s -> ARROW s }
  "fn"                                  { \p s -> LAMBDA s }
  "fun"                                 { \p s -> DEF s }
  "done"                                { \p s -> DONE s }
  "::"                                  { \p s -> OFTYPE s }
  "int"                                 { \p s -> Int_Type s}
  "bool"                                { \p s -> Bool_Type s}
  $alpha+                               { \p s -> ID s }
  $digit+                               { \p s -> CONST (read s) }


{
-- Each right-hand side has type :: AlexPosn -> String -> Token

-- The token type:

data Token =
             EOF String
           | LPAREN String
           | RPAREN String
           | PLUS String
           | MINUS String
           | TIMES String
           | NEGATE String
           | EQUALS String
           | LESSTHAN String
           | GREATERTHAN String
           | NOT String
           | AND String
           | OR String
           | XOR String
           | IMPLIES String
           | IF String
           | THEN String
           | ELSE String
           | FI String
           | LET String
           | ASSIGN String
           | IN String
           | END String
           | FALSE String
           | TRUE String
           | CONST Integer
           | ID String
           | MAP String
           | ARROW String
           | LAMBDA String
           | DEF String
           | DONE String
           | OFTYPE String
           | Bool_Type String
           | Int_Type String
        deriving (Eq,Show)

scanTokens str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError ((AlexPn p line column),c,_,_) -> error $ "Unknown token:" ++ (show line) ++ ":" ++ (show (column-1)) ++ ":" ++ [c] -- (column-1) because column refers to immediately next column
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'
}
