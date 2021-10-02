{
module Tokens_posn (Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = [0-9]
@id = [A-Za-z][A-Za-z’_]*


tokens :-

    $white+				 ;
    $digit+				 { tok (\p s -> INT p (read s)) }
    "PLUS"               { tok(\p s -> PLUS p "PLUS") }
    "MINUS"              { tok(\p s -> MINUS p "MINUS") }
    "TIMES"              { tok(\p s -> TIMES p "TIMES") }
    "NEGATE"             { tok(\p s -> NEGATE p "NEGATE") }
    "EQUALS"             { tok(\p s -> EQUALS p "EQUALS") }
    "LESSTHAN"           { tok(\p s -> LESSTHAN p "LESSTHAN") }
    "GREATERTHAN"        { tok(\p s -> GREATERTHAN p "GREATERTHAN") }
    "NOT"                { tok(\p s -> NOT p "NOT") }
    "AND"                { tok(\p s -> AND p "AND") }
    "OR"                 { tok(\p s -> OR p "OR") }
    "XOR"                { tok(\p s -> XOR p "XOR") }
    "IMPLIES"            { tok(\p s -> IMPLIES p "IMPLIES") }
    "TRUE"               { tok(\p s -> CONST p True)}
    "FALSE"              { tok(\p s -> CONST p False)}
    "("                  { tok(\p s -> LPAREN p s) }
    ")"                  { tok(\p s -> RPAREN p s) }
    let					 { tok (\p s -> LET p "let") }
    in					 { tok (\p s -> IN p "in") }
    if 				     { tok (\p s -> IF p "if") }
    then 				 { tok (\p s -> THEN p "then") }
    else 				 { tok (\p s -> ELSE p "else") }
    "="                  { tok (\p s -> ASSIGN p "=" )}
    @id                  { tok (\p s -> ID p s) }


{
-- Each right-hand side has type :: AlexPosn -> String -> Token

-- Some action helpers:
tok f p s = f p s

-- The token type:
data Token =
	INT AlexPosn Int |
    PLUS AlexPosn String |
    MINUS AlexPosn String |
    TIMES AlexPosn String |
    NEGATE AlexPosn String |
    EQUALS AlexPosn String |
    LESSTHAN AlexPosn String |
    GREATERTHAN AlexPosn String |
    NOT AlexPosn String |
    AND AlexPosn String |
    OR AlexPosn String |
    XOR AlexPosn String |
    IMPLIES AlexPosn String |
    CONST AlexPosn Bool |
    LPAREN AlexPosn String |
    RPAREN AlexPosn String |
    LET AlexPosn String |
    IN AlexPosn String |
    IF AlexPosn String |
    THEN AlexPosn String |
    ELSE AlexPosn String |
    ID AlexPosn String |
    ASSIGN AlexPosn String
	deriving (Eq,Show)

token_posn (INT p _) = p
token_posn (PLUS p _) = p
token_posn (MINUS p _) = p
token_posn (TIMES p _) = p
token_posn (NEGATE p _) = p
token_posn (EQUALS p _) = p
token_posn (LESSTHAN p _) = p
token_posn (GREATERTHAN p _) = p
token_posn (NOT p _) = p
token_posn (AND p _) = p
token_posn (OR p _) = p
token_posn (XOR p _) = p
token_posn (IMPLIES p _) = p
token_posn (LPAREN p _) = p
token_posn (RPAREN p _) = p
token_posn (LET p _) = p
token_posn (IN p _) = p
token_posn (IF p _) = p
token_posn (THEN p _) = p
token_posn (ELSE p _) = p
token_posn (ID p _) = p
token_posn (ASSIGN p _) = p

}