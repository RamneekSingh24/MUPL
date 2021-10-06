-- TODO add proper errors

import Data.Map
import Lexer


type Num = Int
type Tokens = [Token]
type Bindings = (Map String Expr)


dummyPos:: AlexPosn 
dummyPos = AlexPn 0 1 1

nextToken:: Tokens -> Token
nextToken [] = error "nextToken called on empty list"
nextToken toks = head toks

data BinOpr = Plus | Minus | Times | Equals | LessThan | GreaterThan | And | Or | Xor | Implies deriving (Eq,Show)
data UniOpr = Negate | Not deriving (Eq,Show)
data Opr = BinOp BinOpr | UniOp UniOpr deriving (Eq,Show)

data OprWithPos = PosBin AlexPosn BinOpr | PosUni AlexPosn UniOpr deriving (Eq,Show)

data Expr = Nil |
            Num  Int | 
            Const  Bool | 
            Id  String |
            Bi  BinOpr Expr Expr |
            Uni  UniOpr Expr |
            IfThenElse  Expr Expr Expr |
            LetIn Bindings Expr
            deriving (Eq,Show)

data ExprWithPos = Create AlexPosn Expr deriving (Eq,Show)
data Prev = EXPRESSION | BIOPR | UNIOPR | NONE

getExp:: ExprWithPos -> Expr
getExp exp = 
    case exp of
        Create p e -> e

getPos:: ExprWithPos -> AlexPosn
getPos exp = 
    case exp of
        Create p e -> p

precedence:: OprWithPos -> Int
precedence op =
    case op of
        PosUni _ _ -> 0
        PosBin _  Times -> 1
        PosBin _  Plus -> 2
        PosBin _  Minus -> 2
        PosBin _  LessThan -> 3
        PosBin _  GreaterThan -> 3
        PosBin _  Equals -> 4
        PosBin _  And -> 5
        PosBin _  Or -> 5
        PosBin _  Xor -> 5
        PosBin _  Implies -> 6

parse:: String -> Expr
parse_general:: Tokens -> (ExprWithPos, Tokens)
parse_LetIn:: Tokens -> (ExprWithPos, Tokens)
parse_IfThenElse:: Tokens -> (ExprWithPos, Tokens)
parse_paren:: Tokens -> (ExprWithPos, Tokens)

parse_normal_expr:: Prev -> [OprWithPos] -> [ExprWithPos] -> Tokens -> (ExprWithPos, Tokens)
addOpr:: OprWithPos -> [OprWithPos] -> [ExprWithPos] -> ([OprWithPos], [ExprWithPos])
makeTree:: [OprWithPos] -> [ExprWithPos] -> ExprWithPos

isBiOperator:: Token -> Bool
isUniOperator:: Token -> Bool



checkTok:: Prev -> Tokens -> Bool
checkTok BIOPR [] = False
checkTok UNIOPR [] = False
checkTok EXPRESSION [] = True
checkTok prev toks = 
    case prev of
        NONE -> True
        BIOPR -> if not (isBiOperator (head toks)) then True 
                 else error ((show (token_posn (head toks))) ++ " expected expression after operator")
        UNIOPR -> if not (isBiOperator (head toks)) then True
                  else error ((show (token_posn (head toks))) ++ " expected expression after operator") 
        EXPRESSION -> let 
                          tok = (head toks) 
                      in 
                          case tok of
                              INT _ _ -> False
                              CONST _ _ -> False
                              ID _ _ -> False
                              LET _ _ -> False
                              IF _ _ -> False
                              NOT _ _ -> False
                              NEGATE _ _ -> False
                              _ -> True
                              




isBiOperator tok = 
    case tok of
        PLUS _ _ -> True
        MINUS p _ -> True
        TIMES p _ -> True
        EQUALS p _ -> True
        LESSTHAN p _ -> True
        GREATERTHAN p _ -> True
        AND p _ -> True
        OR p _ -> True
        XOR p _ -> True
        IMPLIES p _ -> True
        _ -> False

isUniOperator tok =
    case tok of
    NEGATE _ _ -> True
    NOT _ _ -> True
    _ -> False


parse prog = 
    getExp (fst (parse_general (alexScanTokens prog)))

parse_general [] = ((Create (AlexPn 0 0 0) Nil), [])
parse_general toks = 
    let 
        tok = nextToken toks
    in
        case tok of 
            LET _ _ -> parse_LetIn toks
            IF _ _  -> parse_IfThenElse toks
            LPAREN _ _ -> parse_normal_expr NONE [] [] toks
            INT p _ -> parse_normal_expr NONE [] [] toks
            PLUS p _ -> error ((show p) ++ " Expected Expression before PLUS") 
            MINUS p _ -> error ((show p) ++ " Expected Expression before MINUS") 
            TIMES p _ -> error ((show p) ++ " Expected Expression before TIMES") 
            NEGATE p _ -> parse_normal_expr NONE [] [] toks
            EQUALS p _ -> error ((show p) ++ " Expected Expression before EQUALS") 
            LESSTHAN p _ -> error ((show p) ++ " Expected Expression before EQUALS") 
            GREATERTHAN p _ -> error (show p)
            NOT p _ -> parse_normal_expr NONE [] [] toks
            AND p _ -> error ((show p) ++ "Expected Expression before AND") 
            OR p _ -> error ((show p) ++ "Expected Expression before OR") 
            XOR p _ -> error ((show p) ++ "Expected Expression before XOR") 
            IMPLIES p _ -> error ((show p) ++ "Expected Expression before IMPLIES") 
            CONST p _ -> parse_normal_expr NONE [] [] toks
            RPAREN p _ -> error ((show p) ++ "Expected LPAREN before RPAREN") 
            IN p _ -> error ((show p) ++ "Expected let before in") 
            THEN p _ -> error ((show p) ++ "Expected if before then") 
            ELSE p _ -> error ((show p) ++ "Expected if then before else") 
            ID p _ -> parse_normal_expr NONE [] [] toks
            ASSIGN p _ -> error ((show p) ++ "Expected let id before =")
            FI p _ -> error ((show p) ++ "Expected if then else before fi")
            END p _ -> error ((show p) ++ "Expected let in before end")
            EOF p _ -> (Create p Nil, [])



parse_paren [] = error "bug"
parse_paren toks = 
    if length toks < 3 then error (show (token_posn (head toks)))
    else
        let 
            tok = nextToken toks
        in
            case tok of 
                LPAREN p _ -> 
                    let
                        (expr, toks_left) = parse_general (tail toks)
                    in
                        if length toks_left < 1 then error ((show p) ++ "No RPAREN found after LPAREN")
                        else
                            let 
                                end_tok = nextToken toks_left
                            in
                                case end_tok of
                                    RPAREN _ _ -> (expr, (tail toks_left))
                                    _ -> error ((show p) ++ "No RPAREN found after LPAREN")
                _ -> error (show (token_posn tok) ++ "No RPAREN found after LPAREN") 


-- [let_expr, var_name, assign, expr1, in, expr2 end]
parse_LetIn toks = 
    case toks of
        (LET plet _):((ID _ var_name):((ASSIGN pasgn _ ):toks_left1))->
            if length toks_left1 < 3 then error ((show pasgn) ++ " Invalid Let in expression")
            else 
                let 
                    (expr_assign, toks_left2) = parse_general toks_left1
                in
                    if length toks_left2 < 1 then error ((show (getPos expr_assign)) ++ " Invalid Let in expression")
                    else if length toks_left2 < 2 then error ((show (token_posn (head toks_left2))) ++ " Invalid Let in expression")
                    else case toks_left2 of 
                        (IN p _):toks_left3 -> 
                            let 
                                (expr_in, toks_left4) = parse_general toks_left3
                                binding = fromList [(var_name, (getExp expr_assign))]
                            in
                                if ((length toks_left4) > 0) && (head toks_left4) == (END (token_posn (head toks_left4)) "END")
                                then   (Create plet (LetIn binding (getExp expr_in)), (tail toks_left4))
                                else error ((show p) ++ " Expected end after in") -- expected end after in
                        _ -> error (show (token_posn (head toks_left2)))
        [] -> error "bug in my code"
        _ -> error (show (token_posn (head toks)))


-- [if, expr1, then, exrp2, else expr3 fi]
parse_IfThenElse [] = error "bug in my code"
parse_IfThenElse toks = 
    if length toks <  6 then error (show (token_posn (head toks)))
    else case toks of
        (IF pif _):(tok1:toks_left1) ->
            let
                (expr_if, toks_left2) = parse_general (tok1:toks_left1)
            in
                case toks_left2 of
                    (THEN pthen _):(tok3:toks_left3) -> 
                        let
                            (expr_then, toks_left4) = parse_general (tok3:toks_left3)
                        in
                            case toks_left4 of
                                (ELSE pelse _):(tok5:toks_left5) -> 
                                    let 
                                        (expr_else, toks_left6) = parse_general (tok5:toks_left5)
                                        expr_ifthenelse = 
                                            Create pif (IfThenElse (getExp expr_if) (getExp expr_then) (getExp expr_else))
                                    in
                                        if (length toks_left6) > 0 && (head toks_left6) == (FI (token_posn (head toks_left6)) "FI")
                                        then (expr_ifthenelse, (tail toks_left6))
                                        else error ((show pelse) ++ "expected fi after else")-- expected fi after else 
                                _ -> error ((show pthen)  ++ " invalid if then else expression")
                    _ -> error ((show pif) ++ " invalid if then else expression")
        _ -> error ((show (token_posn (head toks))) ++ " invalid if then else expression")

    

addOpr op oprs exprs = 
    case op of
        PosUni p _ -> (op:oprs, exprs)
        PosBin p _ -> 
            if (length oprs == 0) then (op:oprs, exprs)
            else
                let
                    top_opr = head oprs
                    p1 = precedence top_opr
                    p2 = precedence op
                in
                    if p1 > p2 then (op:oprs, exprs)
                    else
                        case top_opr of
                            PosUni p opr ->
                                if (length exprs) < 1 then error ((show p) ++ " expected expression to match with uniary operator")
                                else 
                                    let 
                                        expr = Create p (Uni opr (getExp (head exprs)))
                                    in
                                        addOpr op (tail oprs) (expr:(tail exprs))
                            PosBin p opr -> 
                                if (length exprs) < 2 then error ((show p) ++ " expected 2 expressions to match with binary operator")
                                else 
                                    let 
                                        expr1 = getExp (head exprs)
                                        expr2 = getExp (exprs!!1)
                                        expr = Create p (Bi opr expr2 expr1)
                                    in
                                        addOpr op (tail oprs) (expr:(tail (tail exprs)))

                                

makeTree [] exprs = case exprs of 
                        [] -> error " at unknown AlexPn"
                        [expr] -> expr
                        _ -> error ((show (getPos (last exprs))) ++ " incomplete expression")

makeTree oprs exprs =
    let
        opr = head oprs
    in
        case opr of
            PosUni p op -> if (length exprs) < 1 then error ((show p) ++ " expected expression to match with uniary operator")
                           else 
                               let
                                   expr = Create p (Uni op (getExp (head exprs)))
                                in
                                    makeTree (tail oprs) (expr:(tail exprs))
            PosBin p op -> if (length exprs) < 2 then error ((show p) ++ " expected expression to match with binary operator")
                           else
                               let
                                   expr1 = getExp (head exprs)
                                   expr2 = getExp (exprs!!1)
                                   expr = Create p (Bi op expr2 expr1)
                                in
                                    makeTree (tail oprs) (expr:(tail (tail exprs)))


parse_normal_expr prev oprs exprs [] = (makeTree oprs exprs, [])
parse_normal_expr prev oprs exprs toks = 
    let
        tok = nextToken toks
    in
        if (not (checkTok prev toks)) then error ((show (token_posn (head toks))) ++ " expected operator after expression")
        else
            case tok of
                PLUS p s -> 
                    let 
                        opr = PosBin p Plus
                        (new_oprs, new_exprs) = addOpr opr oprs exprs
                    in
                        parse_normal_expr BIOPR new_oprs new_exprs (tail toks)
                MINUS p s -> 
                    let 
                        opr = PosBin p Minus
                        (new_oprs, new_exprs) = addOpr opr oprs exprs
                    in
                        parse_normal_expr BIOPR new_oprs new_exprs (tail toks)
                TIMES p s -> 
                    let 
                        opr = PosBin p Times
                        (new_oprs, new_exprs) = addOpr opr oprs exprs
                    in
                        parse_normal_expr BIOPR new_oprs new_exprs (tail toks)
                NEGATE p s -> 
                    let 
                        opr = PosUni p Negate
                        (new_oprs, new_exprs) = addOpr opr oprs exprs
                    in
                        parse_normal_expr UNIOPR new_oprs new_exprs (tail toks)
                EQUALS p s -> 
                    let 
                        opr = PosBin p Equals
                        (new_oprs, new_exprs) = addOpr opr oprs exprs
                    in
                        parse_normal_expr BIOPR new_oprs new_exprs (tail toks)
                LESSTHAN p s -> 
                    let 
                        opr = PosBin p LessThan
                        (new_oprs, new_exprs) = addOpr opr oprs exprs
                    in
                        parse_normal_expr BIOPR new_oprs new_exprs (tail toks)
                GREATERTHAN p s -> 
                    let 
                        opr = PosBin p GreaterThan
                        (new_oprs, new_exprs) = addOpr opr oprs exprs
                    in
                        parse_normal_expr BIOPR new_oprs new_exprs (tail toks)
                NOT p s -> 
                    let 
                        opr = PosUni p Not
                        (new_oprs, new_exprs) = addOpr opr oprs exprs
                    in
                        parse_normal_expr UNIOPR new_oprs new_exprs (tail toks)
                AND p s -> 
                    let 
                        opr = PosBin p And
                        (new_oprs, new_exprs) = addOpr opr oprs exprs
                    in
                        parse_normal_expr BIOPR new_oprs new_exprs (tail toks)
                OR p s -> 
                    let 
                        opr = PosBin p Or
                        (new_oprs, new_exprs) = addOpr opr oprs exprs
                    in
                        parse_normal_expr BIOPR new_oprs new_exprs (tail toks)
                XOR p s -> 
                    let 
                        opr = PosBin p Xor
                        (new_oprs, new_exprs) = addOpr opr oprs exprs
                    in
                        parse_normal_expr BIOPR new_oprs new_exprs (tail toks)
                IMPLIES p s -> 
                    let 
                        opr = PosBin p Implies
                        (new_oprs, new_exprs) = addOpr opr oprs exprs
                    in
                        parse_normal_expr BIOPR new_oprs new_exprs (tail toks)

                INT p x -> parse_normal_expr EXPRESSION oprs ((Create p (Num x)):exprs) (tail toks)
                CONST p x -> parse_normal_expr EXPRESSION oprs ((Create p (Const x)):exprs) (tail toks)
                ID p s -> parse_normal_expr EXPRESSION oprs ((Create p (Id s)):exprs) (tail toks)

                LPAREN p s -> 
                    let
                        (paren_expr, new_toks) = parse_paren toks
                    in
                        parse_normal_expr EXPRESSION oprs (paren_expr:exprs) (new_toks)
                LET p s -> 
                    let
                        (let_in_expr, new_toks) = parse_LetIn toks
                    in
                        parse_normal_expr EXPRESSION oprs (let_in_expr:exprs) (new_toks)
                IF p s -> 
                    let
                        (if_then_else_expr, new_toks) = parse_IfThenElse toks
                    in
                        parse_normal_expr EXPRESSION oprs (if_then_else_expr:exprs) (new_toks)

                THEN p s -> (makeTree oprs exprs, toks)
                ELSE p s -> (makeTree oprs exprs, toks)
                RPAREN p s -> (makeTree oprs exprs, toks)
                IN p s -> (makeTree oprs exprs, toks)
                FI p s -> (makeTree oprs exprs, toks)
                END p s -> (makeTree oprs exprs, toks)
                EOF p s -> (makeTree oprs exprs, toks)

                ASSIGN p s -> error ((show p) ++ "expected let id before =")
            



-- make tree ops exprs = infix evaluation with parenthesis

-- parse_normal_expr toks = 
--     let
--         get_op_expr ops exprs toks= 
--             if next token = then/else/in/rapen/EOF
--                 then (makeTree ops exprs, toks)
--             else if next token = opr 
--                 other_op = ops.top
--                 while precedence opr >= other_op
--                 if other_op = ops.top = uni
--                     then ops.pop, ops.push opr
--                     , exp1 = exprs.pop, exp2 = exprs.pop,
--                     , exprs.push exp1 op exp2
--                     return get_op_expr ops exprs toks
                    
--             else if next token = lparen
            
--             else if next token == if
            
--             else if next token == let
            
--             else error next token 

            



{-
Parse Normal_expr -> Halt at THEN/ELSE/IN/RPAREN
Parse paren_expr -> Halt at RPAREN
Parse bindings -> Halt at In
Parse IfThenElse -> Halt at ASSIGN/THEN/ELSE
Parse LetIn -> Start Parse Bindings , ParseIn->Halt at EOF/ELSE/THEN

Prelude> 5 + let x = let y = 3 in y + 3 in if x > 5 then 6 else 7
11
Prelude> 5 + let x = let y = 2 in y + 3 in if x > 5 then 6 else 7
12
-}









         




    
-- [LPAREN (AlexPn 0 1 1) "(",ID (AlexPn 1 1 2) "xyz",IMPLIES (AlexPn 5 1 6) "IMPLIES",
-- CONST (AlexPn 13 1 14) "FALSE",RPAREN (AlexPn 18 1 19) ")",OR (AlexPn 20 1 21) "OR",
-- CONST (AlexPn 23 1 24) "TRUE",AND (AlexPn 28 1 29) "AND",IF (AlexPn 32 1 33) 
-- "if",ID (AlexPn 35 1 36) "A",THEN (AlexPn 37 1 38) "then",ID (AlexPn 42 1 43) "b", 
-- ELSE (AlexPn 44 1 45) "else",ID (AlexPn 49 1 50) "c"]




-- 5 PLUS 10 XOR (5 MINUS 10) TIMES 5 MINUS 10 PLUS (2 TIMES 5) LESSTHAN GREATERTHAN 

-- 5 MINUS 10 PLUS 5 TIMES 5

{-

STEPS:

parse E  curr_toks =
    if E.next_token()  == 'LPAREN'
        then parse E.nextokens()
    if E.next_token() == 'RPAREN'
        then convert(curr_toks)
    
        

-}






    


