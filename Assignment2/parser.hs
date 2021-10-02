import Data.Map
import Stack
import Lexer


type Num = Int


type Tokens = [Token]

scanToken:: Tokens -> (Token, Tokens)
scanToken [] = error "nextToken called on empty list"
scanToken toks = (head toks, tail toks)

nextToken:: Tokens -> Token
nextToken [] = error "nextToken called on empty list"
nextToken toks = head toks

data BinOpr = Plus | Minus | Times | Equals | LessThan | GreaterThan | And | Or | Xor | Implies deriving (Eq,Show)
data UniOpr = Negate | Not deriving (Eq,Show)
data Opr = BinOp BinOpr | UniOp UniOpr deriving (Eq,Show)

data OprWithPos = PosBin AlexPosn BinOpr | PosUni AlexPosn UniOpr deriving (Eq,Show)

dummyPos:: AlexPosn 
dummyPos = AlexPn 0 1 1


data Bindings = Map String Expr deriving (Eq,Show)


data Expr = Nil |
            Num  Int | 
            Const  Bool | 
            Id  String |
            Bi  BinOpr Expr Expr |
            Uni  UniOpr Expr |
            IfThenElse  Opr Expr |
            LetIn Bindings Expr
            deriving (Eq,Show)

data ExprWithPos = Create AlexPosn Expr deriving (Eq,Show)

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

parse:: Tokens -> Expr

parse [] = Nil
parse toks = 
    let 
        (tok, next) = scanToken toks
    in
        case tok of
            LET p s -> undefined
            IN p s -> undefined
            IF p s -> undefined



parse_h:: [Opr] -> [Expr] -> Tokens -> (Expr, Tokens)
parse_h ops exprs toks = undefined




makeTree:: [OprWithPos] -> [ExprWithPos] -> ExprWithPos
makeTree [] exprs = case exprs of 
                        [] -> error "Unknown AlexPn"
                        [expr] -> expr
                        _ -> error (show (getPos (last exprs)))

makeTree oprs exprs =
    let
        opr = head oprs
    in
        case opr of
            PosUni p op -> if (length exprs) < 1 then error (show p) 
                           else 
                               let
                                   expr = Create p (Uni op (getExp (head exprs)))
                                in
                                    makeTree (tail oprs) (expr:(tail exprs))
            PosBin p op -> if (length exprs) < 2 then error (show p)
                           else
                               let
                                   expr1 = getExp (head exprs)
                                   expr2 = getExp (exprs!!1)
                                   expr = Create p (Bi op expr1 expr2)
                                in
                                    makeTree (tail oprs) (expr:(tail (tail exprs)))




parse_normal_expr:: [OprWithPos] -> [ExprWithPos] -> Tokens -> (ExprWithPos, Tokens)
parse_LetIn:: Tokens -> (ExprWithPos, Tokens)
parse_IfThenElse:: Tokens -> (ExprWithPos, Tokens)
parse_paren:: Tokens -> (ExprWithPos, Tokens)
parse_LetIn toks = undefined
parse_IfThenElse toks = undefined
parse_paren toks = undefined


addOpr:: OprWithPos -> [OprWithPos] -> [ExprWithPos] -> ([OprWithPos], [ExprWithPos])
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
                                if (length exprs) < 1 then error (show p)
                                else 
                                    let 
                                        expr = Create p (Uni opr (getExp (head exprs)))
                                    in
                                        addOpr op (tail oprs) (expr:(tail exprs))
                            PosBin p opr -> 
                                if (length exprs) < 2 then error (show p)
                                else 
                                    let 
                                        expr1 = getExp (head exprs)
                                        expr2 = getExp (exprs!!1)
                                        expr = Create p (Bi opr expr1 expr2)
                                    in
                                        addOpr op (tail oprs) (expr:(tail (tail exprs)))

                                




parse_normal_expr oprs exprs [] = (makeTree oprs exprs, [])
parse_normal_expr oprs exprs toks = 
    let
        tok = nextToken toks
    in
        case tok of
            PLUS p s -> 
                let 
                    opr = PosBin p Plus
                    (new_oprs, new_exprs) = addOpr opr oprs exprs
                in
                    parse_normal_expr new_oprs new_exprs (tail toks)
            MINUS p s -> 
                let 
                    opr = PosBin p Minus
                    (new_oprs, new_exprs) = addOpr opr oprs exprs
                in
                    parse_normal_expr new_oprs new_exprs (tail toks)
            TIMES p s -> 
                let 
                    opr = PosBin p Times
                    (new_oprs, new_exprs) = addOpr opr oprs exprs
                in
                    parse_normal_expr new_oprs new_exprs (tail toks)
            NEGATE p s -> 
                let 
                    opr = PosUni p Negate
                    (new_oprs, new_exprs) = addOpr opr oprs exprs
                in
                    parse_normal_expr new_oprs new_exprs (tail toks)
            EQUALS p s -> 
                let 
                    opr = PosBin p Equals
                    (new_oprs, new_exprs) = addOpr opr oprs exprs
                in
                    parse_normal_expr new_oprs new_exprs (tail toks)
            LESSTHAN p s -> 
                let 
                    opr = PosBin p LessThan
                    (new_oprs, new_exprs) = addOpr opr oprs exprs
                in
                    parse_normal_expr new_oprs new_exprs (tail toks)
            GREATERTHAN p s -> 
                let 
                    opr = PosBin p GreaterThan
                    (new_oprs, new_exprs) = addOpr opr oprs exprs
                in
                    parse_normal_expr new_oprs new_exprs (tail toks)
            NOT p s -> 
                let 
                    opr = PosUni p Not
                    (new_oprs, new_exprs) = addOpr opr oprs exprs
                in
                    parse_normal_expr new_oprs new_exprs (tail toks)
            AND p s -> 
                let 
                    opr = PosBin p And
                    (new_oprs, new_exprs) = addOpr opr oprs exprs
                in
                    parse_normal_expr new_oprs new_exprs (tail toks)
            OR p s -> 
                let 
                    opr = PosBin p Or
                    (new_oprs, new_exprs) = addOpr opr oprs exprs
                in
                    parse_normal_expr new_oprs new_exprs (tail toks)
            XOR p s -> 
                let 
                    opr = PosBin p Xor
                    (new_oprs, new_exprs) = addOpr opr oprs exprs
                in
                    parse_normal_expr new_oprs new_exprs (tail toks)
            IMPLIES p s -> 
                let 
                    opr = PosBin p Implies
                    (new_oprs, new_exprs) = addOpr opr oprs exprs
                in
                    parse_normal_expr new_oprs new_exprs (tail toks)

            INT p x -> parse_normal_expr oprs ((Create p (Num x)):exprs) (tail toks)
            CONST p x -> parse_normal_expr oprs ((Create p (Const x)):exprs) (tail toks)
            ID p s -> parse_normal_expr oprs ((Create p (Id s)):exprs) (tail toks)

            LPAREN p s -> 
                let
                    (paren_expr, new_toks) = parse_paren toks
                in
                    parse_normal_expr oprs (paren_expr:exprs) (new_toks)
            LET p s -> 
                let
                    (let_in_expr, new_toks) = parse_LetIn toks
                in
                    parse_normal_expr oprs (let_in_expr:exprs) (new_toks)
            IF p s -> 
                let
                    (if_then_else_expr, new_toks) = parse_IfThenElse toks
                in
                    parse_normal_expr oprs (if_then_else_expr:exprs) (new_toks)

            THEN p s -> (makeTree oprs exprs, toks)
            ELSE p s -> (makeTree oprs exprs, toks)
            RPAREN p s -> (makeTree oprs exprs, toks)
            IN p s -> (makeTree oprs exprs, toks)

            ASSIGN p s -> error (show p)
            


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






    


