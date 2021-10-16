module Evaluator (eval) where

import Ast
import Data.Map
import Parser

type VarBindings = Map String Expr


eval expr = evalExpr expr empty


-- data Expr =
--              BinExpr BinOp Expr Expr
--            | UnExpr UnOp Expr
--            | Ite Expr Expr Expr
--            | Let VarDecl Expr
--            | BoolConst Bool
--            | IntConst Integer
--            | VarExpr Id
--            | LambdaExpr Id Type Expr
--            | NamedFunExpr Id Id Type Type Expr
--            | FunAppExpr Id Expr
--            | LambdaFunAppExpr Id Type Expr Expr
--            | NamedFunAppExpr Id Id Type Type Expr Expr
--         deriving (Eq,Show)

evalExpr :: Expr -> VarBindings -> Expr

getInt:: Expr -> Integer
getBool:: Expr -> Bool
getLambda:: Expr -> Lambda
getNamedFun:: Expr -> NamedFun

getInt expr =
    case expr of 
        IntConst x -> x
        _ -> error "type checker bug"

getBool expr =
    case expr of 
        BoolConst b -> b
        _ -> error "type checker bug"

getLambda expr =
    case expr of
        LambdaExpr x y z -> Lambda x y z
        _ -> error "type checker bug"

getNamedFun expr =
    case expr of 
        NamedFunExpr a b c d e -> NamedFun a b c d e
        _ -> error "type checker bug"



evalExpr expr binds =
    case expr of
        IntConst _ -> expr
        BoolConst _ -> expr
        LambdaExpr _ _ _ -> expr
        NamedFunExpr _ _ _ _ _ -> expr

        BinExpr op expr1 expr2 ->
            case op of 
                Plus -> IntConst ((getInt (evalExpr expr1 binds)) + (getInt (evalExpr expr2 binds)))
                Minus -> IntConst ((getInt (evalExpr expr1 binds)) - (getInt (evalExpr expr2 binds)))
                Times -> IntConst ((getInt (evalExpr expr1 binds)) * (getInt (evalExpr expr2 binds)))
                Equals -> BoolConst ((getInt (evalExpr expr1 binds)) == (getInt (evalExpr expr2 binds)))
                GreaterThan -> BoolConst ((getInt (evalExpr expr1 binds)) > (getInt (evalExpr expr2 binds)))
                LessThan -> BoolConst ((getInt (evalExpr expr1 binds)) < (getInt (evalExpr expr2 binds)))
                Or -> BoolConst ((getBool (evalExpr expr1 binds)) || (getBool (evalExpr expr2 binds)))
                And -> BoolConst ((getBool (evalExpr expr1 binds)) || (getBool (evalExpr expr2 binds)))
                Xor -> BoolConst ((getBool (evalExpr expr1 binds)) /= (getBool (evalExpr expr2 binds)))  -- Xor = not equals(\=) for booleans
                Implies -> BoolConst ((not (getBool (evalExpr expr1 binds))) || (getBool (evalExpr expr2 binds))) -- Implies a b = (not a) or b
        
        UnExpr op expr1 ->
            case op of
                Negate -> IntConst (-(getInt (evalExpr expr1 binds)))
                Not -> BoolConst (not (getBool (evalExpr expr1 binds)))
        
        Ite expr_if expr_then expr_else ->
            if (getBool (evalExpr expr_if binds))
            then evalExpr expr_then binds
            else evalExpr expr_else binds
        
        Let (Decl var_id var_expr) expr_in ->
            let 
                var_val = evalExpr var_expr binds
                new_binds = insert var_id var_val binds
            in
                evalExpr expr_in new_binds

        VarExpr id ->
            findWithDefault (error ("Unknown Variable: type checker bug" ++ id)) id binds
        
        FunAppExpr fun_id inp_expr ->
            let
                fun = findWithDefault (error ("Unknown Variable: type checker bug" ++ fun_id)) fun_id binds
            in
                case fun of
                    LambdaExpr a b c -> evalExpr (LambdaFunAppExpr a b c  inp_expr) binds
                    NamedFunExpr a b c d e -> evalExpr (NamedFunAppExpr a b c d e inp_expr) binds
                    _ -> error "not a function, type checker bug"
            
        LambdaFunAppExpr inp_id _ lambda_expr inp_expr ->
            let 
                inp_val = evalExpr inp_expr binds -- call by value!
                new_binds = insert inp_id inp_val binds
            in
                evalExpr lambda_expr new_binds

        NamedFunAppExpr fun_id inp_id t1 t2 fun_expr inp_expr ->
            let
                new_binds1 = insert fun_id (NamedFunExpr fun_id inp_id t1 t2 fun_expr) binds
                inp_val = evalExpr inp_expr new_binds1 -- call by value!
                new_binds2 = insert inp_id inp_val new_binds1
            in
                evalExpr fun_expr new_binds2
            


        