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



getInt expr =
    case expr of 
        IntConst x -> x
        _ ->  error "type checker bug"

getBool expr =
    case expr of 
        BoolConst b -> b
        _ -> error "type checker bug"




-- performSubstitution:: Expr VarBindings -> Expr
-- performSubstitution expr binds =
--     case expr of
--         LambdaExpr id typ expr -> 
--             LambdaExpr id typ (performSubstitution expr)
--         NamedFun fun_id var_id typ1 typ2 expr -> 
--             NamedFun fun_id var_id typ1 typ2 (performSubstitution expr)
        






evalExpr expr binds =
    case expr of

        IntConst _ -> expr
        BoolConst _ -> expr
        LambdaExpr a b c closure ->  LambdaExpr a b c (union closure binds)
        NamedFunExpr a b c d e closure -> NamedFunExpr a b c d e (union closure binds)

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
             findWithDefault (error ("Unknown Variable: " ++ id)) id binds
        
        FunAppExpr fun_id inp_expr ->
            let
                fun = findWithDefault (error ("Unknown Variable: " ++ fun_id)) fun_id binds
            in
                case fun of
                    LambdaExpr id typ expr1 closure -> 
                        let
                            inp_val =  evalExpr inp_expr binds
                        in  
                            evalExpr (LambdaFunAppExpr id typ expr1 inp_val empty) (insert id inp_val closure)
                            
                    NamedFunExpr fun_id inp_id t1 t2 fun_expr closure ->
                        let
                            inp_val = evalExpr inp_expr binds
                        in
                            evalExpr (NamedFunAppExpr fun_id inp_id t1 t2 fun_expr inp_val empty) (insert inp_id inp_val closure)
                        
                    _ -> error "not a function"
            
        LambdaFunAppExpr inp_id _ lambda_expr inp_expr closure ->
            let 
                inp_val =  evalExpr inp_expr binds -- call by value!
                new_binds = insert inp_id inp_val binds
            in
                if closure /= empty then error "Function evaluation with non-empty closure" 
                else evalExpr lambda_expr new_binds

        NamedFunAppExpr fun_id inp_id t1 t2 fun_expr inp_expr closure ->
            let
                inp_val = evalExpr inp_expr binds -- call by value!
                new_binds1 = insert fun_id (NamedFunExpr fun_id inp_id t1 t2 fun_expr closure) binds
                new_binds2 = insert inp_id inp_val new_binds1
            in
                if  closure /= empty then error "Function evaluation with non-empty closure" else  evalExpr fun_expr new_binds2

        GeneralAppExpr fun_expr inp_expr ->
            let
                fun_val = evalExpr fun_expr binds 
                inp_val = evalExpr inp_expr binds -- Call by value + lazy evaluation!
            in
                case fun_val of 
                    VarExpr id -> evalExpr (FunAppExpr id inp_val) binds
                    LambdaExpr a b c cls -> evalExpr (LambdaFunAppExpr a b c inp_val empty) (union cls binds)
                    NamedFunExpr a b c d e cls  -> evalExpr (NamedFunAppExpr a b c d e inp_val empty) (union cls binds)
                    _ -> error "Invalid function application"

                    
            
