module Typing (getType) where

import Ast
import Data.Map

type VarTypeBindings = Map String Type


getType expr = checkAndGetType expr empty

checkAndGetType :: Expr -> VarTypeBindings -> Type


isIntegerBinOp :: BinOp -> Bool
isIntegerUniOp :: UnOp -> Bool
isBooleanBinOp :: BinOp -> Bool
isBooleanUniOp :: UnOp -> Bool
isEqualsBinOp :: BinOp -> Bool
returnTypeBinOp :: BinOp -> Type
returnTypeUniOp :: UnOp -> Type

isIntegerBinOp op =
    case op of
        Plus -> True
        Minus -> True
        Times -> True
        GreaterThan -> True
        LessThan -> True
        _ -> False

isIntegerUniOp op =
    case op of
        Negate -> True
        _      -> False

isBooleanUniOp op =
    case op of
        Not -> True
        _   -> False

isBooleanBinOp op =
    case op of
        And -> True
        Or  -> True
        Xor -> True
        Implies -> True
        _       -> False

isEqualsBinOp op =
    case op of
        Equals -> True
        _      -> False

returnTypeBinOp op =
    case op of 
        Plus -> IntType
        Minus -> IntType
        Times -> IntType
        _     -> BoolType
        -- Equals -> BoolType
        -- GreaterThan -> BoolType
        -- LessThan -> BoolType
        -- And -> BoolType
        
returnTypeUniOp op =
    case op of
        Not -> BoolType
        Negate -> IntType

        

typeCheckBinOp :: BinOp -> Type -> Type -> Type

typeCheckBinOp op t1 t2 = 
    if ((isIntegerBinOp op) && (t1 == IntType) && (t2 == IntType)) then returnTypeBinOp op
    else if ((isBooleanBinOp op) && (t1 == BoolType) && (t2 == BoolType)) then returnTypeBinOp op
    else if ((isEqualsBinOp op) && (t1 == t2)) then BoolType
    else error ((show op) ++ " Operator does not type check")


typeCheckUniOp :: UnOp -> Type -> Type

typeCheckUniOp op t1 = 
    if (isIntegerUniOp op) && (t1 == IntType) then returnTypeUniOp op
    else if (isBooleanUniOp op) && (t1 == BoolType) then returnTypeUniOp op
    else error ((show op) ++ " Operator does not type check")
    


checkAndGetType expr binds = 
    case expr of
        BinExpr op expr1 expr2 -> 
            let 
                t1 = checkAndGetType expr1 binds
                t2 = checkAndGetType expr2 binds
            in
                typeCheckBinOp op t1 t2

        UnExpr op expr1 ->
            let 
                t1 = checkAndGetType expr1 binds
            in
                typeCheckUniOp op t1

        Ite expr1 expr2 expr3 ->
            let
                t1 = checkAndGetType expr1 binds
                t2 = checkAndGetType expr2 binds  -- In haskell every binding is lazy evaluated
                t3 = checkAndGetType expr3 binds
            in
                if not (t1 == BoolType) then error "Expected boolean type in the if clause"
                else if (t2 == t3) then t2
                else error $ "Expected similar types in then and else clause " ++ (show t2) ++ " " ++ (show t3)

        Let (Decl id expr1) expr2 ->
            let
                t1 = checkAndGetType expr1 binds
                new_binds = insert id t1 binds
                t2 = checkAndGetType expr2 new_binds
            in
                t2

        BoolConst _ -> BoolType

        IntConst  _ -> IntType

        VarExpr id -> findWithDefault (error ("Unknown Variable" ++ id)) id binds

        LambdaExpr param_id param_type expr1 closure ->
            let
                new_binds = insert param_id param_type binds
                return_type = checkAndGetType expr1 new_binds
            in
                CurryExpr param_type return_type

        NamedFunExpr fun_id param_id param_type return_type fun_expr closure ->
            let
                new_binds1 = insert fun_id (CurryExpr param_type return_type) binds
                new_binds2 = insert param_id param_type new_binds1
                evaluated_return_type = checkAndGetType fun_expr new_binds2
            in
                if evaluated_return_type == return_type then CurryExpr param_type return_type
                else error "Given return type and evaluated return type don't match"

        FunAppExpr fun_id expr1 ->
            let
                fun_type = findWithDefault (error ("Unknown Variable " ++ fun_id)) fun_id binds
                t1 = checkAndGetType expr1 binds
            in
                case fun_type of
                    CurryExpr param_type rtn_type ->
                        if (param_type == t1) then rtn_type
                        else error ("Invalid type applied to function: " ++ fun_id)
                    _ -> error (fun_id ++ " is not a function")
        
        LambdaFunAppExpr id id_type fun_expr inp_expr closure ->
            let
                inp_type = checkAndGetType inp_expr binds
                new_binds = insert id id_type binds  -- Haskell is Lazy!
            in
                if inp_type == id_type then checkAndGetType fun_expr new_binds
                else error ("Invalid type applied to Lambda function")

        NamedFunAppExpr fun_id param_id param_type return_type fun_expr inp_expr closure ->
            let
                inp_type = checkAndGetType inp_expr binds
                new_binds1 = insert fun_id (CurryExpr param_type return_type) binds
                new_binds2 = insert param_id param_type new_binds1
                evaluated_return_type = checkAndGetType fun_expr new_binds2
            in
                if inp_type == param_type then
                    if return_type == evaluated_return_type then return_type
                    else error "Given return type and evaluated return type don't match"
                else error ("Invalid type applied to function: " ++ fun_id)

        GeneralAppExpr fun_expr inp_expr ->
            let 
                fun_type = checkAndGetType fun_expr binds
                inp_type = checkAndGetType inp_expr binds
            in
                case fun_type of 
                    CurryExpr param_type rtn_type ->
                        if (param_type == inp_type) then rtn_type
                        else error ("Invalid type applied to function")
                    _ -> error ("Invalid function application")







            

            
        
            





