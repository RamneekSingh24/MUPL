module Ast where
import Data.Map

type Id = String

type Closure = Map Id Expr

data VarDecl = Decl Id Expr
        deriving (Eq,Show)

data UnOp = Negate | Not
        deriving (Eq,Show)

data BinOp = Plus | Minus | Times | Equals | LessThan | GreaterThan | And | Or | Xor | Implies
        deriving (Eq,Show)

data Type = IntType | BoolType |  CurryExpr Type Type 
                deriving (Eq,Show)

data Expr =
             BinExpr BinOp Expr Expr
           | UnExpr UnOp Expr
           | Ite Expr Expr Expr
           | Let VarDecl Expr
           | BoolConst Bool
           | IntConst Integer
           | VarExpr Id
           | LambdaExpr Id Type Expr Closure
           | NamedFunExpr Id Id Type Type Expr Closure
           | FunAppExpr Id Expr
           | LambdaFunAppExpr Id Type Expr Expr Closure
           | NamedFunAppExpr Id Id Type Type Expr Expr Closure
           | GeneralAppExpr Expr Expr 
        deriving (Eq,Show)


