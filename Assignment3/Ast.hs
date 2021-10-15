module Ast where

type Id = String

data VarDecl = Decl Id Expr
        deriving (Eq,Show)

data UnOp = Negate | Not
        deriving (Eq,Show)

data BinOp = Plus | Minus | Times | Equals | LessThan | GreaterThan | And | Or | Xor | Implies
        deriving (Eq,Show)

data Type = IntType | BoolType |  CurryExpr Type Type 
                deriving (Eq,Show)

data Lambda = Lambda Id Type Expr
                deriving (Eq,Show)

data FunApp = FunApp Id Expr
                deriving (Eq,Show)

data NamedFun = NamedFun Id Id Type Type Expr
                deriving (Eq, Show)

data Expr =
             BinExpr BinOp Expr Expr
           | UnExpr UnOp Expr
           | Ite Expr Expr Expr
           | Let VarDecl Expr
           | BoolConst Bool
           | IntConst Integer
           | VarExpr Id
           | LambdaExpr Id Type Expr
           | NamedFunExpr Id Id Type Type Expr
           | FunAppExpr Id Expr
           | LambdaFunAppExpr Id Type Expr Expr
           | NamedFunAppExpr Id Id Type Type Expr Expr
        deriving (Eq,Show)


