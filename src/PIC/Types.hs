module PIC.Types where

data Expr =
    Register ConfigRegister
    | Func String [Expr]
    | Boolean Bool
    | Var String
    | INumber Int
    | FNumber Float
    | Str String
    | Ope2 Expr Expr
    | Ope1 Expr
    | IF Expr Expr Expr
    | WHILE Expr Expr
    deriving (Eq, Show)