module PIC.Types where

import Data.Word
import qualified PIC.Series.P10 as P10
import qualified PIC.Series.P16F17XX as P16F17XX

type ClockType = Bool

-- 16f17xxの場合
data ConfigRegister = ConfigRegister{
    fosc :: ClockType,
    won :: Bool, 
    t_power :: Bool, 
    mclr :: Bool,
    codeprotect :: Bool,
    bor :: Bool,
    clkout :: Bool,
    ieso :: Bool,
    fcm :: Bool,
    wr :: Bool,
    pps1way :: Bool,
    zcddis :: Bool,
    pll :: Bool,
    stvr :: Bool,
    bors :: Bool,
    lpbor :: Bool,
    lvp :: Bool
    }
    deriving (Eq, Show)

data Expr =
    Register Bool
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

-- 使えるアセンブラ種類で分類
-- 12,16は8bit命令, 18は16bit命令, 16F17xxは16bit命令
data PIC = PIC10F Int | PIC12F Int | PIC16F Int
    | PIC16F17XX Int | PIC18F Int | PIC18C Int
    deriving (Eq, Show)

-- PIC.Series.**からロード
data IntructionSets = IntructionSets deriving (Eq, Show)