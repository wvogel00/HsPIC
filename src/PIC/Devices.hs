module PIC.Devices where

data Device = P16F1705 deriving (Eq, Show)

data ClockType = Internal | External deriving (Eq,Show)

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