{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BinaryLiterals #-}

module PIC.Series.P10 where

import Data.Word
import Debug.Trace
import Data.Maybe
import Data.List (lookup)

data FRegister = F Int deriving (Eq, Show)
data DFlag = O | I deriving (Eq, Show)
type KValue = Int
data BitIndex = B0 | B1 | B2 | B3 | B4 | B5 | B6 | B7 deriving (Eq, Show, Enum)

dValue :: DFlag -> Int
dValue O = 0
dValue I = 32

bValue :: BitIndex -> Int
bValue = read.tail.show

toIndex :: Int -> BitIndex
toIndex n = fromJust . lookup n $ zip [0..7] [B0 ..]

fValue :: FRegister -> Int
fValue f@(F v)
    | 0 <= v && v < 32 = v
    | otherwise = trace ("FRegister value is not allowable : " ++ show f) undefined

asmToBin :: IntructionSets -> [Word8]
asmToBin (ADDWF f d)    = map fromIntegral [0b0001, 128+64+dValue d+fValue f]
asmToBin (ANDWF f d)    = map fromIntegral [0b0001,     64+dValue d+fValue f]
asmToBin (CLRF f)       = map fromIntegral [0b0000,     64+32+      fValue f]
asmToBin CLRW           = map fromIntegral [0b0000,     64                  ]
asmToBin (COMF f d)     = map fromIntegral [0b0010,     64+dValue d+fValue f]
asmToBin (DECF f d)     = map fromIntegral [0b0000, 128+64+dValue d+fValue f]
asmToBin (DECFSZ f d)   = map fromIntegral [0b0010, 128+64+dValue d+fValue f]
asmToBin (INCF f d)     = map fromIntegral [0b0010, 128+   dValue d+fValue f]
asmToBin (INCFSZ f d)   = map fromIntegral [0b0011, 128+64+dValue d+fValue f]
asmToBin (IORWF f d)    = map fromIntegral [0b0001,        dValue d+fValue f]
asmToBin (MOVF f d)     = map fromIntegral [0b0010,        dValue d+fValue f]
asmToBin (MOVWF f)      = map fromIntegral [0b0000,        32+      fValue f]
asmToBin NOP            = map fromIntegral [0b0000, 0]
asmToBin (RLF f d)      = map fromIntegral [0b0011,     64+dValue d+fValue f]
asmToBin (RRF f d)      = map fromIntegral [0b0011,        dValue d+fValue f]
asmToBin (SUBWF f d)    = map fromIntegral [0b0000, 128+   dValue d+fValue f]
asmToBin (SWAPF f d)    = map fromIntegral [0b0011, 128+   dValue d+fValue f]
asmToBin (XORWF f d)    = map fromIntegral [0b0001, 128+   dValue d+fValue f]
-- ビット指向のファイルレジスタ命令
asmToBin (BCF f b)      = map fromIntegral [0b0100,        bValue b+fValue f]
asmToBin (BSF f b)      = map fromIntegral [0b0101,        bValue b+fValue f]
asmToBin (BTFSC f b)    = map fromIntegral [0b0110,        bValue b+fValue f]
asmToBin (BTFSS f b)    = map fromIntegral [0b0111,        bValue b+fValue f]
-- リテラル及びコントロール命令
asmToBin (ANDLW k)      = map fromIntegral [0b1110,      k]
asmToBin (CALL k)       = map fromIntegral [0b1001,      k]
asmToBin CLRWDT         = map fromIntegral [0b0000, 0b0100]
asmToBin (GOTO k)       = map fromIntegral [0b1010+div k 256, k]
asmToBin (IORLW k)      = map fromIntegral [0b1101,      k]
asmToBin (MOVLW k)      = map fromIntegral [0b1100,      k]
asmToBin OPTION         = map fromIntegral [0b0000, 0b0010]
asmToBin (RETLW k)      = map fromIntegral [0b1000,      k]
asmToBin SLEEP          = map fromIntegral [0b0000, 0b0011]
asmToBin (TRIS f)       = map fromIntegral [0b0000, fValue f]
asmToBin (XORLW k)      = map fromIntegral [0b1111,      k]

data PinType = Input | Output | InOut | CLK | Vdd | Vss | NC

-- 0 <= f <= 31, d = {0,1}, 0 <= k <= 511, 0 <= b <= 7
data IntructionSets = ADDWF FRegister DFlag
    | ANDWF FRegister DFlag
    | CLRF  FRegister
    | CLRW
    | COMF FRegister DFlag
    | DECF FRegister DFlag
    | DECFSZ FRegister DFlag
    | INCF  FRegister DFlag
    | INCFSZ FRegister DFlag
    | IORWF FRegister DFlag
    | MOVF FRegister DFlag
    | MOVWF FRegister
    | NOP
    | RLF FRegister DFlag
    | RRF FRegister DFlag
    | SUBWF FRegister DFlag
    | SWAPF FRegister DFlag
    | XORWF FRegister DFlag
    | BCF FRegister BitIndex
    | BSF FRegister BitIndex
    | BTFSC FRegister BitIndex
    | BTFSS FRegister BitIndex
    | ANDLW KValue
    | CALL KValue
    | CLRWDT
    | GOTO KValue
    | IORLW KValue
    | MOVLW KValue
    | OPTION
    | RETLW KValue
    | SLEEP
    | TRIS FRegister
    | XORLW KValue
    | Label
    deriving (Eq, Show)

data P10Register = INDF
    | TMR0
    | PCL
    | STATUS
    | FSR
    | OSCCAL
    | GPIO
    | CMCON
    deriving (Eq, Show)

registerAddress :: P10Register -> Word8
registerAddress INDF    = 0x00
registerAddress TMR0    = 0x01
registerAddress PCL     = 0x02
registerAddress STATUS  = 0x03
registerAddress FSR     = 0x04
registerAddress OSCCAL  = 0x05
registerAddress GPIO    = 0x06
registerAddress CMCON   = 0x07