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
asmToBin (ADDWF f d)    = map fromIntegral [1, 128+64+dValue d+fValue f]
asmToBin (ANDWF f d)    = map fromIntegral [1,     64+dValue d+fValue f]
asmToBin (CLRF f)       = map fromIntegral [0,     64+32+      fValue f]
asmToBin CLRW           = map fromIntegral [0,     64                  ]
asmToBin (COMF f d)     = map fromIntegral [2,     64+dValue d+fValue f]
asmToBin (DECF f d)     = map fromIntegral [0, 128+64+dValue d+fValue f]
asmToBin (DECFSZ f d)   = map fromIntegral [2, 128+64+dValue d+fValue f]
asmToBin (INCF f d)     = map fromIntegral [2, 128+   dValue d+fValue f]
asmToBin (INCFSZ f d)   = map fromIntegral [3, 128+64+dValue d+fValue f]
asmToBin (IORWF f d)    = map fromIntegral [1,        dValue d+fValue f]
asmToBin (MOVF f d)     = map fromIntegral [2,        dValue d+fValue f]
asmToBin (MOVWF f)      = map fromIntegral [0,        32+      fValue f]
asmToBin NOP            = map fromIntegral [0, 0]
asmToBin (RLF f d)      = map fromIntegral [3,     64+dValue d+fValue f]
asmToBin (RRF f d)      = map fromIntegral [3,        dValue d+fValue f]
asmToBin (SUBWF f d)    = map fromIntegral [0, 128+   dValue d+fValue f]
asmToBin (SWAPF f d)    = map fromIntegral [3, 128+   dValue d+fValue f]
asmToBin (XORWF f d)    = map fromIntegral [1, 128+   dValue d+fValue f]
-- ビット指向のファイルレジスタ命令
asmToBin (BCF f b)      = map fromIntegral [4,        bValue b+fValue f]
asmToBin (BSF f b)      = map fromIntegral [5,        bValue b+fValue f]
asmToBin (BTFSC f b)    = map fromIntegral [6,        bValue b+fValue f]
asmToBin (BTFSS f b)    = map fromIntegral [7,        bValue b+fValue f]
-- リテラル及びコントロール命令
asmToBin (ANDLW k)      = map fromIntegral [14,     k]
asmToBin (CALL k)       = map fromIntegral [9,      k]
asmToBin CLRWDT         = map fromIntegral [0,      4]
asmToBin (GOTO k)       = map fromIntegral [10+div k 256, k]
asmToBin (IORLW k)      = map fromIntegral [13,     k]
asmToBin (MOVLW k)      = map fromIntegral [12,     k]
asmToBin OPTION         = map fromIntegral [0,      2]
asmToBin (RETLW k)      = map fromIntegral [8,      k]
asmToBin SLEEP          = map fromIntegral [0,      3]
asmToBin (TRIS f)       = map fromIntegral [0, fValue f]
asmToBin (XORLW k)      = map fromIntegral [15,     k]


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
    deriving (Eq, Show)