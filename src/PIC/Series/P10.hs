module PIC.Series.P10 where

import Data.Word 
data IntructionSets = ADDWF Word8 Word8
    | ANDWF Word8 Word8
    | CLRF  Word8
    | CLRW
    | COMF Word8 Word8
    deriving (Eq, Show)