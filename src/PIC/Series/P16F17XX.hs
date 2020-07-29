module PIC.Series.P16F17XX where

import Data.Word

data IntructionSets = ADDWF Word8 Word8
    | ADDWFC Word8 Word8
    | ANDWF Word8 Word8
    | ASRF Word8 Word8
    | LSLF Word8 Word8
    | LSRF Word8 Word8
    | CLRF Word8
    | CLRW 
    | COMF Word8 Word8
    | DECF Word8 Word8
    | INCF Word8 Word8
    | IORWF Word8 Word8
    | MOVF Word8 Word8
    | WOVWF Word8
    | RLF Word8 Word8
    | RRF Word8 Word8
    | SUBWF Word8 Word8
    | SUBWFB Word8 Word8
    | SWAPF Word8 Word8
    | XORWF Word8 Word8
    | DECFSZ Word8 Word8
    | INCFSZ Word8 Word8
    | BCF Word8 Word8
    | BSF Word8 Word8
    | BTFSC Word8 Word8
    | BTFSS Word8 Word8
    | ADDLW Word8
    | ANDLW Word8
    | IORLW Word8
    | MOVLB Word8
    | MOVLP Word8
    | MOVLW Word8
    | SUBLW Word8
    | XORLW Word8
    | BRA Word8
    | BRW
    | CALL Word8
    | CALLW
    | GOTO Word8
    | RETFIE Word8
    | RETLW Word8
    | RETURN
    | CLRWDT
    | NOP
    | OPTION
    | RESET
    | SLEEP
    | TRIS Word8
    | ADDFSR Word8 Word8
    | MOVIW  Word8 Word8
    | MOVIW_N  Word8 Word8
    | MOVWI Word8 Word8
    | MOVWI_N Word8 Word8
    deriving (Eq, Show)