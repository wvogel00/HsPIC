# HsPIC

The purpose of HsPIC is able to develop PIC microcomputer with Haskell-Like grammar.

### Intel HEX Format
the HEX file for PIC is written in Intel HEX Format.  
(attention : it is written in little-endian)

### support chip
only PIC16F1705
(It may run on the same series chips)

### grammar
The supposed grammar is as below...

```
initRegister = Register{xfreq=8*10^6, won = off, fosc = Internal}

led = P12

l_tika us = replicateM 10 l_tika' where
    l_tika' span = do
        digitalPin led HIGH >> delayUs us
        digitalPin led LOW >> delayUs us

main = do
    setupDevice P16F1705 initRegister
    l_tika 0.1
```

### equipment status
now still work on the parser
