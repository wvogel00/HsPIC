# HsPIC

The purpose of HsPIC is able to develop PIC microcomputer with Haskell-Like grammar.

The supposed grammar is as below...

```
initRegister = Register{xfreq=8*10^6, won = off, fosc = Internal ..}

led = P12

l_tika sec = replicateM 10 l_tika' where
    l_tika span = do
        digitalPin led HIGH >> delay sec
        digitalPin led LOW >> delay sec

main = do
    setupDevice P16F1705 initRegister
    l_tika 0.1

```