# HsPIC

HsPIC helps to develop PIC microcomputer with Haskell-Like grammar.
almost of PIC programming include the I/O interactions, HsPIC is not perfectlly imitate Haskell.
It allow the non-pure programming without monad, but we can use finite lists.
When using variables, the value is curried like ST monad.

### Intel HEX Format
the HEX file for PIC is written in Intel HEX Format.  
(attention : it is written in little-endian)

### support chip
At First, only PIC10F series.
HsPIC will support other chips in the future.

### grammar (Under Implementation)
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

if you want to directly write assembly code to build library, for example, the following grammar is available

```
led_blink = |assembly|
  movlw b00100000
  xorwf portb
  call delay1000
  led_blink
```
you can also write above code as below.
```
led_blink = do
  invert (portb 5)
  delay_ms 1000
  led_blink
```

### equipment status
now still work on the parser
