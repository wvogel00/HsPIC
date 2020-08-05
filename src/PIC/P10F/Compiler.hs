module PIC.P10F.Compiler where

import Text.Trifecta
import Text.Trifecta.Delta (Delta(..))
import PIC.Series.P10
import PIC.Types
import Control.Applicative
import Data.Char (toUpper)
import Data.Maybe
import Data.List
import Debug.Trace

parsetest p = parseString p (Columns 0 0)

-- parser :: Parser (PIC, String)
parser = (,) <$> (string "{-#" *> getDeviceP <* string "#-}") <*> parseExpr

deviceP :: Parser PIC
deviceP = string "PIC10F" *> ( PIC10F . read <$> many digit)
      <|> string "PIC12F" *> ( PIC12F . read <$> many digit)
      <|> string "PIC16F" *> ( PIC16F . read <$> many digit)
      <|> string "PIC18F" *> ( PIC18F . read <$> many digit)
      <|> string "PIC18C" *> ( PIC18C . read <$> many digit)

getDeviceP = spaces *> string "DEVICE" *> spaces *> deviceP <* spaces

parseExpr = (,,,) <$> funcP <*> variablesP <*> optionP <*> exprP

funcP = many alphaNum

variablesP = spaces *> (
    try (string "=" >> return [])
    <|> ( (:) <$> many alphaNum <*> variablesP)
    )

optionP = spaces *> char '|' *> many alphaNum <* char '|' <* spaces

exprP = many alphaNum

data Unit = Mega | Kilo | NoUnit deriving (Eq, Show)

numP = do
    binNumP
    <|> hexNumP
    <|> ( calcUnit <$> many digit <*> numUnitP)

toDecimal :: (Char -> Int -> Int) -> [Char] -> Int
toDecimal f ns = sum . zipWith f ns $ reverse [0 .. length ns-1]

binNumP :: Parser Int
binNumP = bin2dec <$> (string "0b" *> many digit) where
    bin2dec nums = toDecimal bin2dec' nums
    bin2dec' '1' x = 2^x
    bin2dec' '0' x = 0
    bin2dec' _ x = trace ( "the value is expected binary value composed of '0' or '1'. The written value is incorrect") undefined

hexNumP = hex2dec <$> (string "0x" *> many alphaNum) where
    hex2dec nums = toDecimal hex2dec' nums
    hex2dec' c x = case lookup (toUpper c) $ zip "0123456789ABCDEF" [0..] of
        Just v  -> v * 16^x
        Nothing -> trace ( "the value is expected binary value composed of {'0' to 'F'}. The written value is incorrect") undefined

numUnitP = (try $ char 'M' *> many alphaNum >> return Mega)
    <|> (try $ char 'k' *> many alphaNum >> return Kilo)
    <|> return NoUnit

calcUnit v Mega = 10^6* read v
calcUnit v Kilo = 10^3* read v
calcUnit v NoUnit = read v