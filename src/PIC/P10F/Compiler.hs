module PIC.P10F.Compiler where

import Text.Trifecta
import Text.Trifecta.Delta (Delta(..))
import PIC.Series.P10
import PIC.Types
import Control.Applicative

parsetest = parseString parser (Columns 0 0)

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

variablesP = spaces *> (try (string "=" >> return [])
    <|> ( (:) <$> many alphaNum <*> variablesP))

optionP = spaces *> char '|' *> many alphaNum <* char '|' <* spaces

exprP = many alphaNum