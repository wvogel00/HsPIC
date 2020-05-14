module PIC.Parser where

import Text.Trifecta
import Text.Trifecta.Delta (Delta(..))
import PIC.Generator
import PIC.Types

runParse :: String -> Result [Expr]
runParse = parseString parse (Columns 0 0)

parse = return $ [Boolean True]