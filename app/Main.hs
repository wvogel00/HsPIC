module Main where

import Lib
import PIC.Generator
import PIC.Parser
import System.Environment (getArgs)
import Text.Trifecta.Result

fromSuccess v = case v of
    Success a -> a
    Failure _ -> []

main :: IO ()
main = do
    (filename : _) <- getArgs
    if filename == []
        then putStrLn "プロジェクトファイルを指定してください"
        else do
            result <- runParse <$> readFile filename
            case result of
                Success contents -> do
                    outputHexFile "test.hex" $ genRecords contents
                    putStrLn "正常にhexファイルを生成しました"
                Failure _ -> putStrLn "ファイルエラーです"
