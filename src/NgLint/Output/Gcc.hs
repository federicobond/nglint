module NgLint.Output.Gcc where

import Data.List
import NgLint.Matchers
import System.Console.ANSI
import Text.Parsec.Pos


printMessage :: LintMessage -> IO ()
printMessage (LintMessage pos code) = do
    let (errorNumber, message) = splitAt 5 (show code)
    putStr $ intercalate ":" $ map ($ pos) [sourceName, show . sourceLine, show . sourceColumn]
    putStrLn $ ": warning" ++ message ++ " [" ++ errorNumber ++ "]"


printGroupedMessages :: String -> [LintMessage] -> IO ()
printGroupedMessages contents = mapM_ printMessage
