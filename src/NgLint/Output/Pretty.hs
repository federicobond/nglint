module NgLint.Output.Pretty where

import NgLint.Matchers
import System.Console.ANSI
import Text.Parsec.Pos


printMessage :: LintMessage -> IO ()
printMessage (LintMessage pos code) = do
    setSGR [SetColor Foreground Vivid Yellow]
    putStr $ replicate (sourceColumn pos - 1) ' '
    putStrLn ("^-- " ++ show code)
    setSGR [Reset]


printGroupedMessages :: String -> [LintMessage] -> IO ()
printGroupedMessages contents messageList = do
    let lns = lines contents
        (LintMessage pos _) = head messageList


    setSGR [SetConsoleIntensity BoldIntensity]
    putStrLn $ "In " ++ sourceName pos ++ ", line " ++ show (sourceLine pos) ++ ":"
    setSGR [Reset]
    putStrLn (lns !! (sourceLine pos - 1))

    mapM_ printMessage messageList
    putChar '\n'
