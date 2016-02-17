module NgLint.Output.Pretty where

import Data.List
import NgLint.Messages
import NgLint.Output.Common
import System.Console.ANSI
import Text.Parsec.Pos

printMessage :: LintMessage -> IO ()
printMessage (LintMessage pos code) = do
    setSGR [SetColor Foreground Vivid Yellow]
    putStr $ replicate (sourceColumn pos - 1) ' '
    putStrLn ("^-- " ++ show code)
    setSGR [Reset]

printMessageGroup :: [String] -> [LintMessage] -> IO ()
printMessageGroup lns messages = do
    let (LintMessage pos _) = head messages

    setSGR [SetConsoleIntensity BoldIntensity]
    putStrLn $ "In " ++ sourceName pos ++ ", line " ++ show (sourceLine pos) ++ ":"
    setSGR [Reset]
    putStrLn (lns !! (sourceLine pos - 1))

    mapM_ printMessage messages
    putChar '\n'

printMessages :: Formatter
printMessages contents messages = do
    let lns = lines contents
        messageGroups = groupBy eq messages
        eq (LintMessage p1 _) (LintMessage p2 _) = p1 == p2

    mapM_ (printMessageGroup lns) messageGroups
