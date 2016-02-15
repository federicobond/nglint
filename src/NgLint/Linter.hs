module NgLint.Linter where

import Data.List
import Data.Maybe
import Text.Parsec.Pos
import NgLint.Parser
import NgLint.Matchers
import NgLint.Rules
import System.Console.ANSI


rules = [ noRootInsideLocation
        , ifIsEvil
        , sslv3Enabled
        , serverTokensOn
        , tlsv1Enabled ]


lint :: [Decl] -> [LintMessage]
lint decls = sort $ concatMap (\rule -> rule decls) rules


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
