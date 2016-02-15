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
        , serverTokensOn ]


lint :: [Decl] -> [LintMessage]
lint decls = sort $ concatMap (\rule -> rule decls) rules


printLintMessage :: String -> LintMessage -> IO ()
printLintMessage contents (LintMessage pos msg)= do
    let lns = lines contents

    setSGR [SetConsoleIntensity BoldIntensity]
    putStrLn $ "In " ++ sourceName pos ++ ", line " ++ show (sourceLine pos) ++ ":"
    setSGR [Reset]

    putStrLn (lns !! (sourceLine pos - 1))
    setSGR [SetColor Foreground Vivid Yellow]
    putStr $ replicate (sourceColumn pos - 1) ' '
    putStrLn ("^-- " ++ msg)
    setSGR [Reset]

    putChar '\n'
