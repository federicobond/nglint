import System.Environment
import System.Exit
import Text.Parsec
import Text.Parsec.Error
import NgLint.Parser
import NgLint.Linter
import NgLint.Matchers


printUsage :: IO ()
printUsage = putStrLn "usage: nglint file.conf"


lintFile :: FilePath -> IO [LintMessage]
lintFile fileName = do
    content <- readFile fileName
    let config = parse configFile fileName content
    case config of
        Left error -> do
            print error
            return []
        Right (Config decls) -> do
            mapM_ (printLintMessage content) messages
            return messages
            where messages = lint decls


main :: IO ()
main = do
    params <- getArgs
    case length params of
        0 -> printUsage
        _ -> do
            totalMessages <- mapM lintFile params
            let num = length $ concat totalMessages
            putStrLn $ show num ++ " hints."
            if num > 0 then exitFailure else exitSuccess
