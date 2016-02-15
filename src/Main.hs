import System.Environment
import System.Exit
import Text.Parsec
import Text.Parsec.Error
import NgLint.Parser
import NgLint.Linter


printUsage :: IO ()
printUsage = putStrLn "usage: nglint file.conf"


main = do
    params <- getArgs
    case length params of
        0 -> printUsage
        _ -> do 
            let fileName = head params
            content <- readFile fileName

            let config = parse configFile fileName content
            case config of
                Left error -> do
                    print error
                    exitFailure
                Right (Config decls) ->
                    if null messages
                        then exitSuccess
                        else do
                            mapM_ (printLintMessage content) messages
                            exitFailure
                    where messages = lint decls
