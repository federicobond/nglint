module NgLint.Output.Common where

import NgLint.Messages

type Formatter = String -> [LintMessage] -> IO ()
