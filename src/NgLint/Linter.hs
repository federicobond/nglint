module NgLint.Linter where

import Data.List
import Data.Maybe
import NgLint.Matchers
import NgLint.Parser
import NgLint.Rules


rules = [ noRootInsideLocation
        , ifIsEvil
        , sslv3Enabled
        , serverTokensOn
        , tlsv1Enabled ]


lint :: [Decl] -> [LintMessage]
lint decls = sort $ concatMap (\rule -> rule decls) rules
