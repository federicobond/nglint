module NgLint.Linter where

import Data.List
import Data.Maybe
import NgLint.Parser
import NgLint.Matchers
import NgLint.Rules


rules = [ noRootInsideLocation
        , ifIsEvil
        , sslv3Enabled
        , serverTokensOn ]


lint :: [Decl] -> [LintMessage]
lint decls = sort $ concatMap (\rule -> rule decls) rules
