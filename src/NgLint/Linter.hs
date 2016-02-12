module NgLint.Linter where

import Data.List
import Data.Maybe
import NgLint.Parser
import Text.Parsec.Pos

data LintMessage = LintMessage SourcePos String

instance Show LintMessage where
    show (LintMessage pos str) = show pos ++ ": " ++ str


-- matchBlock :: String -> Matcher
-- matchBlock str (Block name _ _) = str == name
-- matchBlock _ _ = False


-- matchDirective :: String -> Matcher
-- matchDirective str (Directive name _) = str == name
-- matchDirective _ _ = False

getPos :: Decl -> SourcePos
getPos (Comment pos _) = pos
getPos (Block pos _ _ _) = pos
getPos (Directive pos _ _) = pos


isRootDirective (Directive _ name _) = name == "root"
isRootDirective _ = False


noRootInsideLocation :: Decl -> [LintMessage]
noRootInsideLocation (Block _ name _ decls) =
    if name == "location" && any isRootDirective decls
        then
            let pos = getPos (fromJust (find isRootDirective decls)) in
            [LintMessage pos "root directive inside location block"]
        else concatMap noRootInsideLocation decls
noRootInsideLocation _ = []


-- checkIfFileExists :: Decl -> [LintMessage]
-- checkIfFileExists (Block pos name args decls) =
--     if name == "if" && args == ["(!-f $request_filename)"]
--     then [LintMessage pos ]
--     concatMap checkIfFileExists decls
-- checkI


-- rules :: [(Decl -> [LintMessage])]
rules = [noRootInsideLocation]


lint :: [Decl] -> [LintMessage]
lint decls = concat [rule decl | decl <- decls, rule <- rules]


-- traverse' :: [Decl] -> [LintMessage] -> [LintMessage]
-- traverse' decls _ = concat [chk decl | decl <- decls] where
--     chk (Comment _) = []
--     chk (Directive _ _) = []
--     chk (Block name _ decls) = []
