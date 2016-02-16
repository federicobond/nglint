module NgLint.Messages where

import Control.Arrow ((>>>))
import NgLint.Parser
import Text.Parsec.Pos (SourcePos)


data ErrorCode = NG001 | NG002 | NG003 | NG004 | NG005 deriving (Eq)

data LintMessage = LintMessage SourcePos ErrorCode deriving (Eq)

instance Show LintMessage where
    show (LintMessage pos code) = show pos ++ ": " ++ show code

instance Ord LintMessage where
    compare (LintMessage p1 _) (LintMessage p2 _) = compare p1 p2

instance Show ErrorCode where
    show NG001 = "NG001: root directive inside location block"
    show NG002 = "NG002: if can be replaced with something else"
    show NG003 = "NG003: enabling SSLv3 leaves you vulnerable to POODLE attack"
    show NG004 = "NG004: enabling server_tokens leaks your web server version number"
    show NG005 = "NG005: enabling TLSv1 leaves you vulnerable to CRIME attack"


label :: ErrorCode -> [Decl] -> [LintMessage]
label code = map buildMessage
    where buildMessage decl = LintMessage (getPos decl) code
          getPos (Comment pos _) = pos
          getPos (Block pos _ _ _) = pos
          getPos (Directive pos _ _) = pos
          getPos (IfDecl pos _ _) = pos
