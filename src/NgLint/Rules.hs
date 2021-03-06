module NgLint.Rules where

import Control.Arrow ((>>>))
import NgLint.Matchers
import NgLint.Messages
import NgLint.Parser

type Rule = [Decl] -> [LintMessage]

mkRule :: ErrorCode -> Matcher -> Rule
mkRule code matcher = matcher >>> label code

noRootInsideLocation :: Rule
noRootInsideLocation = mkRule NG001 $
    matchBlock "location" >>>
    matchDirective "root"

ifIsEvil :: Rule
ifIsEvil = mkRule NG002 matchIfDecl

sslv3Enabled :: Rule
sslv3Enabled = mkRule NG003 $
    matchDirective "ssl_protocols" >>>
    matchArg "SSLv3"

serverTokensOn :: Rule
serverTokensOn = mkRule NG004 $
    matchDirective "server_tokens" >>>
    matchArg "on"

tlsv1Enabled :: Rule
tlsv1Enabled = mkRule NG005 $
    matchDirective "ssl_protocols" >>>
    matchArg "TLSv1"
