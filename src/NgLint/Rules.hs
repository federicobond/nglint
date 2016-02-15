module NgLint.Rules where

import Control.Arrow ((>>>))
import NgLint.Parser
import NgLint.Matchers


type Rule = [Decl] -> [LintMessage]


noRootInsideLocation :: Rule
noRootInsideLocation =
    matchBlock "location" >>>
    matchDirective "root" >>>
    label "root inside location"


ifIsEvil :: Rule
ifIsEvil =
    matchIfDecl >>>
    label "consider replacing if with something else"


sslv3Enabled :: Rule
sslv3Enabled =
    matchDirective "ssl_protocols" >>>
    matchArg "SSLv3" >>>
    label "disable SSLv3 to prevent POODLE attacks"


serverTokensOn :: Rule
serverTokensOn =
    matchDirective "server_tokens" >>>
    matchArg "on" >>>
    label "server_tokens leaks your web server version number"
