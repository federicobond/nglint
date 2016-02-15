module NgLint.Matchers where

import NgLint.Parser
import Text.Parsec.Pos (SourcePos)


data LintMessage = LintMessage SourcePos String deriving (Eq)

instance Show LintMessage where
    show (LintMessage pos str) = show pos ++ ": " ++ str

instance Ord LintMessage where
    compare (LintMessage p1 _) (LintMessage p2 _) = compare p1 p2

type Matcher = [Decl] -> [Decl]


matchBlock :: String -> Matcher
matchBlock name = concatMap matches
    where matches block@(Block _ bname _ decls) =
              if bname == name
                  then block : matchBlock name decls
                  else matchBlock name decls
          matches _ = []


matchDirective :: String -> Matcher
matchDirective name = concatMap matches
    where matches directive@(Directive _ dname _) =
              [directive | dname == name]
          matches (Block _ _ _ decls) = matchDirective name decls
          matches _ = []


matchIfDecl :: Matcher
matchIfDecl = concatMap matches
    where matches ifDecl@(IfDecl _ _ _) = [ifDecl]
          matches (Block _ _ _ decls) = matchIfDecl decls
          matches _ = []


matchArg :: String -> Matcher
matchArg str = filter hasArg
    where hasArg (Directive _ _ args) = str `elem` args
          hasArg _ = False


label :: String -> [Decl] -> [LintMessage]
label msg = map buildMessage
    where buildMessage decl = LintMessage (getPos decl) msg
          getPos (Comment pos _) = pos
          getPos (Block pos _ _ _) = pos
          getPos (Directive pos _ _) = pos
          getPos (IfDecl pos _ _) = pos
