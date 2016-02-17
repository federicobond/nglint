module NgLint.Matchers where

import NgLint.Parser

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
          matches (IfDecl _ _ decls) = matchDirective name decls
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
