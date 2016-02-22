module NgLint.Parser where

import Control.Applicative (liftA)
import Data.Maybe
import NgLint.Position
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as P

data RegexOp = CaseSensitiveRegexMatch
              | CaseInsensitiveRegexMatch
              | CaseSensitiveRegexNotMatch
              | CaseInsensitiveRegexNotMatch
              deriving (Show, Eq)

data CmpOp = Equal | NotEqual deriving (Show, Eq)

data FileOp = FileExists
              | FileNotExists
              | DirectoryExists
              | DirectoryNotExists
              | AnyExists
              | AnyNotExists
              | Executable
              | NotExecutable
              deriving (Show, Eq)

data Literal = Variable String | StringLit String deriving (Show, Eq)
data Condition =
    ConditionVariable String
    | Compare CmpOp Literal Literal
    | RegexMatch RegexOp Literal Literal
    | FileOperation FileOp Literal
    deriving (Show, Eq)

data Decl =
    Comment SourcePos String
    | Block SourcePos String [String] [Decl]
    | Directive SourcePos String [String]
    | IfDecl SourcePos Condition [Decl]
    deriving (Show)

data Config = Config [Decl] deriving (Show)

instance Position Decl where
    getPos (Comment pos _) = pos
    getPos (Block pos _ _ _) = pos
    getPos (Directive pos _ _) = pos
    getPos (IfDecl pos _ _) = pos

parseConfig = parse configFile

configFile :: Parser Config
configFile = do
    spaces
    lst <- decl `sepBy` spaces
    spaces
    eof
    return $ Config lst

nginxDef = emptyDef
    { P.identStart     = letter <|> char '_'
    , P.identLetter    = alphaNum <|> char '_' 
    , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
    }

lexer = P.makeTokenParser nginxDef

parens      = P.parens lexer
braces      = P.braces lexer
identifier  = P.identifier lexer

decl :: Parser Decl
decl = choice $ map try [comment, ifDecl, directive, block]

arg = many1 (alphaNum <|> oneOf "\"*_-+/.'$[]~\\:^()|=?!")

-- http://stackoverflow.com/questions/34342911/parsec-parse-nested-code-blocks
sepBy1Try :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy1Try p sep = do
  x <- p
  xs <- many (try $ sep *> p)
  return (x:xs)

sepByTry p sep = sepBy1Try p sep <|> return []

block :: Parser Decl
block = do
    pos <- getPosition
    name <- identifier
    spaces
    args <- arg `sepByTry` spaces
    spaces
    decls <- braces (decl `sepEndBy` spaces)
    spaces
    return $ Block pos name args decls
    <?> "block"

comment :: Parser Decl
comment = do
    pos <- getPosition
    char '#'
    msg <- manyTill anyChar endOfLine
    return $ Comment pos msg
    <?> "comment"


variable :: Parser String
variable = do
    char '$'
    name <- identifier
    return name

stringLit :: Parser Literal
stringLit = do
    char '"'
    str <- manyTill anyChar (try $ char '"')
    return $ StringLit str

condVariable :: Parser Condition
condVariable = do
    name <- variable
    return $ ConditionVariable name

condCompare :: Parser Condition
condCompare = do
    var <- variable
    spaces
    op <- cmpOp
    spaces
    str <- stringLit
    return $ Compare op (Variable var) str

regexMatch :: Parser Condition
regexMatch = do
    var <- variable
    spaces
    op <- regexOp
    spaces
    regex <- stringLit
    return $ RegexMatch op (Variable var) regex

fileOp :: Parser FileOp
fileOp = do
    negative <- liftA isJust $ optionMaybe (char '!')
    char '-'
    op <- choice $ map char "fdex"

    let isNegative a b = if negative then a else b
    return $ case op of
       'f' -> isNegative FileExists FileNotExists
       'd' -> isNegative DirectoryExists DirectoryNotExists
       'e' -> isNegative AnyExists AnyNotExists
       'x' -> isNegative Executable NotExecutable

regexOp :: Parser RegexOp
regexOp = do
    negative <- liftA isJust $ optionMaybe (char '!')
    char '~'
    insensitive <- liftA isJust $ optionMaybe (char '*')
    return $ case (negative, insensitive) of
        (False, False) -> CaseSensitiveRegexMatch
        (False, True)  -> CaseInsensitiveRegexMatch
        (True,  False) -> CaseSensitiveRegexNotMatch
        (True,  True)  -> CaseInsensitiveRegexNotMatch

cmpOp :: Parser CmpOp
cmpOp = do
    negative <- liftA isJust $ optionMaybe (char '!')
    char '='
    return (if negative then NotEqual else Equal)


fileOperation :: Parser Condition
fileOperation = do
    op <- fileOp
    spaces
    var <- variable
    return $ FileOperation op (Variable var)


condition :: Parser Condition
condition = choice $ map try [condCompare, regexMatch, condVariable, fileOperation]


ifDecl :: Parser Decl
ifDecl = do
    pos <- getPosition
    string "if" <?> "if"
    spaces
    cond <- parens condition
    spaces
    decls <- braces (decl `sepEndBy` spaces)
    spaces
    return $ IfDecl pos cond decls

directive :: Parser Decl
directive = do
    pos <- getPosition
    name <- identifier
    spaces
    args <- arg `sepEndBy` spaces
    char ';'
    return $ Directive pos name args
    <?> "directive"
