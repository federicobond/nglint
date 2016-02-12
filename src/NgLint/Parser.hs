module NgLint.Parser where

import Text.Parsec
import Text.Parsec.String

data Decl =
    Comment SourcePos String
    | Block SourcePos String [String] [Decl]
    | Directive SourcePos String [String]
    | IfDecl SourcePos String [Decl]
    deriving (Show)
data Config = Config [Decl] deriving (Show)


configFile :: Parser Config
configFile = do
    spaces
    lst <- decl `sepEndBy` spaces
    eof
    return $ Config lst


decl :: Parser Decl
decl = try comment <|> try ifDecl <|> try block <|> try directive


-- http://stackoverflow.com/questions/34342911/parsec-parse-nested-code-blocks
sepBy1Try :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy1Try p sep = do
  x <- p
  xs <- many (try $ sep *> p)
  return (x:xs)

sepByTry p sep = sepBy1Try p sep <|> return []


identifier = many1 (alphaNum <|> char '_')


block :: Parser Decl
block = do
    pos <- getPosition
    name <- identifier
    spaces
    args <- many1 (alphaNum <|> oneOf "\"*_-+/.") `sepByTry` spaces
    spaces
    char '{'
    spaces
    decls <- decl `sepByTry` spaces
    spaces
    char '}'
    return $ Block pos name args decls
    <?> "block"


comment :: Parser Decl
comment = do
    pos <- getPosition
    char '#'
    msg <- manyTill anyChar endOfLine
    return $ Comment pos msg
    <?> "comment"


ifDecl :: Parser Decl
ifDecl = do
    pos <- getPosition
    string "if"
    spaces
    char '('
    spaces
    expr <- manyTill anyChar (char ')')
    spaces
    char ')'
    spaces
    char '{'
    spaces
    decls <- decl `sepByTry` spaces
    spaces
    char '}'
    return $ IfDecl pos expr decls


directive :: Parser Decl
directive = do
    pos <- getPosition
    name <- identifier
    spaces
    args <- many1 (alphaNum <|> oneOf "\"*_-+/.") `sepBy` spaces
    char ';'
    return $ Directive pos name args
    <?> "directive"
