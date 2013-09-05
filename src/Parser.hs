module Parser (parse) where

import System.IO
import ParseTree
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Language as L
import qualified Text.Parsec.Token as T


--- Lexer
lexer = T.makeTokenParser $ L.emptyDef 
    { L.commentStart = "(*"
    , L.commentEnd = "*)"
    , L.commentLine = "%"
    , L.opStart = oneOf ":.\\="
    , L.opLetter = oneOf "=>"
    , L.reservedOpNames = [":", ":=", ".", "\\", "=>"]
    , L.reservedNames = [ "Define", "type", "kind", "Pi"]
    }

--- Parser
parseProgram :: Parser Program
parseProgram = do
    T.whiteSpace lexer
    defs <- (many parseDefinition)
    eof
    return defs

parseDefinition :: Parser Definition
parseDefinition = do
    T.symbol lexer "Define"
    name <- T.identifier lexer
    params <- many parseNamedParam
    T.reservedOp lexer ":"
    typ <- parseExpr
    T.reservedOp lexer ":="
    body <- parseExpr
    T.reservedOp lexer "."
    case params of
        [] -> return (name, typ, body)
        _ -> return (name, (Pi params typ), body)

parseExpr = parseConst <|> parseName <|> parseParExpr <|> parseLambda <|> parseApp
parseConst = parseKind <|> parseType

parseKind = do 
    T.reserved lexer "kind"
    return Kind

parseType = do
    T.reserved lexer "type"
    return Type

parseName = do
    name <- T.identifier lexer
    return (Name name)

parseParExpr = do
    expr <- T.parens lexer parseExpr
    return expr

parseLambda = do
    T.reservedOp lexer "\\"
    params <- many parseNamedParam
    T.reservedOp lexer "=>"
    body <- parseExpr
    return (Lambda params body)

parseFuncType = do
    T.reserved lexer "Pi"
    params <- many1 parseNamedParam
    T.reservedOp lexer "."
    typ <- parseExpr
    return (Pi params typ)

parseNamedParam =
    T.parens lexer parseNamedParam'

parseNamedParam' = do
    name <- T.identifier lexer
    T.reservedOp lexer ":"
    typ <- parseExpr
    return (name, typ)

parseApp = do
    expr1 <- parseExpr
    expr2 <- parseExpr
    return $ App expr1 expr2

--- Test code.

{-

doParse input = case parse parseProgram "(undefined)" input of
    Left err -> "Error:" ++ (show err)
    Right v -> (show v)

main = do 
    content <- getContents 
    putStrLn "Input: ===================="
    putStr content
    putStrLn "========================END"
    putStrLn $ doParse content 
    -}

parse = P.parse parseProgram 
