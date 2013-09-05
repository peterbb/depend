module Parser (parse) where

import Debug.Trace
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
    , L.opStart = oneOf ":.\\=,>"
    , L.opLetter = oneOf ":.\\=,>"
    , L.reservedOpNames = [":", ":=", ".", "\\", "=>", ","]
    , L.reservedNames = [ "Axiom", "Define", "type", "kind", "Pi"]
    }

--- Parser
parseProgram :: Parser Program
parseProgram = do
    T.whiteSpace lexer
    defs <- (many parseToplevel)
    eof
    return defs

parseToplevel :: Parser TopLevel
parseToplevel = parseDefinition <|> parseAxiom

parseAxiom :: Parser TopLevel
parseAxiom = do
    T.reserved lexer "Axiom"
    name <- T.identifier lexer
    T.reservedOp lexer ":"
    typ <- parseExpr
    T.reservedOp lexer "."
    return $ Axiom { axName = name, axType = typ }

parseDefinition :: Parser TopLevel
parseDefinition = do
    T.reserved lexer "Define"
    name <- T.identifier lexer
    params <- many parseNamedParam
    T.reservedOp lexer ":"
    typ <- parseExpr
    T.reservedOp lexer ":="
    body <- parseExpr
    T.reservedOp lexer "."
    return $ Definition 
        { defName = name
        , defType = foldr (uncurry Lambda) typ params
        , defBody = body
        }

parseExpr :: Parser ParseTree
parseExpr = try complexExpr <|> simpleExpr

simpleExpr = parseConst <|> parseName <|> parseParExpr <|> 
        parseLambda <|> parseFuncType <?> "expression"

complexExpr = do
    expr1 <- simpleExpr
    expr2 <- parseExpr
    return $ App expr1 expr2


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
    T.whiteSpace lexer
    char '('
    T.whiteSpace lexer
    expr <- parseExpr
    T.whiteSpace lexer
    char ')'
    T.whiteSpace lexer
    return expr

parseLambda = do
    T.reservedOp lexer "\\"
    params <- many parseNamedParam
    T.reservedOp lexer "=>"
    body <- parseExpr
    return $ foldr (uncurry Lambda) body params

parseFuncType = do
    T.reserved lexer "Pi"
    params <- many1 parseNamedParam
    T.reservedOp lexer ","
    typ <- parseExpr
    return $ foldr (uncurry Pi) typ params

parseNamedParam = do
    T.parens lexer parseNamedParam'

parseNamedParam' = do
    name <- T.identifier lexer
    T.reservedOp lexer ":"
    typ <- parseExpr
    return (name, typ)

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
