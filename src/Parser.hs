module Parser (parse) where

import Debug.Trace
import ParseTree
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Language as L
import qualified Text.Parsec.Token as T

-- Only exported program.
-- parse source-code file-info
parse :: String -> String -> Either ParseError Program
parse = P.parse parseProgram 

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
        , defType = foldr (uncurry Pi) typ params
        , defBody = foldr (uncurry Lambda) body params
        }

parseExpr :: Parser ParseTree
parseExpr = do
    exprs <- many1 simpleExpr
    return $ foldl1 App exprs

simpleExpr = parseType <|> parseName <|> parseParExpr <|> 
             parseLambda <|> parseFuncType <?> "expression"

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
