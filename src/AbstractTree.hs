module AbstractTree (translate, Program, TopLevel(..), Expr(..)) where

import Data.List
import Data.Traversable
import Control.Monad.Error
import Control.Monad.Identity

import qualified ParseTree as P

type Program = [ TopLevel ]

data TopLevel = TopLevel { topName :: String
                         , topType :: Expr
                         , topDef  :: Maybe Expr
                         }
    deriving (Show, Eq)

data Expr = 
      Variable String Int
    | Kind
    | Type
    | App Expr Expr
    | Lambda String Expr Expr
    | Pi String Expr Expr
    deriving (Show, Eq)

data TranslateError = TranslateError String

instance Error TranslateError where
    noMsg = TranslateError "Expression not well formed."
    strMsg s = TranslateError $ "Error:" ++ s

instance Show TranslateError where
    show (TranslateError s) = s


type TranslateResult = Either TranslateError

translate :: P.Program -> TranslateResult Program
translate = translate_prog []

translate_prog :: [String] -> P.Program -> TranslateResult Program
translate_prog gamma [] = return []
translate_prog gamma (top:prog) = do
    ast_top <- translate_top gamma top
    ast_prog <- translate_prog (topName ast_top:gamma) prog
    return $ ast_top:ast_prog 

translate_top :: [String] -> P.TopLevel -> TranslateResult TopLevel
translate_top gamma (P.TopLevel name typ m_body) = do
    ast_typ <- translate_expr gamma typ
    ast_body <- traverse (translate_expr gamma) m_body
    return $ TopLevel name ast_typ ast_body

translate_expr :: [String] -> P.ParseTree -> TranslateResult Expr
translate_expr gamma (P.Name name) = 
    case elemIndex name gamma of
        Just i -> return $ Variable name i
        Nothing -> 
            throwError $ TranslateError $ "Variable " ++ name ++ " not bound in " ++ show gamma
translate_expr _ P.Kind = return Kind
translate_expr _ P.Type = return Type
translate_expr gamma (P.App expr1 expr2) = do
    ast_expr1 <- translate_expr gamma expr1
    ast_expr2 <- translate_expr gamma expr2
    return $ App ast_expr1 ast_expr2
translate_expr gamma (P.Lambda name typ body) = do
    ast_typ <- translate_expr gamma typ
    ast_body <- translate_expr (name:gamma) body
    return $ Lambda name ast_typ ast_body
translate_expr gamma (P.Pi name typ body) = do
    ast_typ <- translate_expr gamma typ
    ast_body <- translate_expr (name:gamma) body
    return $ Pi name ast_typ ast_body


