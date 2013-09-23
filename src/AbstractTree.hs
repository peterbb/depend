{-# OPTIONS_GHC -XTemplateHaskell #-}

module AbstractTree (translate, Program, 
        TopLevel(..), Expr(..), topType, topBody) 
    where

import Data.List
import Control.Monad.Error
import Control.Monad.Identity

import qualified ParseTree as P

type Program = [ TopLevel ]

data TopLevel = Definition 
                    { defName :: String
                    , defType :: Expr
                    , defBody :: Expr
                    }
              | Axiom 
                    { axName :: String
                    , axType :: Expr
                    }
    deriving (Show, Eq)

topName (Definition {defName=name}) = name
topName (Axiom {axName=name}) = name
topType (Definition {defType=typ}) = typ
topType (Axiom {axType=typ}) = typ
topBody (Definition {defBody=body}) = Just body
topBody (Axiom {}) = Nothing

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
          
translate_top gamma (P.Definition {P.defName=name, P.defType=typ, P.defBody=body}) = do 
    ast_typ <- translate_expr gamma typ
    ast_body <- translate_expr gamma body
    return $ Definition 
        { defName = name
        , defType = ast_typ
        , defBody = ast_body
        }

translate_top gamma (P.Axiom {P.axName=name, P.axType=typ}) = do
    ast_type <- translate_expr gamma typ
    return $ Axiom {axName=name, axType=ast_type}

translate_expr :: [String] -> P.ParseTree -> TranslateResult Expr
translate_expr gamma (P.Name name) = 
    case elemIndex name gamma of
        Just i -> return $ Variable name i
        Nothing -> throwError (TranslateError $ "Variable " ++ name ++ " not bound in " ++ (show gamma))
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


