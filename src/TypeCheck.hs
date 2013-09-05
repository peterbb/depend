
module TypeCheck (type_check)
    where

import Debug.Trace
import AbstractTree

type_check :: Program -> Either String ()
type_check = type_check_prog []

type_check_prog :: [Expr] -> Program -> Either String ()
type_check_prog _ [] = Right ()
type_check_prog gamma (top:prog) = do
    type_check_top gamma top
    type_check_prog (topType top : gamma) prog

expr_nf :: Expr -> Expr
expr_nf x = x

-- First argument is to be inserted into the second argument.
-- Second argument is assumed to have at most one free variable,
-- and it's binding should be as if there were a binder around the 
-- whole expression.
substitute :: Expr -> Expr -> Expr
substitute = substitute' 0

substitute' i e x@(Variable _ j) 
    | (i == j) = e
    | True = x
substitute' _ _ Kind = Kind
substitute' _ _ Type = Type
substitute' i e (App e1 e2) = App (substitute' i e e1) (substitute' i e e2)
substitute' i e (Lambda x typ body) =
    Lambda x (substitute' i e typ) (substitute' (i + 1) (inc_expr e) body)
substitute' i e (Pi x typ body) =
    Pi x (substitute' i e typ) (substitute' (i + 1) (inc_expr e) body)


inc_expr (Variable x i) = Variable x (i + 1)
inc_expr Kind = Kind
inc_expr Type = Type
inc_expr (App e1 e2) = App (inc_expr e1) (inc_expr e2)
inc_expr (Lambda x typ body) = (Lambda x (inc_expr typ) (inc_expr body))
inc_expr (Pi x typ body) = (Pi x (inc_expr typ) (inc_expr body))

dec_expr (Variable x i) = Variable x (i - 1)
dec_expr Kind = Kind
dec_expr Type = Type
dec_expr (App e1 e2) = App (dec_expr e1) (dec_expr e2)
dec_expr (Lambda x typ body) = (Lambda x (dec_expr typ) (dec_expr body))
dec_expr (Pi x typ body) = (Pi x (dec_expr typ) (dec_expr body))

type_check_top :: [Expr] -> TopLevel -> Either String ()
type_check_top gamma ax@(Axiom {}) = do
    typ <- type_check_expr 0 gamma (axType ax)
    case typ of
        Kind -> Right ()
        Type -> Right ()
        _ -> Left "axiom error"

type_check_top gamma def@(Definition {}) = do
    sort <- type_check_expr 0 gamma (defType def)
    if sort `elem` [Type, Kind]
        then do
            derived_type <- type_check_expr 0 gamma (defBody def)
            if derived_type == (defType def)
                then Right ()
                else Left "definition"
        else Left $ "cant define value of type " ++ (show sort)

type_check_expr :: Int -> [Expr] -> Expr -> Either String Expr

type_check_expr ind g e 
    | trace ("trace: " ++ (replicate (2*ind) ' ') ++ 
             (show g) ++ " |- " ++ (show e)) False = undefined

-- Axiom
type_check_expr _ [] Type = Right Kind

-- Start rule
type_check_expr ind (typeA:gamma) (Variable _ 0) = do
    s <- type_check_expr (ind + 1) gamma typeA
    if s `elem` [Type, Kind]
        then Right (expr_nf (inc_expr typeA))
        else Left "start rule "

-- Application rule
type_check_expr ind gamma (App f a) = do
    f_type <- type_check_expr (ind + 1) gamma f
    case f_type of
        (Pi _ typeA typeB) -> do
            typeA' <- type_check_expr (ind + 1) gamma a
            if typeA == typeA'
                then Right (substitute a typeB)
                else Left "app - beta_eq wrong"
        _ -> Left "app not Pi type"  
    
-- Abstraction
type_check_expr ind gamma (Lambda x typeA b) = do
    typeB <- type_check_expr (ind + 1) (typeA:gamma) b
    s <- type_check_expr (ind + 1) gamma (Pi x typeA typeB)
    if s `elem` [Type, Kind]
        then Right (expr_nf (Pi x typeA typeB))
        else Left "abstraction type not Kind or Type"


-- (s1, s2)-rule
type_check_expr ind gamma (Pi x typeA typeB) = do
    s1 <- type_check_expr (ind + 1) gamma typeA
    if s1 `elem` [Type, Kind]
        then (do s2 <- type_check_expr (ind + 1) (typeA:gamma) typeB
                 if s2 `elem` [Type, Kind]
                    then Right s2
                    else Left $ "(s1, s2)-rule failed on s2, got" ++ (show s2))
        else Left $ "(s1, s2)-rule failed on s1, got " ++ (show s1)

-- Weakening
type_check_expr ind (typeC:gamma) typeA = do
    typeB <- type_check_expr (ind + 1) gamma (dec_expr typeA)
    s <- type_check_expr (ind + 1) gamma typeC
    if s `elem` [Kind, Type]
        then Right (inc_expr typeB)
        else Left "weakening failed"

type_check_expr _ gamma e = Left $ "no rules applicable to " ++ 
    (show gamma) ++ " |- " ++ (show e)





