
module TypeCheck (type_check)
    where

import Control.Monad
import Debug.Trace
import AbstractTree

type_check :: Program -> Either String ()
type_check = type_check_prog [] []

type_check_prog :: [Expr] -> [Maybe Expr] -> Program -> Either String ()
type_check_prog _ _ [] = Right ()
type_check_prog gamma defs (top:prog) = do
    type_check_top gamma defs top
    type_check_prog (topType top : gamma) (topBody top:defs) prog

nth 0 (a:as) = Just a
nth _ [] = Nothing
nth n (_:as) = nth (n-1) as

expr_nf :: [Maybe Expr] -> Expr -> Expr
expr_nf defs x@(Variable _ i) = case nth i defs of
    Nothing -> x -- Can this only happen if this compiler is wrong?
    Just Nothing -> x
    Just (Just def) -> def

expr_nf _ Kind = Kind
expr_nf _ Type = Type
expr_nf defs (App e1 e2) =
    case expr_nf defs e1 of
      Lambda _ _ body -> expr_nf defs (substitute e2 body)
      e1_nf -> App e1_nf (expr_nf defs e2)
expr_nf defs (Lambda x typ body) = Lambda x (expr_nf defs' typ) (expr_nf defs' body)
    where defs' = inc_defs defs
expr_nf defs (Pi x typ1 typ2) = Pi x (expr_nf defs' typ1) (expr_nf defs' typ2)
    where defs' = inc_defs defs

        

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

inc_defs = map $ liftM inc_expr
dec_defs = map $ liftM dec_expr

type_check_top :: [Expr] -> [Maybe Expr] -> TopLevel -> Either String ()
type_check_top gamma defs ax@(Axiom {}) = do
    typ <- type_check_expr 0 gamma defs (axType ax)
    case typ of
        Kind -> Right ()
        Type -> Right ()
        _ -> Left "axiom error"

type_check_top gamma defs def@(Definition {}) = do
    sort <- type_check_expr 0 gamma defs (defType def)
    if sort `elem` [Type, Kind]
        then do
            derived_type <- type_check_expr 0 gamma defs (defBody def)
            if derived_type == (expr_nf defs (defType def))
                then Right ()
                else Left $ ("Definition of " ++ (defName def) ++ " should have type " ++
                        (show (expr_nf defs (defType def))) ++ ", infered to be " ++
                        (show derived_type))
        else Left $ "cant define value of type " ++ (show sort)

type_check_expr :: Int -> [Expr] -> [Maybe Expr] -> Expr -> Either String Expr

type_check_expr ind g _ e 
    | trace ("trace: " ++ (replicate (2*ind) ' ') ++ 
             (show g) ++ " |- " ++ (show e)) False = undefined

-- Axiom
type_check_expr _ [] _ Type = Right Kind

-- Start rule
type_check_expr ind (typeA:gamma) defs (Variable _ 0) = do
    s <- type_check_expr (ind + 1) gamma defs typeA
    if s `elem` [Type, Kind]
        then Right (expr_nf defs (inc_expr typeA))
        else Left "start rule "

-- Application rule
type_check_expr ind gamma defs (App f a) = do
    f_type <- type_check_expr (ind + 1) gamma defs f
    case f_type of
        (Pi _ typeA typeB) -> do
            typeA' <- type_check_expr (ind + 1) gamma defs a
            if typeA == typeA'
                then Right (substitute a typeB)
                else Left $ "In expression '" ++ (show $ App f a) ++ 
                        "', the function '" ++ (show f) ++ "' expects a '" ++
                        (show typeA) ++ "', but got a '" ++ (show typeA') ++ "'"
        _ -> Left "app not Pi type"  
    
-- Abstraction
type_check_expr ind gamma defs (Lambda x typeA b) = do
    typeB <- type_check_expr (ind + 1) (typeA:gamma) (inc_defs defs) b
    s <- type_check_expr (ind + 1) gamma defs (Pi x typeA typeB)
    if s `elem` [Type, Kind]
        then Right (expr_nf defs (Pi x typeA typeB))
        else Left "abstraction type not Kind or Type"

-- (s1, s2)-rule
type_check_expr ind gamma defs (Pi x typeA typeB) = do
    s1 <- type_check_expr (ind + 1) gamma defs typeA
    if s1 `elem` [Type, Kind]
        then (do s2 <- type_check_expr (ind + 1) (typeA:gamma) (inc_defs defs) typeB
                 if s2 `elem` [Type, Kind]
                    then Right s2
                    else Left $ "(s1, s2)-rule failed on s2, got" ++ (show s2))
        else Left $ "(s1, s2)-rule failed on s1, got " ++ (show s1)

-- Weakening
type_check_expr ind (typeC:gamma) defs typeA = do
    typeB <- type_check_expr (ind + 1) gamma (dec_defs defs) (dec_expr typeA)
    s <- type_check_expr (ind + 1) gamma defs typeC
    if s `elem` [Kind, Type]
        then Right (inc_expr typeB)
        else Left "weakening failed"

type_check_expr _ gamma _ e = Left $ "no rules applicable to " ++ 
    (show gamma) ++ " |- " ++ (show e)





