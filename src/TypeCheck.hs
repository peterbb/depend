
module TypeCheck (type_check)
    where

import Control.Monad
import Debug.Trace
import AbstractTree

type Env = [Expr]
type Defs = [Maybe Expr]
type Result = Either String ()

type_ok :: Result
type_ok = Right ()

assert_Type_or_Kind :: Expr -> String -> Result
assert_Type_or_Kind Kind _ = type_ok
assert_Type_or_Kind Type _ = type_ok
assert_Type_or_Kind _ s = Left s

eq_beta :: Defs -> Expr -> Expr -> Bool
eq_beta defs e1 e2 = (expr_nf defs e1) == (expr_nf defs e2)

type_check :: Program -> Result
type_check = type_check_prog [] []

type_check_prog :: Env -> Defs -> Program -> Result
type_check_prog _ _ [] = type_ok
type_check_prog gamma defs (top:prog) = do
    type_check_top gamma defs top
    type_check_prog ((topType top):gamma) ((topBody top):defs) prog

type_check_top :: Env -> Defs -> TopLevel -> Result
type_check_top gamma defs ax@(Axiom {}) = do
    typ <- type_check_expr 0 gamma defs (axType ax)
    assert_Type_or_Kind typ "Type checking axiom"

type_check_top gamma defs def@(Definition {}) = do
    sort <- type_check_expr 0 gamma defs (defType def)
    assert_Type_or_Kind sort "Type checking definition"
    derived_type <- type_check_expr 0 gamma defs (defBody def)
    if eq_beta defs derived_type (defType def)
       then type_ok
       else Left $ ("Definition of " ++ (defName def) ++ " should have type " ++
                (show (expr_nf defs (defType def))) ++ ", infered to be " ++
                (show derived_type))


return_type :: Expr -> Either String Expr
return_type = Right

type_check_expr :: Int -> Env -> Defs -> Expr -> Either String Expr

type_check_expr ind g _ e 
    | trace ("trace: " ++ (replicate (2*ind) ' ') ++ 
             (show g) ++ " |- " ++ (show e)) False = undefined

-- Axiom
type_check_expr _ [] [] Type = Right Kind

-- Start rule
type_check_expr ind (typeA:gamma) (_:defs) (Variable _ 0) = do
    sort <- type_check_expr (ind + 1) gamma defs typeA
    assert_Type_or_Kind sort "start rule"
    return_type (expr_nf defs (inc_expr typeA))

-- Application rule
type_check_expr ind gamma defs (App f a) = do
    f_type <- type_check_expr (ind + 1) gamma defs f
    case f_type of
        (Pi _ typeA typeB) -> do
            typeA' <- type_check_expr (ind + 1) gamma defs a
            if typeA == typeA'
                then Right (expr_nf defs (substitute a typeB))
                else Left $ "In expression '" ++ (show $ App f a) ++ 
                        "', the function '" ++ (show f) ++ "' expects a '" ++
                        (show typeA) ++ "', but got a '" ++ (show typeA') ++ "'"
        _ -> Left "app not Pi type"  
    
-- Abstraction
type_check_expr ind gamma defs (Lambda x typeA b) = do
    typeB <- type_check_expr (ind + 1) (typeA:gamma) (inc_defs defs) b
    sort <- type_check_expr (ind + 1) gamma defs (Pi x typeA typeB)
    assert_Type_or_Kind sort "Abstraction"
    return_type (expr_nf defs (Pi x typeA typeB))

-- (s1, s2)-rule
type_check_expr ind gamma defs (Pi x typeA typeB) = do
    s1 <- type_check_expr (ind + 1) gamma defs typeA
    s2 <- type_check_expr (ind + 1) (typeA:gamma) (inc_defs defs) typeB
    assert_Type_or_Kind s1 "(s1, s2)-rule for s1"
    assert_Type_or_Kind s2 "(s1, s2)-rule for s2"
    return_type s2

-- Weakening
type_check_expr ind (typeC:gamma) defs typeA = do
    typeB <- type_check_expr (ind + 1) gamma (dec_defs defs) (dec_expr typeA)
    s <- type_check_expr (ind + 1) gamma defs typeC
    assert_Type_or_Kind s "weakening"
    return_type (inc_expr typeB)

-- Catch errors.
type_check_expr _ gamma _ e = Left $ "no rules applicable to " ++ 
    (show gamma) ++ " |- " ++ (show e)


-- Aux functions:
nth 0 (a:as) = Just a
nth _ [] = Nothing
nth n (_:as) = nth (n-1) as


expr_nf :: Defns -> Expr -> Expr
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


traverse :: (Int -> String -> Int -> Expr) -> Int -> Expr -> Expr
traverse f n (Variable x i) = f n x i
traverse f n Kind = Kind
traverse f n Type = Type
traverse f n (App e1 e2) = App (traverse f n e1) (traverse f n e2)
traverse f n (Lambda x typ body) =
    Lambda x (traverse f n typ) (traverse f (n + 1) body)
traverse f n (Pi x typ body) = 
    Pi x (traverse f n typ) (traverse f (n + 1) body)

substitute e = traverse (\ n x i -> if (n == i) then e else (Variable x i)) 0
inc_expr = traverse (\ n x i -> Variable x (if (n >= i) then (i + 1) else i)) 0
dec_expr = traverse (\ n x i -> Variable x (if (n >= i) then (i - 1) else i)) 0

inc_defs = map $ liftM inc_expr
dec_defs = map $ liftM dec_expr

