
module TypeCheck (type_check, type_check_expr, substitute, expr_nf)
    where

import Control.Monad
import Debug.Trace
import AbstractTree

type Env = [Expr]
type Defs = [Maybe Expr]
type Result = Either String ()

type_ok :: Result
type_ok = Right ()

assert_Type_or_Kind :: Expr -> Expr -> String -> Result
assert_Type_or_Kind Kind _ _ = type_ok
assert_Type_or_Kind Type _ _ = type_ok
assert_Type_or_Kind typ expr msg = 
    Left $ unlines [ msg ++ ": the expression"
                   , "   " ++ (show expr)
                   , " is of sort"
                   , "   " ++ (show typ)
                   , " but should be of sort Type or Kind." ]

nth :: Int -> [a] -> Maybe a
nth 0 (a:as) = Just a
nth _ [] = Nothing
nth n (_:as) = nth (n-1) as


-- Reduce expression to normal form.
expr_nf :: Defs -> Expr -> Expr

expr_nf defs x@(Variable name i) = case nth i defs of
    Nothing -> error $ "expr_nf: " ++ (show i)  ++ name ++ " is free in " ++ (show defs)
    Just Nothing -> x
    Just (Just def) -> (inc_free_vars (i + 1) def)

expr_nf _ Kind = Kind

expr_nf _ Type = Type

expr_nf defs (App e1 e2) =
    case expr_nf defs e1 of
      Lambda _ _ body -> 
        expr_nf defs (substitute e2 body)
      e1_nf -> App e1_nf (expr_nf defs e2)

expr_nf defs (Lambda x typ body) = 
    Lambda x (expr_nf defs typ) (expr_nf (Nothing:defs) body)

expr_nf defs (Pi x typ1 typ2) =
    Pi x (expr_nf defs typ1) (expr_nf (Nothing:defs) typ2)

-- Apply function to variables (leaves), together with their depth.
traverse :: (Int -> String -> Int -> Expr) -> Int -> Expr -> Expr
traverse f n (Variable x i) = f n x i
traverse f n Kind = Kind
traverse f n Type = Type
traverse f n (App e1 e2) = App (traverse f n e1) (traverse f n e2)
traverse f n (Lambda x typ body) =
    Lambda x (traverse f n typ) (traverse f (n + 1) body)
traverse f n (Pi x typ body) = 
    Pi x (traverse f n typ) (traverse f (n + 1) body)

-- substitute e e' = substitute e for the first free variable in e'
substitute :: Expr -> Expr -> Expr
substitute e = traverse 
    (\ n x i -> 
        if n == i
        then (inc_free_vars n e)
        else (Variable x i)) 
    0

inc_free_vars :: Int -> Expr -> Expr
inc_free_vars inc = traverse (\ n x i -> Variable x (if (n <= i) then (i + inc) else i)) 0

dec_free_vars :: Int -> Expr -> Expr
dec_free_vars dec = traverse (\ n x i -> Variable x (if (n <= i) then (i - dec) else i)) 0

inc_free_vars1 :: Expr -> Expr
inc_free_vars1 = inc_free_vars 1

dec_free_vars1 :: Expr -> Expr
dec_free_vars1 = dec_free_vars 1

inc_defs :: [Maybe Expr] -> [Maybe Expr]
inc_defs = map $ liftM inc_free_vars1

dec_defs :: [Maybe Expr] -> [Maybe Expr]
dec_defs = map $ liftM dec_free_vars1

eq_beta :: Defs -> Expr -> Expr -> Bool
eq_beta defs e1 e2 = (expr_nf defs e1) == (expr_nf defs e2)

type_check :: Program -> Result
type_check = type_check_prog [] [] -- Empty environment and definitions.

type_check_prog :: Env -> Defs -> Program -> Result
type_check_prog _ _ [] = type_ok
type_check_prog gamma defs (top:prog) = do
    type_check_top gamma defs top
    type_check_prog ((expr_nf defs $ topType top):gamma) ((topBody top):defs) prog

type_check_top :: Env -> Defs -> TopLevel -> Result
type_check_top gamma defs ax@(Axiom {}) = do
    sort <- type_check_expr 0 gamma defs (axType ax)
    assert_Type_or_Kind sort (axType ax) "Type checking axiom"

type_check_top gamma defs def@(Definition {}) = do
    sort <- type_check_expr 0 gamma defs (defType def)
    assert_Type_or_Kind sort (defType def) "Type checking definition"
    derived_type <- type_check_expr 0 gamma defs (defBody def)
    if eq_beta defs derived_type (defType def)
       then type_ok
       else Left $ ("Definition of " ++ (defName def) ++ " should have type " ++
                (show (expr_nf defs (defType def))) ++ ", infered to be " ++
                (show derived_type))

type_check_expr :: Int -> Env -> Defs -> Expr -> Either String Expr

--type_check_expr ind g defs e 
--    | trace ("trace: " ++ (replicate (2*ind) ' ') ++ 
--             (show g) ++ ";" ++ (show defs) ++ " |- " ++ (show e)) False = undefined

-- Axiom
type_check_expr _ [] [] Type = return Kind

-- Start rule
type_check_expr ind (typeA:gamma) d@(_:defs) (Variable _ 0) = do
    sort <- type_check_expr (ind + 1) gamma defs typeA
    assert_Type_or_Kind sort typeA "start rule"
    return $ expr_nf d (inc_free_vars1 typeA)

-- Application rule
type_check_expr ind gamma defs (App f a) = do
    f_type <- type_check_expr (ind + 1) gamma defs f
    a_type <- type_check_expr (ind + 1) gamma defs a
    case f_type of
        (Pi _ a_type' fa_type) -> do
            if eq_beta defs a_type a_type'
                then return . (expr_nf defs) . (substitute a) . dec_free_vars1 $ fa_type
                else Left $ "In expression '" ++ (show $ App f a) ++ 
                        "', the function '" ++ (show f) ++ "' expects a '" ++
                        (show a_type') ++ "', but got a '" ++ (show a_type) ++ "'"
        _ -> Left $ unlines [ "The expression in function position "
                            , "  " ++ (show f)
                            , "is of non-applicable type "
                            , "  " ++ (show f_type) 
                            ]
    
-- Abstraction
type_check_expr ind gamma defs (Lambda x typeA body) = do
    body_type <- type_check_expr (ind + 1) (typeA:gamma) (Nothing:defs) body
    sort <- type_check_expr (ind + 1) gamma defs (Pi x typeA body_type)
    assert_Type_or_Kind sort(Pi x typeA body_type) "Abstraction"
    return (expr_nf defs (Pi x typeA body_type))

-- (s1, s2)-rule
type_check_expr ind gamma defs (Pi x typeA typeB) = do
    s1 <- type_check_expr (ind + 1) gamma defs typeA
    s2 <- type_check_expr (ind + 1) (typeA:gamma) (Nothing:defs) typeB
    assert_Type_or_Kind s1 typeA "s1 of (s1, s2)"
    assert_Type_or_Kind s2 typeB "s2 of (s1, s2)"
    return (expr_nf defs s2)

-- Weakening
type_check_expr ind (typeC:gamma) (_:defs) typeA = do
    typeB <- type_check_expr (ind + 1) gamma defs (dec_free_vars1 typeA)
    sort <- type_check_expr (ind + 1) gamma defs typeC
    assert_Type_or_Kind sort typeC "weakening"
    return (inc_free_vars1 typeB)

-- Catch errors.
type_check_expr _ gamma _ e = Left $ "no rules applicable to " ++ 
    (show gamma) ++ " |- " ++ (show e)

