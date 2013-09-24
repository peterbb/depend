module Main
where
import System.Exit(exitSuccess, exitFailure)

import AbstractTree
import TypeCheck

main = do
    mapM test test_cases
    exitSuccess

test :: (String, [Expr], [Maybe Expr], Expr, Expr) -> IO ()
test (name, g, d, ast, expected_type) = case type_check_expr 0 g d ast of
    Left _ -> do putStrLn $ name ++ ": " ++(show ast) ++ " has no type."
                 exitFailure
    Right typ -> if typ == expected_type
                 then return ()
                 else do putStrLn $ name ++ ": got type " ++ (show typ) ++
                                    " expected " ++ (show expected_type)
                         exitFailure

test_cases = [ ("first", [], [], ast1, expected1) 
             , ("second", env2, def2, ast2, expected2)
             ]

ast1 = Lambda "A" Type
         (Lambda "x" (Variable "A" 0)
             (Lambda "y" (Variable "A" 1)
                 (Variable "x" 1)))

expected1 = Pi "A" Type 
            (Pi "x" (Variable "A" 0)
                (Pi "y" (Variable "A" 1)
                    (Variable "A" 2)))

ast2 = Pi "A" Type
        (App (Variable "bool" 1)
             (Variable "A" 0))
expected2 = Type

env2 = [ Pi "A" Type Type ]
def2 = [ Just (Lambda "A" Type
                (Pi "x" (Variable "A" 0)
                    (Pi "y" (Variable "A" 1)
                        (Variable "A" 2)))) ]

    

