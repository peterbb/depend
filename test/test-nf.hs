
module Main(main) 
    where
import System.Exit(exitFailure, exitSuccess)

import AbstractTree
import TypeCheck



test :: [Maybe Expr] -> Expr -> Expr -> IO ()
test defs expr expected =
    let result = expr_nf defs expr
        in if result == expected
           then return ()
           else do putStrLn $ "got " ++ (show result) ++ 
                    " expected " ++ (show expected)

main = do
    test [Nothing]  (Variable "x" 0) (Variable "x" 0)

    let f = (Lambda "x" Type (Variable "x" 0))
        in test [] f f

    test [Just (Variable "x" 0), Just Type]
         (Lambda "y" Type (Variable "z" 1))
         (Lambda "y" Type (Variable "x" 2))

    test [Nothing]
         (App (Lambda "x" Type (Variable "x" 0))
              (Variable "y" 0))
         (Variable "y" 0)

    exitSuccess

