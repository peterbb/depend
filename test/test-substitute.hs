module Main
where

import System.Exit (exitFailure, exitSuccess)

import AbstractTree
import TypeCheck



check subexpr expr expected = 
    let e = substitute subexpr expr
        in if e == expected
             then return ()
             else do putStrLn $ "got " ++ (show e) ++ " expected " ++ (show expected)
                     exitFailure

check_id e = check (Variable "xx" (-111)) e e


main = do
    check Type (Variable "n" 0) Type
    check_id $ Lambda "x" Type (App (Variable "x" 0) (Variable "x" 0))

    check (Variable "n" 0) (Lambda "x" Type (Variable "z" 1))
            (Lambda "x" Type (Variable "n" 1))

    let id = (Lambda "x" Type (Variable "x" 0))
        in check id
                 (Pi "x" (Variable "id" 0)
                    (App (Variable "id" 1)
                         (Lambda "z" Type
                             (Variable "id" 2))))
                 (Pi "x" id
                    (App id (Lambda "z" Type id)))

    let f e = (Lambda "z" Type (Lambda "y" Type e))
        in check (Variable "x" 2) 
                 (f (Variable "z" 2))
                 (f (Variable "x" 4))

    exitSuccess

