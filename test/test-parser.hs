

module Main(main) 
    where
import System.Exit(exitFailure, exitSuccess)

import Parser
import ParseTree

program = unlines [
    "Axiom int : type.",
    "Define bool (A:type) : type := Pi (x:A) (y:A), A.",
    "Define True (A:type) : bool A := \\(x:A) (y:A) => x.",
    "Define False (A:type) : bool A := \\(x:A) (y:A) => y.",
    "Define And3 (A:type) (a:bool A) (b:bool A) (c: bool A) : bool A :=",
    "     a (b c False) False.",
    "Define test : int := And3 int (True int) (False int) (False int)."
    ]

    

expected_ast = [
     TopLevel "int" Type Nothing
    ,TopLevel "bool"
              (Pi "A" Type Type)
              (Just $Lambda "A" Type (Pi "x" (Name "A") (Pi "y" (Name "A") (Name "A"))))
    ,TopLevel "True"
                (Pi "A" Type (App (Name "bool") (Name "A")))
                (Just $ Lambda "A" Type
                            (Lambda "x" (Name "A")
                                (Lambda "y" (Name "A")
                                    (Name "x"))))
    ,TopLevel "False"
                (Pi "A" Type (App (Name "bool") (Name "A")))
                (Just $ Lambda "A" Type
                            (Lambda "x" (Name "A")
                                (Lambda "y" (Name "A")
                                    (Name "y"))))
    ,TopLevel "And3"
                (Pi "A" Type (Pi "a" (App (Name "bool") (Name "A"))
                                        (Pi "b" (App (Name "bool") (Name "A"))
                                           (Pi "c" (App (Name "bool") (Name "A"))
                                              (App (Name "bool") (Name "A"))))))
                (Just $ Lambda "A" Type
                            (Lambda "a" (App (Name "bool") (Name "A"))
                                (Lambda "b" (App (Name "bool") (Name "A"))
                                    (Lambda "c" (App (Name "bool") (Name "A"))
                                         (App (App (Name "a") (App (App (Name "b") 
                                                                        (Name "c")) 
                                                                   (Name "False")))
                                              (Name "False"))))))
    ,TopLevel "test"
                (Name "int")
                (Just $ App (App (App (App (Name "And3") (Name "int")) 
                                       (App (Name "True") (Name "int")))
                                  (App (Name "False") (Name "int")))
                             (App (Name "False") (Name "int")))
    ]



main = case parse "(const in test-parser.hs)" program of
       Left s -> do putStrLn $ "Error: " ++ (show s)
                    exitFailure
       Right ast -> if ast == expected_ast
                    then exitSuccess
                    else do putStrLn "Given ast:"
                            putStrLn (show ast)
                            putStrLn "does not match expected ast:"
                            putStrLn (show expected_ast)
                            exitFailure
    
