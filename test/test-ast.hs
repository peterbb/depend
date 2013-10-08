
module Main(main) 
    where
import System.Exit(exitFailure, exitSuccess)

import qualified ParseTree as P
import qualified AbstractTree as A


parse_tree :: P.Program
parse_tree = [
     P.TopLevel "int" P.Type Nothing
    ,P.TopLevel "bool"
                (P.Pi "A" P.Type P.Type)
                (Just $ P.Lambda "A" P.Type 
                    (P.Pi "x" (P.Name "A") 
                        (P.Pi "y" (P.Name "A") (P.Name "A"))))
    ,P.TopLevel "True"
                (P.Pi "A" P.Type (P.App (P.Name "bool") (P.Name "A")))
                (Just $ P.Lambda "A" P.Type
                            (P.Lambda "x" (P.Name "A")
                                (P.Lambda "y" (P.Name "A")
                                    (P.Name "x"))))
    ,P.TopLevel "False"
                (P.Pi "A" P.Type (P.App (P.Name "bool") (P.Name "A")))
                (Just $ P.Lambda "A" P.Type
                            (P.Lambda "x" (P.Name "A")
                                (P.Lambda "y" (P.Name "A")
                                    (P.Name "y"))))
    ,P.TopLevel "And3"
                (P.Pi "A" P.Type (P.Pi "a" (P.App (P.Name "bool") (P.Name "A"))
                                        (P.Pi "b" (P.App (P.Name "bool") (P.Name "A"))
                                           (P.Pi "c" (P.App (P.Name "bool") (P.Name "A"))
                                              (P.App (P.Name "bool") (P.Name "A"))))))
                (Just $ P.Lambda "A" P.Type
                            (P.Lambda "a" (P.App (P.Name "bool") (P.Name "A"))
                                (P.Lambda "b" (P.App (P.Name "bool") (P.Name "A"))
                                    (P.Lambda "c" (P.App (P.Name "bool") (P.Name "A"))
                                         (P.App (P.App (P.Name "a") (P.App (P.App (P.Name "b")
                                                                        (P.Name "c"))
                                                                   (P.Name "False")))
                                              (P.Name "False"))))))
    ,P.TopLevel "test"
                (P.Name "int")
                (Just $ P.App (P.App (P.App (P.App (P.Name "And3") (P.Name "int"))
                                       (P.App (P.Name "True") (P.Name "int")))
                                  (P.App (P.Name "False") (P.Name "int")))
                             (P.App (P.Name "False") (P.Name "int")))
    ]


expected_ast :: A.Program
expected_ast = [
     A.TopLevel "int" A.Type Nothing
    ,A.TopLevel "bool"
                (A.Pi "A" A.Type A.Type)
                (Just $ A.Lambda "A" A.Type 
                    (A.Pi "x" (A.Variable "A" 0) 
                        (A.Pi "y" (A.Variable "A" 1) 
                            (A.Variable "A" 2))))
    ,A.TopLevel "True"
                (A.Pi "A" A.Type 
                                (A.App (A.Variable "bool" 1) 
                                       (A.Variable "A" 0)))
                (Just $ A.Lambda "A" A.Type
                            (A.Lambda "x" (A.Variable "A" 0)
                                (A.Lambda "y" (A.Variable "A" 1)
                                    (A.Variable "x" 1))))
    ,A.TopLevel "False"
                (A.Pi "A" A.Type 
                    (A.App (A.Variable "bool" 2) (A.Variable "A" 0)))
                (Just $ A.Lambda "A" A.Type
                            (A.Lambda "x" (A.Variable "A" 0)
                                (A.Lambda "y" (A.Variable "A" 1)
                                    (A.Variable "y" 0))))
    ,A.TopLevel "And3"
                (A.Pi "A" A.Type (A.Pi "a" (A.App (A.Variable "bool" 3) 
                                                            (A.Variable "A" 0))
                                        (A.Pi "b" (A.App (A.Variable "bool" 4) 
                                                         (A.Variable "A" 1))
                                           (A.Pi "c" (A.App (A.Variable "bool" 5) 
                                                            (A.Variable "A" 2))
                                              (A.App (A.Variable "bool" 6) 
                                                     (A.Variable "A" 3))))))
                (Just $ A.Lambda "A" A.Type
                            (A.Lambda "a" (A.App (A.Variable "bool" 3) 
                                                 (A.Variable "A" 0))
                                (A.Lambda "b" (A.App (A.Variable "bool" 4) 
                                                     (A.Variable "A" 1))
                                    (A.Lambda "c" (A.App (A.Variable "bool" 5) 
                                                         (A.Variable "A" 2))
                                         (A.App (A.App (A.Variable "a" 2) 
                                                        (A.App (A.App (A.Variable "b" 1)
                                                                      (A.Variable "c" 0))
                                                                (A.Variable "False" 4)))
                                              (A.Variable "False" 4))))))
    ,A.TopLevel "test"
                (A.Variable "int" 4)
                (Just $ A.App (A.App (A.App (A.App (A.Variable "And3" 0) 
                                                      (A.Variable "int" 4))
                                               (A.App (A.Variable "True" 2) 
                                                      (A.Variable "int" 4)))
                                  (A.App (A.Variable "False" 1) (A.Variable "int" 4)))
                             (A.App (A.Variable "False" 1) (A.Variable "int" 4)))
    ]


main = do
    case A.translate parse_tree of
        Left s -> do putStrLn (show s)
                     exitFailure
        Right ast -> if ast == expected_ast
                     then exitSuccess
                     else do putStrLn "Got ast:"
                             putStrLn (show ast)
                             putStrLn "Expected ast:"
                             putStrLn (show expected_ast)
                             exitFailure

