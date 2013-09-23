
module Main(main) 
    where
import System.Exit(exitFailure, exitSuccess)

import qualified ParseTree as P
import qualified AbstractTree as A


parse_tree :: P.Program
parse_tree = [
     P.Axiom {P.axName="int", P.axType=P.Type}
    ,P.Definition {P.defName="bool"
                ,P.defType=P.Pi "A" P.Type P.Type
                ,P.defBody=P.Lambda "A" P.Type 
                    (P.Pi "x" (P.Name "A") 
                        (P.Pi "y" (P.Name "A") (P.Name "A")))
                }
    ,P.Definition {P.defName="True"
                ,P.defType=P.Pi "A" P.Type (P.App (P.Name "bool") (P.Name "A"))
                ,P.defBody=P.Lambda "A" P.Type
                            (P.Lambda "x" (P.Name "A")
                                (P.Lambda "y" (P.Name "A")
                                    (P.Name "x")))
                }
    ,P.Definition {P.defName="False"
                ,P.defType=P.Pi "A" P.Type (P.App (P.Name "bool") (P.Name "A"))
                ,P.defBody=P.Lambda "A" P.Type
                            (P.Lambda "x" (P.Name "A")
                                (P.Lambda "y" (P.Name "A")
                                    (P.Name "y")))
                }
    ,P.Definition {P.defName="And3"
                ,P.defType=P.Pi "A" P.Type (P.Pi "a" (P.App (P.Name "bool") (P.Name "A"))
                                        (P.Pi "b" (P.App (P.Name "bool") (P.Name "A"))
                                           (P.Pi "c" (P.App (P.Name "bool") (P.Name "A"))
                                              (P.App (P.Name "bool") (P.Name "A")))))
                ,P.defBody=P.Lambda "A" P.Type
                            (P.Lambda "a" (P.App (P.Name "bool") (P.Name "A"))
                                (P.Lambda "b" (P.App (P.Name "bool") (P.Name "A"))
                                    (P.Lambda "c" (P.App (P.Name "bool") (P.Name "A"))
                                         (P.App (P.App (P.Name "a") (P.App (P.App (P.Name "b")
                                                                        (P.Name "c"))
                                                                   (P.Name "False")))
                                              (P.Name "False")))))
                }
    ,P.Definition {P.defName="test"
                ,P.defType=P.Name "int"
                ,P.defBody=P.App (P.App (P.App (P.App (P.Name "And3") (P.Name "int"))
                                       (P.App (P.Name "True") (P.Name "int")))
                                  (P.App (P.Name "False") (P.Name "int")))
                             (P.App (P.Name "False") (P.Name "int"))
                }
    ]


expected_ast :: A.Program
expected_ast = [
     A.Axiom {A.axName="int", A.axType=A.Type}
    ,A.Definition {A.defName="bool"
                ,A.defType=A.Pi "A" A.Type A.Type
                ,A.defBody=A.Lambda "A" A.Type 
                    (A.Pi "x" (A.Variable "A" 0) 
                        (A.Pi "y" (A.Variable "A" 1) 
                            (A.Variable "A" 2)))
                }
    ,A.Definition {A.defName="True"
                ,A.defType=A.Pi "A" A.Type 
                                (A.App (A.Variable "bool" 1) 
                                       (A.Variable "A" 0))
                ,A.defBody=A.Lambda "A" A.Type
                            (A.Lambda "x" (A.Variable "A" 0)
                                (A.Lambda "y" (A.Variable "A" 1)
                                    (A.Variable "x" 1)))
                }
    ,A.Definition {A.defName="False"
                ,A.defType=A.Pi "A" A.Type 
                    (A.App (A.Variable "bool" 2) (A.Variable "A" 0))
                ,A.defBody=A.Lambda "A" A.Type
                            (A.Lambda "x" (A.Variable "A" 0)
                                (A.Lambda "y" (A.Variable "A" 1)
                                    (A.Variable "y" 0)))
                }
    ,A.Definition {A.defName="And3"
                ,A.defType=A.Pi "A" A.Type (A.Pi "a" (A.App (A.Variable "bool" 3) 
                                                            (A.Variable "A" 0))
                                        (A.Pi "b" (A.App (A.Variable "bool" 4) 
                                                         (A.Variable "A" 1))
                                           (A.Pi "c" (A.App (A.Variable "bool" 5) 
                                                            (A.Variable "A" 2))
                                              (A.App (A.Variable "bool" 6) 
                                                     (A.Variable "A" 3)))))
                ,A.defBody=A.Lambda "A" A.Type
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
                                              (A.Variable "False" 4)))))
                }
    ,A.Definition {A.defName="test"
                ,A.defType=A.Variable "int" 4
                ,A.defBody=A.App (A.App (A.App (A.App (A.Variable "And3" 0) 
                                                      (A.Variable "int" 4))
                                               (A.App (A.Variable "True" 2) 
                                                      (A.Variable "int" 4)))
                                  (A.App (A.Variable "False" 1) (A.Variable "int" 4)))
                             (A.App (A.Variable "False" 1) (A.Variable "int" 4))
                }
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

