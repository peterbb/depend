module Main
where
import System.Exit(exitSuccess, exitFailure)

import AbstractTree
import TypeCheck


ast = [
     Axiom {axName="int", axType=Type}
    ,Definition {defName="bool"
                ,defType=Pi "A" Type Type
                ,defBody=Lambda "A" Type 
                    (Pi "x" (Variable "A" 0) 
                        (Pi "y" (Variable "A" 1) 
                            (Variable "A" 2)))
                }
    ,Definition {defName="True"
                ,defType=Pi "A" Type 
                                (App (Variable "bool" 1) 
                                       (Variable "A" 0))
                ,defBody=Lambda "A" Type
                            (Lambda "x" (Variable "A" 0)
                                (Lambda "y" (Variable "A" 1)
                                    (Variable "x" 1)))
                }
    ,Definition {defName="False"
                ,defType=Pi "A" Type 
                    (App (Variable "bool" 2) (Variable "A" 0))
                ,defBody=Lambda "A" Type
                            (Lambda "x" (Variable "A" 0)
                                (Lambda "y" (Variable "A" 1)
                                    (Variable "y" 0)))
                }
    ,Definition {defName="And3"
                ,defType=Pi "A" Type (Pi "a" (App (Variable "bool" 3) 
                                                            (Variable "A" 0))
                                        (Pi "b" (App (Variable "bool" 4) 
                                                         (Variable "A" 1))
                                           (Pi "c" (App (Variable "bool" 5) 
                                                            (Variable "A" 2))
                                              (App (Variable "bool" 6) 
                                                     (Variable "A" 3)))))
                ,defBody=Lambda "A" Type
                            (Lambda "a" (App (Variable "bool" 3) 
                                                 (Variable "A" 0))
                                (Lambda "b" (App (Variable "bool" 4) 
                                                     (Variable "A" 1))
                                    (Lambda "c" (App (Variable "bool" 5) 
                                                         (Variable "A" 2))
                                         (App (App (Variable "a" 2) 
                                                        (App (App (Variable "b" 1)
                                                                      (Variable "c" 0))
                                                                (Variable "False" 4)))
                                              (Variable "False" 4)))))
                }
    ,Definition {defName="test"
                ,defType=Variable "int" 4
                ,defBody=App (App (App (App (Variable "And3" 0) 
                                                      (Variable "int" 4))
                                               (App (Variable "True" 2) 
                                                      (Variable "int" 4)))
                                  (App (Variable "False" 1) (Variable "int" 4)))
                             (App (Variable "False" 1) (Variable "int" 4))
                }
    ]





main = case type_check ast of
        Left s -> do putStrLn $ "Error: " ++ s
                     exitFailure
        Right () -> exitSuccess

