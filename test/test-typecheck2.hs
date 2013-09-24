module Main
where
import System.Exit(exitSuccess, exitFailure)

import AbstractTree
import TypeCheck


ast1 = [
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
    ,Definition {defName="And2"
                ,defType=Pi "A" Type
                          (Pi "a" (App (Variable "bool" 3) (Variable "A" 0))
                            (Pi "b" (App (Variable "bool" 4) (Variable "A" 1))
                                (App (Variable "bool" 5) (Variable "A" 2))))
                ,defBody=Lambda "A" Type
                            (Lambda "P" (App (Variable "bool" 2) (Variable "A" 0))
                                (Lambda "Q" (App (Variable "bool" 3) (Variable "A" 1))
                                    (Lambda "a" (Variable "A" 2)
                                        (Lambda "b" (Variable "A" 3)
                                            (App (App (Variable "P" 3)
                                                      (App (App (Variable "Q" 2)
                                                                (Variable "a" 1))
                                                           (Variable "b" 0)))
                                                 (Variable "b" 0)))))) }
    ]

main = do 
    case type_check ast1 of
        Left s -> do putStrLn $ "Error: " ++ s
                     exitFailure
        Right () -> exitSuccess
    

