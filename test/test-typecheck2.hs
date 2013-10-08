module Main
where
import System.Exit(exitSuccess, exitFailure)

import AbstractTree
import TypeCheck


ast1 = [
     TopLevel "int" Type Nothing
    ,TopLevel "bool"
              (Pi "A" Type Type)
              (Just $ Lambda "A" Type 
                   (Pi "x" (Variable "A" 0) 
                       (Pi "y" (Variable "A" 1) 
                           (Variable "A" 2))) )
    ,TopLevel "True"
              (Pi "A" Type 
                  (App (Variable "bool" 1) 
                       (Variable "A" 0)))
              (Just $ Lambda "A" Type
                            (Lambda "x" (Variable "A" 0)
                                (Lambda "y" (Variable "A" 1)
                                    (Variable "x" 1))))
    ,TopLevel "False"
              (Pi "A" Type 
                    (App (Variable "bool" 2) (Variable "A" 0)))
              (Just $ Lambda "A" Type
                            (Lambda "x" (Variable "A" 0)
                                (Lambda "y" (Variable "A" 1)
                                    (Variable "y" 0))))
    ,TopLevel "And2"
              (Pi "A" Type
                          (Pi "a" (App (Variable "bool" 3) (Variable "A" 0))
                            (Pi "b" (App (Variable "bool" 4) (Variable "A" 1))
                                (App (Variable "bool" 5) (Variable "A" 2)))))
              (Just $ Lambda "A" Type
                            (Lambda "P" (App (Variable "bool" 3) (Variable "A" 0))
                                (Lambda "Q" (App (Variable "bool" 4) (Variable "A" 1))
                                    (Lambda "a" (Variable "A" 2)
                                        (Lambda "b" (Variable "A" 3)
                                            (App (App (Variable "P" 3)
                                                      (App (App (Variable "Q" 2)
                                                                (Variable "a" 1))
                                                           (Variable "b" 0)))
                                                 (Variable "b" 0)))))))
    ]

main = do 
    case type_check ast1 of
        Left s -> do putStrLn $ "Error: " ++ s
                     exitFailure
        Right () -> exitSuccess
    

