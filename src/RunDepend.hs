

module Main (main) 
    where
import System.Environment

import Parser
import ParseTree
import AbstractTree
import TypeCheck



report s = do
    putStrLn "INPUT================"
    putStrLn s
    putStrLn "===========END INPUT"

parse' text = case parse "(stdin)" text of
    Left s -> error $ "parsing: " ++ (show s)
    Right tree -> tree

translate' tree = case translate tree of
    Left s -> error $ "translating to ast:" ++ (show s)
    Right ast -> ast

type_check' ast = case type_check ast of
    Left s -> error $ "type check: " ++ s
    Right () -> putStrLn "Success!"
    

check :: String -> IO ()
check text =
    let syntax_tree = parse' text
        ast = translate' syntax_tree 
        in type_check' ast

main = do
    putStrLn "The Depend Type Checker."
    (name:[]) <- getArgs
    prog <- readFile name
    report prog
    check prog


