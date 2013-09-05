
module ParseTree (ParseTree(..), Program, Definition) where

    type Program = [ Definition ]
    type Definition = (String, ParseTree, ParseTree)
    data ParseTree = Name String
                   | Kind
                   | Type
                   | App ParseTree ParseTree
                   | Lambda String ParseTree ParseTree
                   | Pi String ParseTree ParseTree
                   deriving (Show)

    
