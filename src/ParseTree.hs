
module ParseTree (ParseTree(..), Program, Definition) where

    type Program = [ Definition ]
    type Definition = (String, ParseTree, ParseTree)
    type ParamList = [ (String, ParseTree) ]
    data ParseTree = Name String
                   | Kind
                   | Type
                   | App ParseTree ParseTree
                   | Lambda ParamList ParseTree
                   | Pi ParamList ParseTree
                   deriving (Show)

    
