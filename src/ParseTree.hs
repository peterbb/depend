module ParseTree (ParseTree(..), Program, TopLevel(..))
    where

type Program = [ TopLevel ]

data TopLevel = Definition { defName :: String
                           , defType :: ParseTree
                           , defBody :: ParseTree
                           }
              | Axiom { axName :: String
                      , axType :: ParseTree
                      }

data ParseTree = Name String
               | Kind
               | Type
               | App ParseTree ParseTree
               | Lambda String ParseTree ParseTree
               | Pi String ParseTree ParseTree
               deriving (Show)

    
