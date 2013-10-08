module ParseTree (ParseTree(..), Program, TopLevel(..))
    where

type Program = [ TopLevel ]

data TopLevel = TopLevel { topName :: String
                         , topType :: ParseTree
                         , topBody :: Maybe ParseTree
                          }
    deriving (Show, Eq)

data ParseTree = Name String
               | Kind
               | Type
               | App ParseTree ParseTree
               | Lambda String ParseTree ParseTree
               | Pi String ParseTree ParseTree
               deriving (Show, Eq)

    
