module AST.SymbolTable where

data SymbolTable = 
        SymbolTable {
            prefix   :: String,
            nameList :: [String], 
            maxIndex :: Int
        } deriving (Show)

initSymTable :: String -> SymbolTable
initSymTable prefix = SymbolTable prefix [] 0

incSymTable :: SymbolTable -> SymbolTable
incSymTable symTable = 
    SymbolTable prefix' newList newMax
    where
        prefix' = prefix symTable
        newMax  = maxIndex symTable + 1
        newList = nameList symTable ++ [prefix' ++ (show newMax)]

topSymbol :: SymbolTable -> String
topSymbol symTable = last $ nameList symTable

