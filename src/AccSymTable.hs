module AccSymTable where

-- import AccType
import Data.Map as Map

type SymName        = String
type SymId          = Int

type SymTable       = Map.Map SymId SymName

-- insert var and return id
insertSymTable :: SymTable -> (SymId, SymName, SymTable) 
insertSymTable symTable =
    (id, name, newSymTable)
    where
        id   = 1 + (Map.size symTable)
        name = "Var_" ++ (show id)
        newSymTable = Map.insert id name symTable


