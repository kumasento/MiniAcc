type SymbolTable = [(String, Operand)]

data CodegenState 
    = CodegenState {
        currentBlock    :: Name ,
        blocks          :: Map.Map Name BlockState,
        symtab          :: SymbolTable
    } deriving (Show)
