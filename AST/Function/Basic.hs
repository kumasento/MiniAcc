module AST.Function.Basic where

data ZipWith    = ZipWith2 
                | ZipWith3
                deriving (Show)
data Basic  = BasicZipWith ZipWith
            deriving (Show)
