module AccType where

data ArrayT = ArrayT {
    length  :: Int
} deriving (Show)

data BinopT = Add | Sub | Mul | Div deriving (Show)

