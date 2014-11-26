module AccType where

type LengthType = Int
type ElemListType   = [Double]

data ArrayT = ArrayT {
    length  :: LengthType,
    array   :: ElemListType
} deriving (Show)

data BinopT = Add | Sub | Mul | Div deriving (Show, Eq)

