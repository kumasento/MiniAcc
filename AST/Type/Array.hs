module AST.Type.Array where

import AST.Type.Vector
import AST.Type.Scalar

data Array = ArrDIM0 { length::Int, array::Vector } 
                deriving (Show)
