module AST.Function.Operator where

data Binop  = BinopAdd
            | BinopSub
            | BinopMul
            | BinopDiv
            deriving (Show)

data Operator = BinopOperator Binop
                deriving (Show)
