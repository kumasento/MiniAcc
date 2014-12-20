module AST.Type.Vector where

-- Vector Type: THIS VECTOR IS NOT AN ARRAY !!!
--

data Vector = VectorDouble  [Double]
            | VectorInteger [Int]
            | VectorFloat   [Float]
            | VectorChar    [Char]
            | VectorNull
            deriving (Show, Eq)

nullVector = VectorNull :: Vector

