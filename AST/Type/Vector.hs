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

data VectorType = VectorTypeDouble
                | VectorTypeInteger
                | VectorTypeFloat
                | VectorTypeChar
                deriving (Show, Eq)

typeOfVector :: Vector -> VectorType
typeOfVector (VectorInteger vector) = VectorTypeInteger
typeOfVector (VectorDouble  vector) = VectorTypeDouble
typeOfVector (VectorFloat   vector) = VectorTypeFloat
typeOfVector (VectorChar    vector) = VectorTypeChar
