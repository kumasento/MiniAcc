module AST.Type.Scalar where

-- naming conventions:
--  Scalar[Integer | Double | Char | Float], the types in [] indicating
--  the type in OpenCL
--  All the underlying data type is Double(Haskell)

data Scalar = ScalarInteger Integer
            | ScalarDouble  Double
            | ScalarChar    Char
            | ScalarFloat   Float
            deriving (Show, Eq)

data ScalarType = ScalarTypeInteger 
                | ScalarTypeDouble
                | ScalarTypeChar
                | ScalarTypeFloat
                deriving (Show, Eq)

typeOfScalar :: Scalar -> ScalarType
typeOfScalar (ScalarInteger scalar)     = ScalarTypeInteger
typeOfScalar (ScalarDouble  scalar)     = ScalarTypeDouble
typeOfScalar (ScalarFloat   scalar)     = ScalarTypeFloat
typeOfScalar (ScalarChar    scalar)     = ScalarTypeChar
