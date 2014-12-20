module Codegen.GenTerm where

import AST.Type.Vector
import AST.Type.Scalar
import Codegen.Const

import Data.List as List

vectorTypeStr :: Vector -> String
vectorTypeStr (VectorDouble vec)    = "double*"
vectorTypeStr (VectorInteger vec)   = "int*"
vectorTypeStr (VectorChar vec)      = "char*"
vectorTypeStr (VectorFloat vec)     = "float*"

genCodeVector :: Vector -> String -> String
genCodeVector vector nameStr =
    List.intercalate " "
        [   vectorTypeStr vector,
            nameStr,
            equalOpStr,
            genFunction readFileStr [nameStr],
            semiOpStr]

scalarTypeStr :: Scalar -> String
scalarTypeStr (ScalarInteger scalar)    = intTypeStr
scalarTypeStr (ScalarDouble scalar)     = doubleTypeStr
scalarTypeStr (ScalarFloat scalar)      = floatTypeStr
scalarTypeStr (ScalarChar scalar)       = charTypeStr

-- how to write this cleaner?
scalarToStr :: Scalar -> String
scalarToStr (ScalarInteger scalar)    = show scalar
scalarToStr (ScalarDouble scalar)     = show scalar
scalarToStr (ScalarFloat scalar)      = show scalar
scalarToStr (ScalarChar scalar)       = show scalar

genCodeScalar :: Scalar -> String -> String
genCodeScalar scalar nameStr =
    List.intercalate " " 
        [   scalarTypeStr scalar,
            nameStr, 
            equalOpStr, 
            scalarToStr scalar, 
            semiOpStr]
             
