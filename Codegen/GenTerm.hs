module Codegen.GenTerm where

import AST.Node
import AST.Type.Scalar
import AST.Type.Vector
import Codegen.Const

import Data.List as List

vectorTypeToStr :: VectorType -> String
vectorTypeToStr vectorType =
    case vectorType of 
        VectorTypeInteger    -> vecIntegerTypeStr
        VectorTypeDouble     -> vecDoubleTypeStr
        VectorTypeFloat      -> vecFloatTypeStr
        VectorTypeChar       -> vecCharTypeStr

vectorTypeStr :: Vector -> String
vectorTypeStr vector = vectorTypeToStr $ typeOfVector vector

genCodeVector :: Vector -> String -> [String]
genCodeVector vector nameStr =
    [List.intercalate " "
        [ vectorTypeStr vector
        , nameStr
        , equalOpStr
        , genFunction readFileStr [nameStr]]
         ++ semiOpStr]

scalarTypeToStr :: ScalarType -> String 
scalarTypeToStr scalarType = 
    case scalarType of
        ScalarTypeInteger   -> intTypeStr
        ScalarTypeDouble    -> doubleTypeStr
        ScalarTypeFloat     -> floatTypeStr
        ScalarTypeChar      -> charTypeStr

scalarTypeStr :: Scalar -> String
scalarTypeStr scalar = scalarTypeToStr $ typeOfScalar scalar

scalarToStr :: Scalar -> String
scalarToStr (ScalarInteger scalar)    = show scalar
scalarToStr (ScalarDouble scalar)     = show scalar
scalarToStr (ScalarFloat scalar)      = show scalar
scalarToStr (ScalarChar scalar)       = show scalar

genCodeScalar :: Scalar -> String -> [String]
genCodeScalar scalar nameStr =
    [List.intercalate " " 
        [ scalarTypeStr scalar
        , nameStr
        , equalOpStr 
        , scalarToStr scalar]
          ++ semiOpStr]
             
typeToStr :: TermType -> String
typeToStr (TermScalarType scalarType) = scalarTypeToStr scalarType
typeToStr (TermVectorType vectorType) = vectorTypeToStr vectorType
