module AST.Node where

-- import AST.Type
--
import AST.Type.Vector
import AST.Type.Scalar

import AST.Function.Operator
import AST.Function.Lambda
import AST.Function.Basic


-- We could view all the AST nodes as a set of 3 subsets:
-- 1. Subset of Term, which consists the node of Array, Vector and Scalar.
-- 2. Subset of Function (or Func), which consists the node of built-in 
--  functions.
-- 3. Subset of Expr, which is the upper level beyond Term and Func.

data ASTTerm    = ASTVector Vector
                | ASTScalar Scalar
                deriving (Show)

data ASTFunc    = ASTOperator   Operator
                | ASTBasic      Basic
                deriving (Show)

data ASTLambda  = 
        ASTLambda {
            lambdaBody :: LambdaASTExpr,
            lambdaName :: String, -- something like _lambda_defined_func_1
        } deriving (Show)

data ASTExpr    = ASTTermExpr   ASTTerm
                | ASTFuncExpr   ASTFunc [ASTExpr]
                | ASTLambdaExpr ASTLambda
                deriving (Show)

data TermType   = TermScalarType    ScalarType
                | TermVectorType    VectorType
                deriving (Show, Eq)
type TermName   = String

-- type system
typeOfTerm :: ASTTerm -> TermType
typeOfTerm (ASTScalar scalar) = TermScalarType $ typeOfScalar scalar
typeOfTerm (ASTVector vector) = TermVectorType $ typeOfVector vector

