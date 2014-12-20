module AST.Node where

-- import AST.Type
--
import AST.Type.Array
import AST.Type.Vector
import AST.Type.Scalar

import AST.Function.Operator
import AST.Function.Basic

-- We could view all the AST nodes as a set of 3 subsets:
-- 1. Subset of Term, which consists the node of Array, Vector and Scalar.
-- 2. Subset of Function (or Func), which consists the node of built-in 
--  functions.
-- 3. Subset of Expr, which is the upper level beyond Term and Func.

data ASTTerm    = ASTArray  Array
                | ASTVector Vector
                | ASTScalar Scalar
                deriving (Show)

data ASTFunc    = ASTOperator   Operator
                | ASTBasic      Basic
                deriving (Show)

data ASTExpr    = ASTTermExpr ASTTerm
                | ASTFuncExpr ASTFunc
                deriving (Show)
