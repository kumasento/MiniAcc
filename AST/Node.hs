module AST.Node where

-- import AST.Type
--
import AST.Type.Vector
import AST.Type.Scalar

import AST.Function.Operator
import AST.Function.Basic

-- please notice that the type array has been MOVED here!
-- it's a node in AST now

data ASTArray   = 
        ASTArray {
            length :: ASTExpr,
            vector :: ASTExpr
        } deriving (Show)

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

data ASTExpr    = ASTTermExpr   ASTTerm
                | ASTFuncExpr   ASTFunc
                | ASTStructExpr ASTStruct
                deriving (Show)

-- extra nodes:
-- 1. Struct: Including ASTArray
--
data ASTStruct  = ASTStructArray ASTArray
                deriving (Show)
                

