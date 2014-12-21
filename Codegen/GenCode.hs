module Codegen.GenCode where

import AST.Node

import Codegen.GenTerm

genCodeTerm :: ASTTerm -> String -> [String] 
genCodeTerm (ASTScalar scalar) nameStr = genCodeScalar scalar nameStr
genCodeTerm (ASTVector vector) nameStr = genCodeVector vector nameStr

-- Struct is what? just like a function, with constructor and parameters
genCodeStruct :: ASTStruct -> 
