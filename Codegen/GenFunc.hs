module Codegen.GenFunc where

import AST.Node
import AST.Function.Operator
import AST.Function.Basic

import Codegen.Const 
import Codegen.GenTerm

import Data.List as List

-- genCodeFuncStr [type] [name] [funcname] [params]
genCodeFuncStr :: String -> String -> String -> [String] -> [String]
genCodeFuncStr t n f ps =
    [List.intercalate " "
        [   t,
            n,
            equalOpStr,
            genFunction f ps,
            semiOpStr]]

binopAddStr = "_binop_add"
binopSubStr = "_binop_sub"
binopMulStr = "_binop_mul"
binopDivStr = "_binop_div"

binopToStr :: Binop -> String
binopToStr binop =
    case binop of
        BinopAdd -> binopAddStr
        BinopSub -> binopSubStr
        BinopMul -> binopMulStr
        BinopDiv -> binopDivStr

optToStr :: Operator -> String
optToStr (BinopOperator binop) = binopToStr binop 

zipWithToStr :: ZipWith -> String
zipWithToStr zw = 
    case zw of 
        ZipWith2    -> "zipWith2"
        ZipWith3    -> "zipWith3"

basicToStr :: Basic -> String
basicToStr (BasicZipWith zipWith) = zipWithToStr zipWith

funcToStr :: ASTFunc -> String
funcToStr (ASTOperator opt) = optToStr opt
funcToStr (ASTBasic basic)  = basicToStr basic

genCodeFunc :: ASTFunc -> TermType -> TermName -> [String] -> [String]
genCodeFunc func t n ps =
    genCodeFuncStr 
        (typeToStr t)
        n
        (funcToStr func)
        ps
    
