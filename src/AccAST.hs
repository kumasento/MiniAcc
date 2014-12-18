module AccAST where

import Data.Map as Map

import AccType 
import AccSymTable
import AccCodeGen

-- zipWith (+) A (zipWith (+) B C)
-- A :: ArrayT
-- B :: ArrayT
-- C :: ArrayT
-- (+) :: BinopT

data AccFuncT = ZipWith deriving (Show, Eq)

data AccT   = AccTerm ArrayT
            | AccExpr AccFuncT  AccT
            | AccParm BinopT    AccT
            | AccNode AccT      AccT
            deriving (Show)

-- the result [String] is a mid repr

parseAcc :: AccT -> SymTable -> ([DeclRecord], SymTable)
parseAcc (AccExpr f x) symTable = (xs, newSymTable)
    where
        (xs, newSymTable) = case f of
            ZipWith -> parseZipWith x symTable

parseAcc (AccTerm x) symTable   = 
    ([lengthDecl, arrayDecl, arrTDecl], newSymTable3)
    where 
        -- this part is confusing:
        -- insertSymTable will do the symTable insertion and return the position, where it has been currenly inserted.

        (lenVarId, lenVarName, newSymTable1)    = insertSymTable symTable
        (arrayId, arrayName, newSymTable2)      = insertSymTable newSymTable1
        (arrTId, arrTName, newSymTable3)        = insertSymTable newSymTable2

        lengthVal   = (show $ AccType.length x)
        arrayVal    = array x
        
        lengthDecl  = SimpleVarDecl "int" lenVarName lengthVal
        arrayDecl   = ArrayVarDecl "float*" arrayName arrayVal
        arrTDecl    =  
            (ArrDecl "ArrayT*" arrTName "CreateArrayT" [lenVarName, arrayName])

-- parseAcc (AccParm b x) symTable = [show b] ++ (parseAcc x symTable)

parseAcc (AccNode x y) symTable = 
    (resX ++ resY, newSymTable2)
    where
        (resX, newSymTable1) = (parseAcc x symTable) 
        (resY, newSymTable2) = (parseAcc y newSymTable1)

parseZipWith :: AccT -> SymTable -> ([DeclRecord], SymTable)
parseZipWith (AccParm b (AccNode x y)) symTable = 
    (decl1 ++ decl2 ++ [zipWithDecl], nSymT3)
    where
        (decl1, nSymT1)         = parseAcc x symTable
        (decl2, nSymT2)         = parseAcc y nSymT1
        (rId, rName, nSymT3)    = insertSymTable nSymT2

        zipWithDecl = (ZipWithDecl 
            "zipWith"
            [ arrNameStr $ last decl1
            , arrNameStr $ last decl2
            , getBinopsName b]
            rName
            "ArrayT*" )


use :: ArrayT -> AccT
use a = AccTerm a

zipWith :: BinopT -> AccT -> AccT -> AccT
zipWith b x y = (AccExpr ZipWith (AccParm b (AccNode x y)))

-- Test 
