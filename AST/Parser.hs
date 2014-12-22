module AST.Parser where

import AST.Type.Vector
import AST.Type.Scalar
import AST.Function.Operator

import AST.Node
import AST.SymbolTable

import Codegen.GenCode
import Codegen.GenFunc

data DataTuple = 
        DataTuple {
            nameStr :: String, 
            vector  :: Vector
        } deriving (Show, Eq)-- (name, value)

genData :: ASTTerm -> String -> [DataTuple]
genData (ASTScalar scalar) nameStr = []
genData (ASTVector vector) nameStr = [DataTuple nameStr vector]

data EvalTuple =
        EvalTuple {
            nameEval :: TermName,
            typeEval :: TermType
        } deriving (Show, Eq)

nullEvalTuple = EvalTuple "" (TermScalarType ScalarTypeInteger) -- assume

data Parser = 
        Parser {
            retTuple    :: EvalTuple,   -- The evaluation result. force single 
            instStack   :: [String],    -- Stack of instructions
            ptrStack    :: [DataTuple], -- Stack of vector pointers
            symTable    :: SymbolTable
        } deriving (Show)

initParser :: String -> Parser
initParser varPrefix = 
    Parser nullEvalTuple [] [] $ initSymTable varPrefix

defaultParser = initParser "var_"

parse :: ASTExpr -> Parser -> Parser
parse (ASTTermExpr term) parser = parseTerm term parser
parse (ASTFuncExpr func exprs) parser = parseFunc func exprs parser    

parseTerm :: ASTTerm -> Parser -> Parser
parseTerm term parser = 
    -- parsing scalar:
    -- 1 step: get variable name
    -- 2 step: build inst string
    -- 3 step: build parser

    -- parsing vector:
    -- 1 step: get variable name
    -- 2 step: build DataTuple
    -- 3 step: build inst string -> READ('var_x')
    -- 4 step: build new parser
    --
    -- so we'll need to integrate those things
    Parser eval insts ptrs sym
    where
        sym     = incSymTable $ symTable parser
        nameTerm= topSymbol sym
        typeTerm= typeOfTerm term 

        eval    = EvalTuple nameTerm typeTerm
        -- @inst and @ptr are 2 result lists
        inst    = genCodeTerm term nameTerm
        ptr     = genData term nameTerm

        insts   = instStack parser  ++ inst
        ptrs    = ptrStack parser   ++ ptr

exampleASTTermScalar = ASTScalar (ScalarInteger 1)
exampleASTTermVector = ASTVector (VectorDouble [1..10])

parseExprList :: [ASTExpr] -> Parser -> [Parser]
parseExprList [] _          = []
parseExprList (e:es) init   = 
    parser : parseExprList es parser
    where
        parser  = parse e init

parseFunc :: ASTFunc -> [ASTExpr] -> Parser -> Parser
parseFunc func exprs parser =
    -- how to parse function? => (func, [Expr])
    -- 1 step: recognize which function it is.
    -- 2 step: parse each expr and check the parsed result
    -- 3 
    Parser eval insts ptrs sym
    where
        parserList  = parseExprList exprs parser
        parserLast  = last parserList
        evalList    = map retTuple parserList
        paramList   = map nameEval evalList

        typeTerm    = typeEval      $ retTuple parserLast
        sym         = incSymTable   $ symTable parserLast
        nameTerm    = topSymbol sym
    
        eval        = EvalTuple nameTerm typeTerm
        inst        = genCodeFunc func typeTerm nameTerm paramList
        insts       = instStack parserLast ++ inst
        ptrs        = ptrStack parserLast

exampleBinop = 
    ASTFuncExpr 
        (ASTOperator (BinopOperator BinopAdd))
        [ (ASTTermExpr (ASTScalar (ScalarInteger 1)))
        , (ASTTermExpr (ASTScalar (ScalarInteger 2)))]

exampleBinop2 =
    ASTFuncExpr
        (ASTOperator (BinopOperator BinopAdd))
        [ exampleBinop
        , exampleBinop]

{-
exampleZipWith2 = 
    ASTFuncExpr 
        (ASTBasic (BasicZipWith ZipWith2))
        [ (ASTFuncExpr -- How to distinguish ?
-}
