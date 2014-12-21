module AST.Parser where

import AST.Type.Array
import AST.Type.Vector
import AST.Type.Scalar

import AST.Node
import AST.SymbolTable

import Codegen.GenCode

data DataTuple = 
        DataTuple {
            nameStr :: String, 
            vector  :: Vector
        } deriving (Show, Eq)-- (name, value)

nullDataTuple = DataTuple "" nullVector

genData :: ASTTerm -> String -> [DataTuple]
genData (ASTScalar scalar) nameStr = []
genData (ASTVector vector) nameStr = [DataTuple nameStr vector]

data Parser = 
        Parser {
            nameEval    :: String,      -- The evaluation result.            
            instStack   :: [String],    -- Stack of instructions
            ptrStack    :: [DataTuple], -- Stack of vector pointers
            symTable    :: SymbolTable
        } deriving (Show)

initParser :: String -> Parser
initParser varPrefix = 
    Parser "" [] [] $ initSymTable varPrefix

defaultParser = initParser "var_"

parse :: ASTExpr -> Parser -> Parser
parse (ASTTermExpr term) parser     = parseTerm term parser
parse (ASTStructExpr struct) parser = parseStruct struct parser

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
    Parser name insts ptrs sym
    where
        sym     = incSymTable $ symTable parser
        name    = topSymbol sym
        -- @inst and @ptr are 2 result lists
        inst    = genCodeTerm term name
        ptr     = genData term name

        insts   = instStack parser  ++ inst
        ptrs    = ptrStack parser   ++ ptr

exampleASTTermScalar = ASTScalar (ScalarInteger 1)
exampleASTTermVector = ASTVector (VectorDouble [1..10])

parseStruct :: ASTStruct -> Parser -> Parser
parseStruct (ASTStructArray array) parser =
    Parser name insts ptrs sym
    where
        parser1     = parse (length array) parser
        parser2     = parse (vector array) parser1
        
        nameEval1   = nameEval parser1
        nameEval2   = nameEval parser2
        sym         = incSymTable $ symTable parser2
        name        = topSymbol sym
        inst        = genCode
