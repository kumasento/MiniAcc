module AST.Parser where

import AST.Type.Vector
import AST.Type.Scalar
import AST.Function.Operator
import AST.Function.Lambda

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
            retTuple        :: EvalTuple,   -- The evaluation result. force single 
            instStack       :: [String],    -- Stack of instructions
            ptrStack        :: [DataTuple], -- Stack of vector pointers
            symTable        :: SymbolTable,
            funcSymTable    :: SymbolTable,
            lambdaStack     :: [[String]]
        } deriving (Show)

initParser :: String -> String -> Parser
initParser varPrefix funPrefix = 
    Parser 
        nullEvalTuple 
        [] 
        [] 
        (initSymTable varPrefix)
        (initSymTable funPrefix)
        []

defaultParser = initParser "_var_" "_lambda_fun_"

parse :: ASTExpr -> Parser -> Parser
parse (ASTTermExpr term) parser = parseTerm term parser
parse (ASTFuncExpr func exprs) parser = parseFunc func exprs parser    
parse (ASTLambdaExpr lambda) parser = parseLambda lambda parser

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
    Parser eval insts ptrs sym fsym ls
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

        fsym    = funcSymTable parser
        ls      = lambdaStack parser

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
    Parser eval insts ptrs sym fsym ls
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

        fsym        = funcSymTable parser
        ls          = lambdaStack parser

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

parseLambda :: ASTLambda -> Parser -> Parser
parseLambda lambda parser =
    -- ok here we'll have 2 symbol table. 1 for var, 1 for func
    Parser leval insts ptrs sym fsym ls 
    where
        body        = parseLambdaBody 
                        (lambdaBody lambda)
                        defaultParserLambdaVar
        fsym        = incSymTable       $ funcSymTable parser
        fstr        = topSymbol         fsym
        params      = paramListL        body
        instsL      = instStackL        body
        eval        = retTupleL         body
        nameStr     = nameEvalL         eval
        typeStr     = builtinTypeToStr  $ typeEvalL eval
        
        -- [typeStr] [fstr] ([params])
        lambdaDecl  = genLambdaDecl fstr typeStr params
        -- return [nameStr]
        lambdaRet   = genLambdaRet nameStr

        lambdaFunc  = genLambdaFunc lambdaDecl instsL lambdaRet
        ls          = lambdaStack parser ++ [lambdaFunc]
        
        termType    = TermScalarType (builtinTypeToScalarType $ typeEvalL eval)

        -- parseLambda should return [returnType] and the [functionName]
        leval       = EvalTuple fstr termType
        insts       = instStack parser
        ptrs        = ptrStack parser
        sym         = symTable parser

exampleLambda = 
    ASTLambdaExpr 
        (ASTLambda exampleLambdaASTFunc2)

{-
exampleZipWith2 = 
    ASTFuncExpr 
        (ASTBasic (BasicZipWith ZipWith2))
        [ (ASTFuncExpr -- How to distinguish ?
-}
