module AST.Function.Lambda where

import AST.SymbolTable
import Codegen.Const
import Data.List as List

-- Lambda expression is built up upon 2 components:
-- 1. variable, or specifically, Scalar type
-- 

-- build the AST Tree
--
data LambdaBuiltinType  = LambdaInteger 
                        | LambdaDouble 
                        deriving (Show)
data LambdaBuiltinFunc  = LambdaFuncAdd 
                        | LambdaFuncSub
                        | LambdaFuncMul
                        | LambdaFuncDiv
                        deriving (Show, Eq)

builtinTypeToStr :: LambdaBuiltinType -> String
builtinTypeToStr t = 
    case t of
        LambdaInteger   -> "int"
        LambdaDouble    -> "double"
builtinFuncToStr :: LambdaBuiltinFunc -> String
builtinFuncToStr f =
    case f of
        LambdaFuncAdd   -> "_lambda_builtin_add"
        LambdaFuncSub   -> "_lambda_builtin_sub"
        LambdaFuncMul   -> "_lambda_builtin_mul"
        LambdaFuncDiv   -> "_lambda_builtin_div"


data LambdaASTTerm = 
        LambdaASTTerm {
            termType    :: LambdaBuiltinType,
            termValue   :: String
        } deriving (Show)

data LambdaASTFunc  = LambdaBuiltinFuncType LambdaBuiltinFunc
                    deriving (Show)

data LambdaASTExpr  = LambdaASTExprTerm LambdaASTTerm
                    | LambdaASTExprFunc LambdaASTFunc [LambdaASTExpr]
                    deriving (Show)

funcToStr :: LambdaASTFunc -> String
funcToStr (LambdaBuiltinFuncType f) = builtinFuncToStr f

data EvalLambda = 
        EvalLambda {
            typeEval :: LambdaBuiltinType,
            nameEval :: String
        } deriving (Show)

data ParserLambda =
        ParserLambda {
            retTuple    :: EvalLambda,
            instStack   :: [String],
            symTable    :: SymbolTable
        }
        deriving (Show)

defaultParserLambdaVar = 
    ParserLambda 
        (EvalLambda LambdaInteger "")
        []
        (initSymTable "_lambda_var_")

exampleLambdaASTExpr = LambdaASTExprTerm (LambdaASTTerm LambdaInteger "1")


parseLambdaBody :: LambdaASTExpr -> ParserLambda -> ParserLambda
parseLambdaBody (LambdaASTExprTerm term) parser =
    ParserLambda eval insts sym
    where
        sym     = incSymTable $ symTable parser
        name    = topSymbol sym
        eval    = EvalLambda (termType term) name
        inst    = [List.intercalate " "
                    [ builtinTypeToStr (termType term)
                    , name
                    , equalOpStr
                    , termValue term
                    , semiOpStr ]] 
        insts   = instStack parser ++ inst 


parseLambdaBody (LambdaASTExprFunc func exprs) parser =
    ParserLambda eval insts sym
    where
        ps      = parseLambdaBodyList exprs parser
        p       = last ps

        sym     = incSymTable $ symTable p
        name    = topSymbol sym
        t       = typeEval $ retTuple p
        eval    = EvalLambda t name

        names   = map (nameEval . retTuple) ps
        funcStr = funcToStr func
        
        inst    = [List.intercalate " "
                    [ builtinTypeToStr t
                    , name
                    , equalOpStr
                    , genFunction funcStr names
                    , semiOpStr]]
        insts   = instStack p ++ inst

exampleLambdaASTFunc = 
    LambdaASTExprFunc
        (LambdaBuiltinFuncType LambdaFuncAdd)
        [ LambdaASTExprTerm (LambdaASTTerm LambdaInteger "1")
        , LambdaASTExprTerm (LambdaASTTerm LambdaInteger "2")]

parseLambdaBodyList :: [LambdaASTExpr] -> ParserLambda -> [ParserLambda]
parseLambdaBodyList (e:es) p = 
    p' : parseLambdaBodyList es p'
    where
        p' = parseLambdaBody e p
parseLambdaBodyList [] _ = []
