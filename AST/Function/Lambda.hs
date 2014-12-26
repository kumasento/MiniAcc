module AST.Function.Lambda where

import AST.SymbolTable
import AST.Type.Scalar
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

builtinTypeToScalarType :: LambdaBuiltinType -> ScalarType
builtinTypeToScalarType t =
    case t of 
        LambdaInteger   -> ScalarTypeInteger
        LambdaDouble    -> ScalarTypeDouble

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
            typeEvalL :: LambdaBuiltinType,
            nameEvalL :: String
        } deriving (Show)

data ParserLambda =
        ParserLambda {
            retTupleL    :: EvalLambda,
            instStackL   :: [String],
            symTableL    :: SymbolTable,
            paramListL   :: [(String, String)]
        }
        deriving (Show)

defaultParserLambdaVar = 
    ParserLambda 
        (EvalLambda LambdaInteger "")
        []
        (initSymTable "_lambda_var_")
        []

exampleLambdaASTExpr = LambdaASTExprTerm (LambdaASTTerm LambdaInteger "1")

builtinParamStr = "_LAMBDA_PARAM_"

isValueParam :: String -> Bool
isValueParam str = isInfixOf builtinParamStr str

parseLambdaBody :: LambdaASTExpr -> ParserLambda -> ParserLambda
parseLambdaBody (LambdaASTExprTerm term) parser =
    ParserLambda eval insts sym params
    where
        sym     = incSymTable $ symTableL parser
        name    = topSymbol sym
        eval    = EvalLambda (termType term) name
        typeStr = builtinTypeToStr (termType term)
        inst    = [List.intercalate " "
                    [ typeStr
                    , name
                    , equalOpStr
                    , termValue term]
                    ++ semiOpStr] 
        val     = termValue term
        insts   = instStackL parser ++ inst 
        params  = paramListL parser ++ 
                    if isValueParam val 
                        then [(typeStr, val)]
                        else []
        

parseLambdaBody (LambdaASTExprFunc func exprs) parser =
    ParserLambda eval insts sym params
    where
        ps      = parseLambdaBodyList exprs parser
        p       = last ps

        sym     = incSymTable $ symTableL p
        name    = topSymbol sym
        t       = typeEvalL $ retTupleL p
        eval    = EvalLambda t name

        names   = map (nameEvalL . retTupleL) ps
        funcStr = funcToStr func
        
        inst    = [List.intercalate " "
                    [ builtinTypeToStr t
                    , name
                    , equalOpStr
                    , genFunction funcStr names]
                    ++ semiOpStr]
        insts   = instStackL p ++ inst
        params  = paramListL p

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
