
module AccCodeGen where

import AccType
import Data.List as List

getBinopsName :: BinopT -> String
getBinopsName x = case x of 
                    Add -> "\'+\'"
                    Sub -> "\'-\'"
                    Mul -> "\'*\'"
                    Div -> "\'/\'"

-- generate array decl

type DeclParamType  = String
type DeclParamName  = String

data DeclRecord = SimpleVarDecl {
                    simpleVarType       :: String,
                    simpleVarNameStr    :: String,
                    simpleVarValue      :: String
                }
                | ArrayVarDecl {
                    arrayVarType        :: String,
                    arrayVarNameStr     :: String,
                    arrayVarValueList   :: ElemListType
                } 
                | ArrDecl {
                    arrTypePtr  :: String,
                    arrNameStr  :: String,
                    arrDeclFunc :: String,
                    arrDeclParm :: [String]
                } 
                | ZipWithDecl {
                    funcName    :: String,
                    funcParam   :: [String],
                    arrNameStr  :: String,
                    funcRetType :: String
                } deriving (Show)

genDecl :: DeclRecord -> String
genDecl (SimpleVarDecl varType varNameStr varValue) = 
    varType ++ " " ++ varNameStr ++ " = " ++ varValue ++ ";"

genDecl (ArrayVarDecl x y z) = 
    varTypePtr++" "++varNameStr++" = "++"{"++varValueStr++"};"
    where
        varTypePtr  = x
        varValue    = z
        varListSize = List.length varValue
        varNameStr  = y ++"["++(show varListSize)++"]"
        varValueStr = List.intercalate "," $ map show varValue

genDecl (ArrDecl x y z t) = 
    (List.intercalate " " [varTypePtr,varNameStr,"=",varFunc]) ++ ";"
    where
        varTypePtr  = x
        varNameStr  = y
        varFuncName = z
        varParams   = "("++(List.intercalate "," t)++")"
        varFunc     = varFuncName ++ varParams

genDecl (ZipWithDecl x y z t) = 
    (List.intercalate " "
        [ t, z, "=", x
        , "("++(List.intercalate "," y)++")"
        , ";"])

