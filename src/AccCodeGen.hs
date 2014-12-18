
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


-- genDecl should return a tuple:
-- 1. for real instructions
-- 2. for data file
--
genDecl :: DeclRecord -> (String, String)
genDecl (SimpleVarDecl varType varNameStr varValue) = 
    (instStr, dataStr)
    where
        instStr = varType ++ " " ++ varNameStr ++ " = " ++ varValue ++ ";"
        dataStr = ""

-- change this function, put varValueStr in a new return

genDecl (ArrayVarDecl x y z) = 
    (instStr, dataStr)
    where
        varTypePtr  = x
        varValue    = z
        varListSize = List.length varValue
        varValueStr = List.intercalate " " $ map show varValue

        instStr     = varTypePtr++" "++y++" = "
                        ++"READ(\""++y++"\");"
        dataStr     = y++":"++varValueStr

genDecl (ArrDecl x y z t) = 
    ((List.intercalate " " [varTypePtr,varNameStr,"=",varFunc]) ++ ";", "")
    where
        varTypePtr  = x
        varNameStr  = y
        varFuncName = z
        varParams   = "("++(List.intercalate "," t)++")"
        varFunc     = varFuncName ++ varParams

genDecl (ZipWithDecl x y z t) = 
    ((List.intercalate " "
        [ t, z, "=", x
        , "("++(List.intercalate "," y)++")"
        , ";"]), "")


genCodeStrList :: [DeclRecord] -> ([String], [String])
genCodeStrList xs = 
    (rsx, rsy)
    where
        rs  = Prelude.map genDecl xs
        rsx = Prelude.map fst rs
        rsy = Prelude.map snd rs

genCodeFileStr :: [String] -> String
genCodeFileStr xs = "{\n" ++ (List.intercalate "\n" xs) ++ "\n}"

genDataFileStr :: [String] -> String
genDataFileStr xs = List.intercalate "\n" $ filter (\x -> x /= "") xs
