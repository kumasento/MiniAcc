module Codegen.Const where

import Data.List as List

equalOpStr  = "="
semiOpStr   = ";"
returnStr   = "return"
lbOpStr     = "{"
rbOpStr     = "}"

doubleTypeStr   = "double"
intTypeStr      = "int"
floatTypeStr    = "float"
charTypeStr     = "char"

vecIntegerTypeStr   = "int*"
vecDoubleTypeStr    = "double*"
vecFloatTypeStr     = "float*"
vecCharTypeStr      = "char*"

readFileStr     = "READ"

genFunction :: String -> [String] -> String 
genFunction nameStr paramList =
    nameStr ++ "(" ++ paramStr ++ ")"
    where
        paramStr = List.intercalate "," paramList
