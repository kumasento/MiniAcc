module Codegen.Const where

import Data.List as List

equalOpStr  = "="
semiOpStr   = ";"

intTypeStr      = "int"
doubleTypeStr   = "double"
floatTypeStr    = "float"
charTypeStr     = "char"

readFileStr     = "READ"

genFunction :: String -> [String] -> String 
genFunction nameStr paramList =
    nameStr ++ "(" ++ paramStr ++ ")"
    where
        paramStr = List.intercalate "," paramList
