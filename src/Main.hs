
import AccType
import AccCodeGen
import AccAST

import Data.Map as Map

addZipWith :: ArrayT -> ArrayT -> AccT
addZipWith x y = 
    let x' = use x
        y' = use y
    in
    AccAST.zipWith Add x' y'

runTwoArrays :: ArrayT -> ArrayT -> [String]
runTwoArrays x y = 
    Prelude.map AccCodeGen.genDecl (fst (parseAcc a $ Map.empty))
    where
        a = addZipWith x y

main = do
    putStrLn "{"
    mapM_ putStrLn $ runTwoArrays 
                        (ArrayT 2048 [1..2048])
                        (ArrayT 2048 [1..2048])
    putStrLn "}"
