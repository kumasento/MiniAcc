
import AccType
import AccCodeGen
import AccAST
import System.Process

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

-- zipWith (+) A (zipWith (+) B C) => A + B + C

runThreeArrays :: ArrayT -> ArrayT -> ArrayT -> [String]
runThreeArrays x y z = 
    let x' = use x
        y' = use y
        z' = use z
    in
    Prelude.map AccCodeGen.genDecl 
        (fst 
            (parseAcc (AccAST.zipWith Add x' (AccAST.zipWith Add y' z')) 
                        $ Map.empty))

main = do
    writeFile 
            "ExampleSolution.h" 
            (genCodeBlock $ runThreeArrays 
                    (ArrayT 128 [1..1024])
                    (ArrayT 128 [1..1024])
                    (ArrayT 128 [1..1024]))

    runCommand "make Example1 && ./Example1.o"
    res <- readFile "ExampleOutput.dat"
    
    putStrLn res
    
