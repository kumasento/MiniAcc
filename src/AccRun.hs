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

exampleAccT :: AccT
exampleAccT = 
    let x' = use (ArrayT 128 [1..1024])
        y' = use (ArrayT 128 [1..1024])
        z' = use (ArrayT 128 [1..1024])
    in
    (AccAST.zipWith Add x' (AccAST.zipWith Add y' z'))

exampleParseAccString :: AccT -> [String]
exampleParseAccString x = 
    Prelude.map AccCodeGen.genDecl xs
    where
        decls   = (parseAcc x $ Map.empty)
        xs      = fst decls

runAccMonad :: AccT -> IO [Double]
runAccMonad x = do
    writeFile 
            "ExampleSolution.h" 
            (genCodeBlock $ exampleParseAccString x)

    system "make Example1 > ExampleMakeResult.tmp"
    system "./Example1.o"
    res <- readFile "ExampleOutput.dat"

    -- system "rm ExampleOutput.dat"
    -- system "rm ExampleSolution.h"
   
    return ((read res)::[Double])
-- 
-- run :: AccT -> [Double]
-- run x = runMonad x

