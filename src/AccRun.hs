
module AccRun where

import AccType
import AccCodeGen
import AccAST
import System.Process

import Data.ByteString as BS
import Data.List as List
import Data.Map as Map

parseAccStr :: AccT -> ([String], [String])
parseAccStr x = 
    AccCodeGen.genCodeStrList xs
    where 
        decls   = (parseAcc x $ Map.empty)
        xs      = fst decls

-- What runAccMonad should do is:
-- 1. Take the root node of AST and run the iteration.
-- 2. Output the iteration result to a outer header file.
-- 3. Build OpenCL Program
-- 4. Run
-- 5. Read the result back

oclSrcDir       = "../oclsrc/"
oclSrcInclude   = oclSrcDir ++ "include/"
oclSrcData      = oclSrcDir ++ "data/"

headerFileName  = oclSrcInclude ++ "Solution.h"
dataFileName    = oclSrcData ++ "Solution.dat"
outFileName     = oclSrcData ++ "Output.dat"

runAccMonad :: AccT -> IO [Double]
runAccMonad x = do
    let accStr = parseAccStr x
    -- Write result to header file
    Prelude.writeFile headerFileName $ (genCodeFileStr . fst) accStr
    Prelude.writeFile dataFileName   $ (genDataFileStr . snd) accStr

    let cmdStr = [ "cd ../oclsrc"
                 , "make Main"         -- Build OpenCL program
                 , "./bin/oclmain.o"   -- Run
                 , "cd ../src" ]

    let cmdStrLine = List.intercalate " && " cmdStr

    system cmdStrLine
    
    res <- Prelude.readFile outFileName -- Read result back

    -- system "rm ExampleOutput.dat"
    -- system "rm ExampleSolution.h"
   
    return ((read res)::[Double])
-- 
-- run :: AccT -> [Double]
-- run x = runMonad x

