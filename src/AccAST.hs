module AccAST where

import AccType 

-- zipWith (+) A (zipWith (+) B C)
-- A :: ArrayT
-- B :: ArrayT
-- C :: ArrayT
-- (+) :: BinopT

data AccFuncT = ZipWith deriving (Show, Eq)

data AccT   = AccTerm ArrayT
            | AccExpr AccFuncT  AccT
            | AccParm BinopT    AccT
            | AccNode AccT      AccT
            deriving (Show)

-- the result [String] is a mid repr

parseAcc :: AccT -> [String]
parseAcc (AccExpr f x) = [show f] ++ xs
    where
        xs = case f of
            ZipWith -> parseZipWith x
parseAcc (AccTerm x)   = [show x]
parseAcc (AccParm b x) = [show b] ++ (parseAcc x)
parseAcc (AccNode x y) = (parseAcc x) ++ (parseAcc y)

parseZipWith :: AccT -> [String]
parseZipWith (AccParm b (AccNode x y)) = 
    [show b] ++ (parseAcc x) ++ (parseAcc y)

use :: ArrayT -> AccT
use a = AccTerm a

zipWith :: BinopT -> AccT -> AccT -> AccT
zipWith b x y = (AccExpr ZipWith (AccParm b (AccNode x y)))
