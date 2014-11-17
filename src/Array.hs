-- Array.hs: Containing the definition of Array
--

{-# LANGUAGE FlexibleInstances          #-} -- Shape (sh, Int)
{-# LANGUAGE GADTs                      #-} -- data .. where

module Array where

-- The principle of designation is to build a type which contains all the info
-- that a array need:
-- ARRAY [a] -> Array shape elemType [a]
--

-- Shape is a synonym of n-Tuple, where n means number of dimensions and each 
-- element is representing the number of elements in that dimension:
-- ((), 1, 2) -> 2-dimension, 1 on the first and 2 on the second

type DIM0 = ()
type DIM1 = (DIM0, Int)
type DIM2 = (DIM1, Int)
type DIM3 = (DIM2, Int)

-- Shape Interfaces
--
-- do I need more functions? 
-- dim      -> get the dim  
-- size     -> get the size of 1-dim array
-- toList   -> get the size of each dimension
class Shape sh where
    dim     :: sh -> Int        -- get the dimension of the shape
    size    :: sh -> Int        -- get the size of that shape
    toList  :: sh -> [Int]      -- get size of each dimension

instance Shape () where
    dim     _   = 0             -- every instance of DIM0 has 0-dim and 1-size
    size    _   = 1
    toList  ()  = []

instance (Shape sh) => Shape (sh, Int) where
    -- It's treating ((), Int, ..., Int) ..
    dim     (sh, _)     = dim       sh + 1        -- 
    size    (sh, sz)    = size      sh * sz      
    toList  (sh, x)     = toList    sh ++ [x]
    
sDIM1 :: Int -> DIM1
sDIM1 x     = ((), x) 

sDIM2 :: Int -> Int -> DIM2
sDIM2 x y   = (sDIM1 x, y)

sDIM3 :: Int -> Int -> Int -> DIM3
sDIM3 x y z = (sDIM2 x y, z)


-- ElemType is the type of all the elements in the array. 
-- 
-- there're only 3 types: Int, Float and Double
-- interfaces will be used to generate underlying data structures
-- 

data IntT       = Int    deriving (Eq, Show)
data FloatT     = Float  deriving (Eq, Show)
data DoubleT    = Double deriving (Eq, Show)

class (Show a) => Elt a where
    byteN   :: a -> Int

instance Elt IntT where
    byteN Int       = 4

instance Elt FloatT where
    byteN Float     = 4

instance Elt DoubleT where
    byteN Double    = 8

-- Array: THIS is the type of each kind of array, without data
--
-- Array sh e creates a SET, which contains all the possible array types
-- for example: 
-- Array DIM1 Int is a "Array sh e" type

data Array sh e where
    Array :: (Shape sh, Elt e) =>
                sh  ->
                e   ->
                Array sh e

-- print related infos
instance Show (Array sh e) where
    show (Array shape elt) = "(Array "++show (toList shape)++","++show elt++")"

-- ArrayL is the container of Array type and Array data
-- According to Array's definition, sh and e's type could be deduced
-- What about a?

type ArrayL sh e a = (Array sh e, [a])

-- build ArrayL from Array Type and Array Data
fromList :: (Shape sh, Elt e, Num a) => sh -> e -> [a] -> ArrayL sh e a
fromList shape elt xs = (arrT, xss)
    where
        arrT    = Array shape elt
        sizeN   = size  shape
        xss     = take  sizeN xs

-- split data out
splitList   :: ArrayL sh e a -> [a]
splitList   ((Array shape elt), xs) = xs

splitShape  :: ArrayL sh e a -> sh
splitShape  ((Array shape elt), xs) = shape

splitElt    :: ArrayL sh e a -> e
splitElt    ((Array shape elt), xs) = elt
