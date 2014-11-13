{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DeriveDataTypeable  #-} -- will introduce typeOf function

module Types where

-- standard library
import Data.Typeable

-- All these representations are inspired by Repa library.

-- 'Z' represents rank-0
data Z = Z 
    deriving (Eq, Show, Typeable)

-- declare a new infix operator --> :. 
-- representing the 'shape construction'
infixl 3 :.
data tail :. head = tail :. head
    deriving (Eq, Show, Typeable)

type DIM0 = Z
type DIM1 = DIM0 :. Int
type DIM2 = DIM1 :. Int

-- Typeclass: Shape
--
-- we're regarding Shape as a 2-tuple, which has a inner Shape as a first element
-- for example: ((), 3, 2) represents a 2 dimensional array
--              with 3 elements on the first dimension and 2 on the second
class (Eq sh) => Shape sh where
    dim     :: sh -> Int
    size    :: sh -> Int

instance Shape () where
    dim _       = 0
    size ()     = 1

instance (Shape sh) => Shape (sh, Int) where
    dim (x,_)       = dim x + 1 
    size (sh, sz)   = size sh * sz

-- Typeclass: Elt
--
-- till now, only simple numeric types are supported:
--      Int, Float, Double

class (Num e) => Elt e
instance Elt Int
instance Elt Float
instance Elt Double

-- Data: Array
--

data Array sh e where
    Array :: (Shape sh, Elt e) => sh ->e ->Array sh e
