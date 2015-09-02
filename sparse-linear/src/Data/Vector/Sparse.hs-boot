module Data.Vector.Sparse where

import qualified Data.Vector.Unboxed as U

data Vector v a = Vector !Int (U.Vector Int) (v a)
