{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Data.MorallyEq where

import Control.Lens
import Data.Complex
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as U

import Numeric.LinearAlgebra.Sparse

class MorallyEq a where
    morallyEq :: Show a => a -> a -> Bool

instance MorallyEq Double where
    morallyEq x y = x == y || approxEq
      where approxEq = (< 1.0E-8) $ 2 * abs (x - y) / (abs x + abs y)

instance MorallyEq (Complex Double) where
    morallyEq x y = x == y || approxEq
      where
        approxEq =
            (< 1.0E-8) $ 2 * magnitude (x - y) / (magnitude x + magnitude y)

instance (Format fmt, MorallyEq a, Orient or, Show a, Unbox a) =>
         MorallyEq (Matrix fmt or a) where
    morallyEq ((^. uncompressed) -> a) ((^. uncompressed) -> b) =
        U.and $ U.zipWith ok (unpack a) (unpack b)
      where
        ok (i, j, x) (m, n, y) = i == m && j == n && morallyEq x y

instance (MorallyEq a, Show a, Unbox a) => MorallyEq (Vector a) where
    morallyEq a b = U.and $ U.zipWith morallyEq a b

