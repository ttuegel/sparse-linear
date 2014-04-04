{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Numeric.LinearAlgebra.Sparse.QuickCheck where

import Control.Applicative
import Control.Lens
import Control.Monad (liftM)
import Data.Maybe (catMaybes)
import Data.Proxy.PolyKind
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as U
import Test.QuickCheck

import Numeric.LinearAlgebra.Sparse

arbitraryMatU :: (Arbitrary a, OrderR ord, Unbox a)
              => Int -> Int -> Gen (Matrix U ord a)
arbitraryMatU r c = (pack r c . U.fromList) <$> resize nnz arbitrary
  where
    nnz = (r * c) `div` 2

shrinkMat :: (FormatR fmt, OrderR ord, Unbox a) => Matrix fmt ord a -> [Matrix fmt ord a]
shrinkMat mat =
    let (r, c) = view dim mat
        dropRow | r > 1 = Just $ set dim (pred r, c) mat
                | otherwise = Nothing
        dropCol | c > 1 = Just $ set dim (r, pred c) mat
                | otherwise = Nothing
    in catMaybes [dropRow, dropCol]

-- Need pairs of matrices that are the same size, so we don't want an
-- Arbitrary instance for just (Matrix U ord a).
instance (Arbitrary a, OrderR ord, OrderR ord', Unbox a) => Arbitrary (Matrix U ord a, Matrix U ord' a) where
    arbitrary = do
      r <- abs <$> arbitrarySizedIntegral
      c <- abs <$> arbitrarySizedIntegral
      (,) <$> arbitraryMatU r c <*> arbitraryMatU r c

    shrink = undefined
