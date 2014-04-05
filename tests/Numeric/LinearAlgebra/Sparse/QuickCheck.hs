{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Numeric.LinearAlgebra.Sparse.QuickCheck where

import Control.Applicative
import Control.Lens
import Control.Monad (liftM)
import Data.Function (on)
import Data.List (nubBy)
import Data.Maybe (catMaybes)
import Data.Proxy.PolyKind
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as U
import Test.QuickCheck

import Numeric.LinearAlgebra.Sparse

arbitraryMat  :: (Arbitrary a, FormatR fmt, OrderR ord, Unbox a)
              => Int -> Int -> Gen (Matrix fmt ord a)
arbitraryMat r c = (fromU . pack r c . U.fromList . nubBy ((==) `on` indices) . take nnz . filter checkBounds . map fixIndices) <$> arbitrary
  where
    nnz = (r * c) `div` 2
    fixIndices (i, j, x) = (abs i, abs j, x)
    checkBounds (i, j, _) = i < r && j < c
    indices (i, j, _) = (i, j)

shrinkMat :: (FormatR fmt, OrderR ord, Unbox a)
          => Matrix fmt ord a -> [Matrix fmt ord a]
shrinkMat mat =
    let (r, c) = view dim mat
        dropRow | r > 1 = Just $ set dim (pred r, c) mat
                | otherwise = Nothing
        dropCol | c > 1 = Just $ set dim (r, pred c) mat
                | otherwise = Nothing
    in catMaybes [dropRow, dropCol]

-- Need pairs of matrices that are the same size, so we don't want an
-- Arbitrary instance for just (Matrix U ord a).
instance (Arbitrary a, FormatR fmt, OrderR ord, OrderR ord', Unbox a)
    => Arbitrary (Matrix fmt ord a, Matrix fmt ord' a) where

    arbitrary = do
      r <- abs <$> arbitrarySizedIntegral
      c <- abs <$> arbitrarySizedIntegral
      (,) <$> arbitraryMat r c <*> arbitraryMat r c

    shrink (a, b) = zip (shrinkMat a) (shrinkMat b)

-- Need triples of matrices that are the same size, so we don't want an
-- Arbitrary instance for just (Matrix U ord a).
instance
    (Arbitrary a, FormatR fmt, OrderR ord, OrderR ord', OrderR ord'', Unbox a)
    => Arbitrary (Matrix fmt ord a, Matrix fmt ord' a, Matrix fmt ord'' a) where

    arbitrary = do
      r <- abs <$> arbitrarySizedIntegral
      c <- abs <$> arbitrarySizedIntegral
      (,,) <$> arbitraryMat r c <*> arbitraryMat r c <*> arbitraryMat r c

    shrink (a, b, c) = zip3 (shrinkMat a) (shrinkMat b) (shrinkMat c)
