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
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as U
import Test.QuickCheck

import Numeric.LinearAlgebra.Sparse

arbitraryMat  :: (Arbitrary a, FormatR fmt, Orient or, Unbox a)
              => Int -> Int -> Gen (Matrix fmt or a)
arbitraryMat r c = (fromU . pack r c . U.fromList . nubBy ((==) `on` indices) . take nnz . filter checkBounds . map fixIndices) <$> arbitrary
  where
    nnz = (r * c) `div` 2
    fixIndices (i, j, x) = (abs i, abs j, x)
    checkBounds (i, j, _) = i < r && j < c
    indices (i, j, _) = (i, j)

shrinkMat :: (FormatR fmt, Orient or, Unbox a)
          => Matrix fmt or a -> [Matrix fmt or a]
shrinkMat mat =
    let (r, c) = view dim mat
        dropRow | r > 1 = Just $ set dim (pred r, c) mat
                | otherwise = Nothing
        dropCol | c > 1 = Just $ set dim (r, pred c) mat
                | otherwise = Nothing
    in catMaybes [dropRow, dropCol]

-- Need pairs of matrices that are the same size, so we don't want an
-- Arbitrary instance for just (Matrix U or a).
instance (Arbitrary a, FormatR fmt, Orient or, Orient or', Unbox a)
    => Arbitrary (Matrix fmt or a, Matrix fmt or' a) where

    arbitrary = do
      r <- abs <$> arbitrarySizedIntegral
      c <- abs <$> arbitrarySizedIntegral
      (,) <$> arbitraryMat r c <*> arbitraryMat r c

    shrink (a, b) = zip (shrinkMat a) (shrinkMat b)

-- Need triples of matrices that are the same size, so we don't want an
-- Arbitrary instance for just (Matrix U or a).
instance
    (Arbitrary a, FormatR fmt, Orient or, Orient or', Orient or'', Unbox a)
    => Arbitrary (Matrix fmt or a, Matrix fmt or' a, Matrix fmt or'' a) where

    arbitrary = do
      r <- abs <$> arbitrarySizedIntegral
      c <- abs <$> arbitrarySizedIntegral
      (,,) <$> arbitraryMat r c <*> arbitraryMat r c <*> arbitraryMat r c

    shrink (a, b, c) = zip3 (shrinkMat a) (shrinkMat b) (shrinkMat c)

-- Need pairs of matrices that are the same size, so we don't want an
-- Arbitrary instance for just (Matrix U or a).
instance (Arbitrary a, FormatR fmt, Orient or, Unbox a)
    => Arbitrary (Matrix fmt or a, Vector a) where

    arbitrary = do
      r <- abs <$> arbitrarySizedIntegral
      c <- abs <$> arbitrarySizedIntegral
      (,) <$> arbitraryMat r c <*> U.replicateM c arbitrary

    shrink (a, b) = [(set dim (r, c - 1) a, U.take (c - 1) b)]
      where
        (r, c) = view dim a

-- Need pairs of matrices that are the same size, so we don't want an
-- Arbitrary instance for just (Matrix U or a).
instance (Arbitrary a, FormatR fmt, Orient or, Orient or', Unbox a)
    => Arbitrary (Matrix fmt or a, Matrix fmt or' a, Vector a) where

    arbitrary = do
      r <- abs <$> arbitrarySizedIntegral
      c <- abs <$> arbitrarySizedIntegral
      (,,) <$> arbitraryMat r c <*> arbitraryMat r c <*> U.replicateM c arbitrary

    shrink (a, b, d) | c > 1 = [(set dim (r, c - 1) a, set dim (r, c - 1) b, U.take (c - 1) d)]
                     | otherwise = []
      where
        (r, c) = view dim a
