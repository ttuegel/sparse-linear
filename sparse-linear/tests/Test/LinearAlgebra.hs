{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.LinearAlgebra where

import qualified Data.Vector.Generic as V
import Data.Vector.Unboxed (Vector)
import Test.Hspec
import Test.QuickCheck hiding ((><))

import Data.Matrix.Sparse
import qualified Data.Vector.Sparse as S
import Data.Vector.Util (increasing, nondecreasing)

instance (Arbitrary a, Unbox a) => Arbitrary (Vector a) where
    arbitrary = fmap V.fromList $ suchThat arbitrary $ \v -> length v > 0

instance (Arbitrary a, Num a, Unbox a) => Arbitrary (Matrix Vector a) where
    arbitrary = do
      m <- arbdim
      n <- arbdim
      arbitraryMatrix m n

arbdim :: Gen Int
arbdim = arbitrary `suchThat` (> 0)

arbitraryMatrix
  :: (Arbitrary a, Num a, Unbox a)
  => Int -> Int -> Gen (Matrix Vector a)
arbitraryMatrix nr nc = do
  triples <- vectorOf (nr * nc `div` 4 + 1) $ do
    r <- choose (0, nr - 1)
    c <- choose (0, nc - 1)
    x <- arbitrary
    return (r, c, x)
  return $ (nr >< nc) triples

checkMatrix :: Gen (Matrix Vector Int) -> SpecWith ()
checkMatrix arbmat =
  it "format properties" $ property $ do
    mat@Matrix {..} <- arbmat
    let dieUnless str = counterexample ("failed: " ++ str ++ " " ++ show mat)
        slices = map (slice mat) [0..(ncols - 1)]
    return $ conjoin
      [ dieUnless "nondecreasing pointers"
        (nondecreasing pointers)

      , dieUnless "length pointers == ncols + 1"
        (V.length pointers == ncols + 1)

      , dieUnless "length values == last pointers"
        (V.length values == V.last pointers)

      , dieUnless "length indices == last pointers"
        (V.length indices == V.last pointers)

      , dieUnless "increasing indices in slice"
        (all (increasing . (\(S.Vector _ idx _) -> idx)) slices)

      , dieUnless "all indices >= 0"
        (V.all (>= 0) indices)

      , dieUnless "all indices < nrows"
        (V.all (< nrows) indices)
      ]
