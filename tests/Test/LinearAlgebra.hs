{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.LinearAlgebra where

import qualified Data.Vector.Generic as V
import Data.Vector.Unboxed (Vector, Unbox)
import Test.Hspec
import Test.QuickCheck hiding ((><))

import Data.Matrix.Sparse
import qualified Data.Vector.Sparse as S
import Data.Vector.Util (increasing, nondecreasing)

instance (Arbitrary a, Unbox a) => Arbitrary (Vector a) where
    arbitrary = fmap V.fromList $ suchThat arbitrary $ \v -> length v > 0

instance (Arbitrary a, Num a, Orient or, Unbox a) => Arbitrary (Matrix or a) where
    arbitrary = do
      m <- arbdim
      n <- arbdim
      arbitraryMatrix m n

arbdim :: Gen Int
arbdim = arbitrary `suchThat` (> 0)

arbitraryMatrix
  :: (Arbitrary a, Num a, Orient or, Unbox a)
  => Int -> Int -> Gen (Matrix or a)
arbitraryMatrix rdim cdim = do
  triples <- vectorOf (rdim * cdim `div` 4) $ do
    r <- choose (0, rdim - 1)
    c <- choose (0, cdim - 1)
    x <- arbitrary
    return (r, c, x)
  return $ (rdim >< cdim) triples

checkMatrixRowR :: Gen (Matrix Row Double) -> SpecWith ()
checkMatrixRowR =
  describe "format properties :: Matrix Row Double" . checkMatrix

checkMatrixRowZ :: Gen (Matrix Row (Complex Double)) -> SpecWith ()
checkMatrixRowZ =
  describe "format properties :: Matrix Row (Complex Double)" . checkMatrix

checkMatrixColR :: Gen (Matrix Col Double) -> SpecWith ()
checkMatrixColR =
  describe "format properties :: Matrix Col Double" . checkMatrix

checkMatrixColZ :: Gen (Matrix Col (Complex Double)) -> SpecWith ()
checkMatrixColZ =
  describe "format properties :: Matrix Col (Complex Double)" . checkMatrix

checkMatrix
  :: (Arbitrary a, Num a, Orient or, Unbox a)
  => Gen (Matrix or a) -> SpecWith ()
checkMatrix arbmat = do
  it "nondecreasing pointers" $ property $ do
    Matrix{..} <- arbmat
    return $ nondecreasing pointers

  it "length pointers == odim + 1" $ property $ do
    Matrix{..} <- arbmat
    return $ V.length pointers == odim + 1

  it "length values == last pointers" $ property $ do
    Matrix{..} <- arbmat
    return $ V.length entries == V.last pointers

  it "increasing indices in slice" $ property $ do
    mat <- arbmat
    let slices = map (slice mat) [0..(odim mat - 1)]
    return $ all (increasing . fst . V.unzip . S.entries) slices

  it "all indices >= 0" $ property $ do
    Matrix{..} <- arbmat
    let indices = fst $ V.unzip entries
    return $ V.all (>= 0) indices

  it "all indices < idim" $ property $ do
    Matrix{..} <- arbmat
    let indices = fst $ V.unzip entries
    return $ V.all (< idim) indices
