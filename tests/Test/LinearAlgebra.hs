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
  it "format properties :: Matrix Row Double" . checkMatrix

checkMatrixRowZ :: Gen (Matrix Row (Complex Double)) -> SpecWith ()
checkMatrixRowZ =
  it "format properties :: Matrix Row (Complex Double)" . checkMatrix

checkMatrixColR :: Gen (Matrix Col Double) -> SpecWith ()
checkMatrixColR =
  it "format properties :: Matrix Col Double" . checkMatrix

checkMatrixColZ :: Gen (Matrix Col (Complex Double)) -> SpecWith ()
checkMatrixColZ =
  it "format properties :: Matrix Col (Complex Double)" . checkMatrix

checkMatrix
  :: (Arbitrary a, Num a, Orient or, Show a, Unbox a)
  => Gen (Matrix or a) -> Property
checkMatrix arbmat = property $ do
  mat@Matrix{..} <- arbmat
  let dieUnless str = counterexample ("failed: " ++ str ++ " " ++ show mat)
      slices = map (slice mat) [0..(odim - 1)]
      indices = fst $ V.unzip entries
  return $ conjoin
    [ dieUnless "nondecreasing pointers" (nondecreasing pointers)
    , dieUnless "length pointers == odim + 1" (V.length pointers == odim + 1)
    , dieUnless "length values == last pointers" (V.length entries == V.last pointers)
    , dieUnless "increasing indices in slice" (all (increasing . fst . V.unzip . S.entries) slices)
    , dieUnless "all indices >= 0" (V.all (>= 0) indices)
    , dieUnless "all indices < idim" (V.all (< idim) indices)
    ]
