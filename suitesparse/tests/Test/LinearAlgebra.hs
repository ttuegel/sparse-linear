{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.LinearAlgebra where

import Data.Traversable
import qualified Data.Vector.Generic as V
import Data.Vector.Unboxed (Vector, Unbox)
import Test.Hspec
import Test.QuickCheck

import Data.Matrix.Sparse
import qualified Data.Vector.Sparse as S
import Data.Vector.Util (increasing, nondecreasing)

instance (Arbitrary a, Unbox a) => Arbitrary (Vector a) where
    arbitrary = fmap V.fromList $ suchThat arbitrary $ \v -> length v > 0

instance (Arbitrary a, Num a, Unbox a) => Arbitrary (Matrix a) where
    arbitrary = do
      nr <- arbitrary `suchThat` (> 0)
      let nc = nr
      triples <- forM [0..(nr * nr `div` 4)] $ \_ -> do
        r <- arbitrary `suchThat` (\r -> r >= 0 && r < nr)
        c <- arbitrary `suchThat` (\c -> c >= 0 && c < nc)
        x <- arbitrary
        return (r, c, x)
      return $ fromTriples nr nc triples

prop_pointersNondecreasing :: Matrix a -> Bool
prop_pointersNondecreasing Matrix{..} = nondecreasing pointers

prop_pointersLength :: Matrix a -> Bool
prop_pointersLength Matrix{..} = V.length pointers == ncols + 1

prop_valuesLength :: Unbox a => Matrix a -> Bool
prop_valuesLength Matrix{..} =
  V.length entries == fromIntegral (V.last pointers)

prop_indicesIncreasing :: Unbox a => Matrix a -> Bool
prop_indicesIncreasing mat =
  all (increasing . fst . V.unzip . S.entries)
  $ map (slice mat) [0..(ncols mat - 1)]

prop_indicesNonNegative :: Unbox a => Matrix a -> Bool
prop_indicesNonNegative = V.all (>= 0) . fst . V.unzip . entries

prop_indicesInRange :: Unbox a => Matrix a -> Bool
prop_indicesInRange Matrix{..} = V.all (< nrows) $ fst $ V.unzip entries

checkFunMat1
  :: (Arbitrary a, Num a, Show a, Unbox a)
  => (Matrix a -> Matrix a) -> SpecWith ()
checkFunMat1 f = do
  it "nondecreasing pointers" $ property $ prop_pointersNondecreasing . f
  it "pointers length" $ property $ prop_pointersLength . f
  it "values length" $ property $ prop_valuesLength . f
  it "increasing indices per slice" $ property $ prop_indicesIncreasing . f
  it "non-negative indices" $ property $ prop_indicesNonNegative . f
  it "indices < dim" $ property $ prop_indicesInRange . f

checkFunMat2
  :: (Arbitrary a, Num a, Show a, Unbox a)
  => (Matrix a -> Matrix a -> Matrix a) -> SpecWith ()
checkFunMat2 f = do
  it "nondecreasing pointers" $ property $ \a b ->
    prop_pointersNondecreasing (f a b)

  it "pointers length" $ property $ \a b ->
    prop_pointersLength (f a b)

  it "values length" $ property $ \a b ->
    prop_valuesLength (f a b)

  it "increasing indices per slice" $ property $ \a b ->
    prop_indicesIncreasing (f a b)

  it "non-negative indices" $ property $ \a b ->
    prop_indicesNonNegative (f a b)

  it "indices < dim" $ property $ \a b ->
    prop_indicesInRange (f a b)
