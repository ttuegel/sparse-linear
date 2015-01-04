{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.LinearAlgebra where

import Data.Traversable
import qualified Data.Vector.Generic as V
import Data.Vector.Storable (Storable, Vector)
import Test.QuickCheck

import Data.Matrix.Sparse
import qualified Data.Vector.Sparse as S
import Data.Vector.Util (increasing, nondecreasing)
import Numeric.LinearAlgebra.Sparse

instance (Arbitrary a, Storable a) => Arbitrary (Vector a) where
    arbitrary = fmap V.fromList $ suchThat arbitrary $ \v -> length v > 0

instance (Arbitrary a, CxSparse a, Indices or) => Arbitrary (Matrix or a) where
    arbitrary = do
      nr <- arbitrary `suchThat` (> 0)
      let nc = nr
      triples <- forM [0..(nr * nr `div` 4)] $ \_ -> do
        r <- arbitrary `suchThat` (\r -> r >= 0 && r < nr)
        c <- arbitrary `suchThat` (\c -> c >= 0 && c < nc)
        x <- arbitrary
        return (r, c, x)
      return $ fromTriples nr nc triples

prop_pointersNondecreasing :: Matrix or a -> Bool
prop_pointersNondecreasing Matrix{..} = nondecreasing pointers

prop_pointersLength :: Matrix or a -> Bool
prop_pointersLength Matrix{..} = V.length pointers == dimM + 1

prop_valuesLength :: Storable a => Matrix or a -> Bool
prop_valuesLength Matrix{..} =
  V.length values == (fromIntegral $ V.last pointers)

prop_indicesIncreasing :: Storable a => Matrix or a -> Bool
prop_indicesIncreasing mat = V.all (increasing . S.indices) (slices mat)

prop_indicesNonNegative :: Storable a => Matrix or a -> Bool
prop_indicesNonNegative mat = V.all (>= 0) (indices mat)

prop_indicesInRange :: Storable a => Matrix or a -> Bool
prop_indicesInRange mat = V.all (< dn) (indices mat)
  where dn = fromIntegral $ dimN mat
