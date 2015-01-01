{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.LinearAlgebra where

import Control.Applicative
import Data.Traversable
import Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as U
import Test.QuickCheck

import Data.Matrix.Sparse
import qualified Data.Vector.Sparse as SpV
import Numeric.LinearAlgebra.Sparse

instance (Arbitrary a, Storable a) => Arbitrary (Vector a) where
    arbitrary = fmap V.fromList $ suchThat arbitrary $ \v -> length v > 0

instance (Arbitrary a, CxSparse a, Storable a, Unbox a) => Arbitrary (Matrix a) where
    arbitrary = do
      nr <- arbitrary `suchThat` (> 0)
      let nc = nr
          ixs = do
            r <- [0..(nr - 1)]
            c <- [0..(nc - 1)]
            return (r, c)
      xs <- forM ixs $ \(r, c) -> (,,) r c <$> arbitrary
      return $ compress nr nc $ U.fromList xs

nondecreasing :: (Ord a, Storable a) => Vector a -> Bool
nondecreasing vec = V.and $ V.zipWith (<=) (V.init vec) (V.tail vec)

increasing :: (Ord a, Storable a) => Vector a -> Bool
increasing vec = V.and $ V.zipWith (<) (V.init vec) (V.tail vec)

prop_columnPointersNondecreasing :: Matrix a -> Bool
prop_columnPointersNondecreasing Matrix{..} = nondecreasing columnPointers

prop_columnPointersLength :: Matrix a -> Bool
prop_columnPointersLength Matrix{..} = V.length columnPointers == nColumns + 1

prop_valuesLength :: Storable a => Matrix a -> Bool
prop_valuesLength Matrix{..} =
  V.length values == (fromIntegral $ V.last columnPointers)

prop_rowIndicesIncreasing :: Storable a => Matrix a -> Bool
prop_rowIndicesIncreasing mat = all (increasing . SpV.indices) (toColumns mat)
