{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.LinearAlgebra where

import Data.Traversable
import Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as V
import Test.QuickCheck

import Data.Matrix.Sparse
import qualified Data.Vector.Sparse as SpV
import Data.Vector.Util (increasing, nondecreasing)
import Numeric.LinearAlgebra.Sparse

instance (Arbitrary a, Storable a) => Arbitrary (Vector a) where
    arbitrary = fmap V.fromList $ suchThat arbitrary $ \v -> length v > 0

instance (Arbitrary a, CxSparse a) => Arbitrary (Matrix a) where
    arbitrary = do
      nr <- arbitrary `suchThat` (> 0)
      let nc = nr
      triples <- forM [0..(nr * nr `div` 4)] $ \_ -> do
        r <- arbitrary `suchThat` (\r -> r >= 0 && r < nr)
        c <- arbitrary `suchThat` (\c -> c >= 0 && c < nc)
        x <- arbitrary
        return (r, c, x)
      return $ fromTriples nr nc triples

prop_columnPointersNondecreasing :: Matrix a -> Bool
prop_columnPointersNondecreasing Matrix{..} = nondecreasing columnPointers

prop_columnPointersLength :: Matrix a -> Bool
prop_columnPointersLength Matrix{..} = V.length columnPointers == nColumns + 1

prop_valuesLength :: Storable a => Matrix a -> Bool
prop_valuesLength Matrix{..} =
  V.length values == (fromIntegral $ V.last columnPointers)

prop_rowIndicesIncreasing :: Storable a => Matrix a -> Bool
prop_rowIndicesIncreasing mat = all (increasing . SpV.indices) (toColumns mat)

prop_rowIndicesNonNegative :: Storable a => Matrix a -> Bool
prop_rowIndicesNonNegative mat = V.all (>= 0) (rowIndices mat)

prop_rowIndicesInRange :: Storable a => Matrix a -> Bool
prop_rowIndicesInRange mat = V.all (< nr) (rowIndices mat)
  where nr = fromIntegral $ nRows mat
