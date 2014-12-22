module Test.QuickCheck.Arbitrary.LinearAlgebra where

import Data.Traversable
import Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as U
import Test.QuickCheck

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
      xs <- forM ixs $ \(r, c) -> do
        x <- arbitrary
        return (r, c, x)
      return $ compress nr nc $ U.fromList xs


