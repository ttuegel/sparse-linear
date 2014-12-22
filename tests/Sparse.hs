module Main where

import Data.Traversable
import Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as U
import Test.Hspec
import Test.QuickCheck

import Numeric.LinearAlgebra.Sparse

main :: IO ()
main = hspec $ do
  describe "Numeric.LinearAlgebra.Sparse" $ do

    it "kronecker (ident x) (ident y) == ident (x * y)"
      $ property prop_kroneckerIdent

    it "takeDiag . diag == id" $ property prop_takeDiag

    it "ident n `mulV` xs == xs" $ property prop_identMulV

    it "lin 1 a (-1) a == 0" $ property prop_addInv

    it "a `add` zeros == a" $ property prop_addId

    it "transpose . transpose == id" $ property prop_transposeId

    it "ctrans . ctrans == id" $ property prop_ctransId

    it "a `mul` ident == a" $ property prop_mulId

prop_kroneckerIdent :: Int -> Int -> Property
prop_kroneckerIdent x y = (x > 0 && y > 0) ==> lhs == rhs
  where
    lhs :: Matrix (Complex Double)
    lhs = kronecker (ident x) (ident y)
    rhs = ident (x * y)

prop_takeDiag :: Vector (Complex Double) -> Bool
prop_takeDiag v = takeDiag (diag v) == v

prop_identMulV :: Vector (Complex Double) -> Bool
prop_identMulV v = mulV identM v == v
  where
    identM = ident (V.length v)

prop_addInv :: Matrix (Complex Double) -> Bool
prop_addInv m = lin 1 m (-1) m == m0
  where
    m0 = cmap (const 0) m

prop_addId :: Matrix (Complex Double) -> Bool
prop_addId m = add m (zeros (nrows m) (ncols m)) == m

prop_transposeId :: Matrix (Complex Double) -> Bool
prop_transposeId m = transpose (transpose m) == m

prop_ctransId :: Matrix (Complex Double) -> Bool
prop_ctransId m = ctrans (ctrans m) == m

prop_mulId :: Matrix (Complex Double) -> Bool
prop_mulId m = m `mul` (ident $ ncols m) == m

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
