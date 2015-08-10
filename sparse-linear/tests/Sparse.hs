{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as V
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.QuickCheck

import Data.Matrix.Sparse
import Data.Matrix.Sparse.Foreign
import Test.LinearAlgebra

(~==) :: (IsReal a, Eq a, Fractional a, Fractional (RealOf a), Num a, Ord (RealOf a), Show a, Unbox a, Unbox (RealOf a)) => Matrix a -> Matrix a -> Property
(~==) a b =
  counterexample (show a ++ " /= " ++ show b)
  $ ncols a == ncols b
  && nrows a == nrows b
  && pointers a == pointers b
  && indices a == indices b
  && V.and (V.zipWith (\x y -> x == y || x `closeEnoughTo` y) (values a) (values b))
  where
    indices = fst . V.unzip . entries
    values = snd . V.unzip . entries
    closeEnoughTo x y = mag (x - y) / mag (x + y) < 1E-10

main :: IO ()
main = hspec $ do
  describe "Data.Matrix.Sparse" $ do
    describe "fromTriples" $ do
      checkMatrixR arbitrary
      checkMatrixZ arbitrary

    describe "kronecker" $ do
      it "assembles identity matrices" $ property $ do
        m <- arbdim
        n <- arbdim
        return $ kronecker (ident m) (ident n) === (ident (m * n) :: Matrix Double)

      checkMatrixR (kronecker <$> arbitrary <*> arbitrary)
      checkMatrixZ (kronecker <$> arbitrary <*> arbitrary)

    describe "diag" $ do
      it "takeDiag (diag v) == v" $ property $ do
        m <- arbdim
        v <- V.fromList <$> vectorOf m arbitrary
        return $ takeDiag (diag v) == (v :: Vector Double)

      checkMatrixR (diag <$> arbitrary)
      checkMatrixZ (diag <$> arbitrary)

    describe "mulV" $ do
      it "ident `mulV` v == v :: Matrix Double" $ property $ do
        m <- arbdim
        v <- V.fromList <$> vectorOf m arbitrary
        let mat :: Matrix Double
            mat = ident m
        return $ mat `mulV` v == v

      it "ident `mulV` v == v :: Matrix (Complex Double)" $ property $ do
        m <- arbdim
        v <- V.fromList <$> vectorOf m arbitrary
        let mat :: Matrix (Complex Double)
            mat = ident m
        return $ mat `mulV` v == v

      it "ident `mulV` v == v :: Matrix Double" $ property $ do
        m <- arbdim
        v <- V.fromList <$> vectorOf m arbitrary
        let mat :: Matrix Double
            mat = ident m
        return $ mat `mulV` v == v

      it "ident `mulV` v == v :: Matrix (Complex Double)" $ property $ do
        m <- arbdim
        v <- V.fromList <$> vectorOf m arbitrary
        let mat :: Matrix Double
            mat = ident m
        return $ mat `mulV` v == v

    describe "lin" $ do
      let addIdent
            :: (Arbitrary a, Eq a, Num a, Num (Matrix a), Unbox a)
            => Matrix a -> Bool
          addIdent a = a + zeros (nrows a) (ncols a) == a
      it "a + zeros == a :: Matrix Double"
        $ property (addIdent :: Matrix Double -> Bool)
      it "a + zeros == a :: Matrix (Complex Double)"
        $ property (addIdent :: Matrix (Complex Double) -> Bool)

      let addInv
            :: (Arbitrary a, Eq a, Num a, Num (Matrix a), Unbox a)
            => Matrix a -> Bool
          addInv a = a - a == cmap (const 0) a
      it "a - a `propTo` zeros :: Matrix Double"
        $ property (addInv :: Matrix Double -> Bool)
      it "a - a `propTo` zeros :: Matrix (Complex Double)"
        $ property (addInv :: Matrix (Complex Double) -> Bool)

      let addComm
            :: (Arbitrary a, Eq a, Num a, Num (Matrix a), Unbox a)
            => Matrix a -> Gen Bool
          addComm a = do
            b <- arbitraryMatrix (nrows a) (ncols a)
            return $ a + b == b + a
      it "a + b == b + a :: Matrix Double"
        $ property (addComm :: Matrix Double -> Gen Bool)
      it "a + b == b + a :: Matrix (Complex Double)"
        $ property (addComm :: Matrix (Complex Double) -> Gen Bool)

      let addAssoc
            :: (Arbitrary a, Eq a, Fractional a, Fractional (RealOf a), IsReal a, Num a, Num (Matrix a), Ord (RealOf a), Show a, Unbox a, Unbox (RealOf a))
            => Matrix a -> Gen Property
          addAssoc a = do
            b <- arbitraryMatrix (nrows a) (ncols a)
            c <- arbitraryMatrix (nrows a) (ncols a)
            return $ ((a + b) + c) ~== (a + (b + c))
      it "(a + b) + c == a + (b + c) :: Matrix Double"
        $ property (addAssoc :: Matrix Double -> Gen Property)
      it "(a + b) + c == a + (b + c) :: Matrix (Complex Double)"
        $ property (addAssoc :: Matrix (Complex Double) -> Gen Property)

      let arbitraryAdd
            :: (Arbitrary a, Num a, Num (Matrix a), Unbox a)
            => Gen (Matrix a)
          arbitraryAdd = do
            a <- arbitrary
            b <- arbitraryMatrix (nrows a) (ncols a)
            return $ a + b
      checkMatrixR arbitraryAdd
      checkMatrixZ arbitraryAdd

    describe "transpose" $ do
      it "transpose . diag == diag :: Matrix Double " $ property $ do
        v <- arbitrary
        let mat :: Matrix Double
            mat = diag v
        return $ transpose mat == mat
      it "transpose . diag == diag :: Matrix (Complex Double) " $ property $ do
        v <- arbitrary
        let mat :: Matrix (Complex Double)
            mat = diag v
        return $ transpose mat == mat

    describe "ctrans" $ do
      it "preserves hermitian matrices" $ do
        let m :: Matrix (Complex Double)
            m = fromTriples 2 2 [(0, 0, 2), (0, 1, -1), (1, 0, -1), (1, 1, 2)]
        m `shouldBe` ctrans m
      it "preserves sigma_x" $ do
        let m :: Matrix (Complex Double)
            m = fromTriples 2 2 [(0, 1, 1), (1, 0, 1)]
        m `shouldBe` ctrans m
      it "preserves sigma_y" $ do
        let m :: Matrix (Complex Double)
            m = fromTriples 2 2 [(0, 1, 0 :+ (-1)), (1, 0, 0 :+ 1)]
        m `shouldBe` ctrans m

    describe "mul" $ do
      let mulIdentL
            :: (Arbitrary a, Eq a, Num a, Num (Matrix a), Unbox a)
            => Matrix a -> Gen Bool
          mulIdentL a = do
            return $ ident (nrows a) * a == a
      it "ident * a == a :: Matrix Double"
        $ property (mulIdentL :: Matrix Double -> Gen Bool)
      it "ident * a == a :: Matrix (Complex Double)"
        $ property (mulIdentL :: Matrix (Complex Double) -> Gen Bool)

      let mulIdentR
            :: (Arbitrary a, Eq a, Num a, Num (Matrix a), Unbox a)
            => Matrix a -> Gen Bool
          mulIdentR a = do
            return $ a * ident (ncols a) == a
      it "ident * a == a :: Matrix Double"
        $ property (mulIdentR :: Matrix Double -> Gen Bool)
      it "ident * a == a :: Matrix (Complex Double)"
        $ property (mulIdentR :: Matrix (Complex Double) -> Gen Bool)

      let mulAssoc
            :: (Arbitrary a, Eq a, Fractional a, Fractional (RealOf a), IsReal a, Num a, Num (Matrix a), Ord (RealOf a), Show a, Unbox a, Unbox (RealOf a))
            => Matrix a -> Gen Property
          mulAssoc a = do
            let m = ncols a
            n <- arbdim
            b <- arbitraryMatrix m n
            p <- arbdim
            c <- arbitraryMatrix n p
            return $ ((a * b) * c) ~== (a * (b * c))
      it "(a * b) * c == a * (b * c) :: Matrix Double"
        $ property (mulAssoc :: Matrix Double -> Gen Property)
      it "(a * b) * c == a * (b * c) :: Matrix (Complex Double)"
        $ property (mulAssoc :: Matrix (Complex Double) -> Gen Property)

      let arbitraryMul
            :: (Arbitrary a, Num a, Num (Matrix a), Unbox a)
            => Gen (Matrix a)
          arbitraryMul = do
            m <- arbdim
            n <- arbdim
            a <- arbitraryMatrix m n
            p <- arbdim
            b <- arbitraryMatrix n p
            return $! a * b
      checkMatrixR arbitraryMul
      checkMatrixZ arbitraryMul

    describe "fromBlocksDiag" $ do
      let arbitraryFromBlocksDiag :: (Arbitrary a, Num a, Unbox a) => Gen (Matrix a)
          arbitraryFromBlocksDiag = do
            n <- arbdim
            mats <- vectorOf n arbitrary
            return $ fromBlocksDiag
              $ (map Just mats)
              : replicate (n - 1) (replicate n Nothing)
      checkMatrixR arbitraryFromBlocksDiag
      checkMatrixZ arbitraryFromBlocksDiag

  describe "Data.Matrix.Sparse.Foreign" $ do
    it "fromForeign . withConstMatrix == id (Double)"
      $ property (prop_withConstFromForeign :: Matrix Double -> Bool)
    it "fromForeign . withConstMatrix == id (Complex Double)"
      $ property (prop_withConstFromForeign :: Matrix (Complex Double) -> Bool)

prop_withConstFromForeign
  :: (Eq a, Num a, Storable a, Unbox a) => Matrix a -> Bool
prop_withConstFromForeign mat =
  unsafePerformIO (withConstMatrix mat $ fromForeign True) == mat
