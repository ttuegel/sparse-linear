{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=500 #-}

module Main where

import Control.Applicative
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as V
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.QuickCheck

import Data.Matrix.Sparse
import Data.Matrix.Sparse.Foreign
import Test.LinearAlgebra

(~==) :: (IsReal a, Eq a, Fractional a, Fractional (RealOf a), Num a, Ord (RealOf a), Show a, Unbox a, Unbox (RealOf a)) => Matrix or a -> Matrix or a -> Property
(~==) a b =
  counterexample (show a ++ " /= " ++ show b)
  $ odim a == odim b
  && idim a == idim b
  && pointers a == pointers b
  && indices a == indices b
  && V.all (< 1E-10) (V.map (\x -> 1 - mag x) $ V.zipWith (/) (values a) (values b))
  where
    indices = fst . V.unzip . entries
    values = snd . V.unzip . entries

main :: IO ()
main = hspec $ do
  describe "Data.Matrix.Sparse" $ do
    describe "fromTriples" $ do
      checkMatrixRowR arbitrary
      checkMatrixRowZ arbitrary
      checkMatrixColR arbitrary
      checkMatrixColZ arbitrary

    describe "kronecker" $ do
      it "assembles identity matrices" $ property $ do
        m <- arbdim
        n <- arbdim
        return $ kronecker (ident m) (ident n) === (ident (m * n) :: Matrix Row Double)

      let arbitraryKronecker
            :: (Arbitrary a, Num a, Orient or, Unbox a) => Gen (Matrix or a)
          arbitraryKronecker = kronecker <$> arbitrary <*> arbitrary
      checkMatrixRowR arbitraryKronecker
      checkMatrixRowZ arbitraryKronecker
      checkMatrixColR arbitraryKronecker
      checkMatrixColZ arbitraryKronecker

    describe "diag" $ do
      it "takeDiag (diag v) == v" $ property $ do
        m <- arbdim
        v <- V.fromList <$> vectorOf m arbitrary
        return $ takeDiag (diag v) == (v :: Vector Double)

      let arbitraryDiag
            :: (Arbitrary a, Eq a, Num a, Orient or, Unbox a)
            => Gen (Matrix or a)
          arbitraryDiag = diag <$> arbitrary
      checkMatrixRowR arbitraryDiag
      checkMatrixRowZ arbitraryDiag
      checkMatrixColR arbitraryDiag
      checkMatrixColZ arbitraryDiag

    describe "mulV" $ do
      it "ident `mulV` v == v :: Matrix Row Double" $ property $ do
        m <- arbdim
        v <- V.fromList <$> vectorOf m arbitrary
        let mat :: Matrix Row Double
            mat = ident m
        return $ mat `mulV` v == v

      it "ident `mulV` v == v :: Matrix Row (Complex Double)" $ property $ do
        m <- arbdim
        v <- V.fromList <$> vectorOf m arbitrary
        let mat :: Matrix Row (Complex Double)
            mat = ident m
        return $ mat `mulV` v == v

      it "ident `mulV` v == v :: Matrix Col Double" $ property $ do
        m <- arbdim
        v <- V.fromList <$> vectorOf m arbitrary
        let mat :: Matrix Col Double
            mat = ident m
        return $ mat `mulV` v == v

      it "ident `mulV` v == v :: Matrix Col (Complex Double)" $ property $ do
        m <- arbdim
        v <- V.fromList <$> vectorOf m arbitrary
        let mat :: Matrix Col Double
            mat = ident m
        return $ mat `mulV` v == v

    describe "lin" $ do
      let addIdent
            :: (Arbitrary a, Eq a, Num a, Num (Matrix or a), Orient or, Unbox a)
            => Matrix or a -> Bool
          addIdent a =
            let (rdim, cdim) = orientSwap (orient a) (odim a, idim a)
            in a + zeros rdim cdim == a
      it "a + zeros == a :: Matrix Row Double"
        $ property (addIdent :: Matrix Row Double -> Bool)
      it "a + zeros == a :: Matrix Row (Complex Double)"
        $ property (addIdent :: Matrix Row (Complex Double) -> Bool)
      it "a + zeros == a :: Matrix Col Double"
        $ property (addIdent :: Matrix Col Double -> Bool)
      it "a + zeros == a :: Matrix Col (Complex Double)"
        $ property (addIdent :: Matrix Col (Complex Double) -> Bool)

      let addInv
            :: (Arbitrary a, Eq a, Num a, Num (Matrix or a), Orient or, Unbox a)
            => Matrix or a -> Bool
          addInv a = a - a == cmap (const 0) a
      it "a - a `propTo` zeros :: Matrix Row Double"
        $ property (addInv :: Matrix Row Double -> Bool)
      it "a - a `propTo` zeros :: Matrix Row (Complex Double)"
        $ property (addInv :: Matrix Row (Complex Double) -> Bool)
      it "a - a `propTo` zeros :: Matrix Col Double"
        $ property (addInv :: Matrix Col Double -> Bool)
      it "a - a `propTo` zeros :: Matrix Col (Complex Double)"
        $ property (addInv :: Matrix Col (Complex Double) -> Bool)

      let addComm
            :: (Arbitrary a, Eq a, Num a, Num (Matrix or a), Orient or, Unbox a)
            => Matrix or a -> Gen Bool
          addComm a = do
            let (rdim, cdim) = orientSwap (orient a) (odim a, idim a)
            b <- arbitraryMatrix rdim cdim
            return $ a + b == b + a
      it "a + b == b + a :: Matrix Row Double"
        $ property (addComm :: Matrix Row Double -> Gen Bool)
      it "a + b == b + a :: Matrix Row (Complex Double)"
        $ property (addComm :: Matrix Row (Complex Double) -> Gen Bool)
      it "a + b == b + a :: Matrix Col Double"
        $ property (addComm :: Matrix Col Double -> Gen Bool)
      it "a + b == b + a :: Matrix Col (Complex Double)"
        $ property (addComm :: Matrix Col (Complex Double) -> Gen Bool)

      let addAssoc
            :: (Arbitrary a, Eq a, Fractional a, Fractional (RealOf a), IsReal a, Num a, Num (Matrix or a), Ord (RealOf a), Orient or, Show a, Unbox a, Unbox (RealOf a))
            => Matrix or a -> Gen Property
          addAssoc a = do
            let (rdim, cdim) = orientSwap (orient a) (odim a, idim a)
            b <- arbitraryMatrix rdim cdim
            c <- arbitraryMatrix rdim cdim
            return $ ((a + b) + c) ~== (a + (b + c))
      it "(a + b) + c == a + (b + c) :: Matrix Row Double"
        $ property (addAssoc :: Matrix Row Double -> Gen Property)
      it "(a + b) + c == a + (b + c) :: Matrix Row (Complex Double)"
        $ property (addAssoc :: Matrix Row (Complex Double) -> Gen Property)
      it "(a + b) + c == a + (b + c) :: Matrix Col Double"
        $ property (addAssoc :: Matrix Col Double -> Gen Property)
      it "(a + b) + c == a + (b + c) :: Matrix Col (Complex Double)"
        $ property (addAssoc :: Matrix Col (Complex Double) -> Gen Property)

      let arbitraryAdd
            :: (Arbitrary a, Num a, Num (Matrix or a), Orient or, Unbox a)
            => Gen (Matrix or a)
          arbitraryAdd = do
            a <- arbitrary
            let (rdim, cdim) = orientSwap (orient a) (odim a, idim a)
            b <- arbitraryMatrix rdim cdim
            return $ a + b
      checkMatrixRowR arbitraryAdd
      checkMatrixRowZ arbitraryAdd
      checkMatrixColR arbitraryAdd
      checkMatrixColZ arbitraryAdd

    describe "transpose" $ do
      it "transpose . diag == diag :: Matrix Row Double " $ property $ do
        v <- arbitrary
        let mat :: Matrix Row Double
            mat = diag v
        return $ reorient (transpose mat) == mat
      it "transpose . diag == diag :: Matrix Row (Complex Double) " $ property $ do
        v <- arbitrary
        let mat :: Matrix Row (Complex Double)
            mat = diag v
        return $ reorient (transpose mat) == mat
      it "transpose . diag == diag :: Matrix Col Double " $ property $ do
        v <- arbitrary
        let mat :: Matrix Col Double
            mat = diag v
        return $ reorient (transpose mat) == mat
      it "transpose . diag == diag :: Matrix Col (Complex Double) " $ property $ do
        v <- arbitrary
        let mat :: Matrix Col (Complex Double)
            mat = diag v
        return $ reorient (transpose mat) == mat

    describe "ctrans" $ do
      it "preserves hermitian matrices" $ do
        let m :: Matrix Col (Complex Double)
            m = fromTriples 2 2 [(0, 0, 2), (0, 1, -1), (1, 0, -1), (1, 1, 2)]
        m `shouldBe` reorient (ctrans m)
      it "preserves sigma_x" $ do
        let m :: Matrix Col (Complex Double)
            m = fromTriples 2 2 [(0, 1, 1), (1, 0, 1)]
        m `shouldBe` reorient (ctrans m)
      it "preserves sigma_y" $ do
        let m :: Matrix Col (Complex Double)
            m = fromTriples 2 2 [(0, 1, 0 :+ (-1)), (1, 0, 0 :+ 1)]
        m `shouldBe` reorient (ctrans m)

    describe "mul" $ do
      let mulIdentL
            :: (Arbitrary a, Eq a, Num a, Num (Matrix or a), Orient or, Unbox a)
            => Matrix or a -> Gen Bool
          mulIdentL a = do
            let (rdim, _) = orientSwap (orient a) (odim a, idim a)
            return $ ident rdim * a == a
      it "ident * a == a :: Matrix Row Double"
        $ property (mulIdentL :: Matrix Row Double -> Gen Bool)
      it "ident * a == a :: Matrix Row (Complex Double)"
        $ property (mulIdentL :: Matrix Row (Complex Double) -> Gen Bool)
      it "ident * a == a :: Matrix Col Double"
        $ property (mulIdentL :: Matrix Col Double -> Gen Bool)
      it "ident * a == a :: Matrix Col (Complex Double)"
        $ property (mulIdentL :: Matrix Col (Complex Double) -> Gen Bool)

      let mulIdentR
            :: (Arbitrary a, Eq a, Num a, Num (Matrix or a), Orient or, Unbox a)
            => Matrix or a -> Gen Bool
          mulIdentR a = do
            let (_, cdim) = orientSwap (orient a) (odim a, idim a)
            return $ a * ident cdim == a
      it "ident * a == a :: Matrix Row Double"
        $ property (mulIdentR :: Matrix Row Double -> Gen Bool)
      it "ident * a == a :: Matrix Row (Complex Double)"
        $ property (mulIdentR :: Matrix Row (Complex Double) -> Gen Bool)
      it "ident * a == a :: Matrix Col Double"
        $ property (mulIdentR :: Matrix Col Double -> Gen Bool)
      it "ident * a == a :: Matrix Col (Complex Double)"
        $ property (mulIdentR :: Matrix Col (Complex Double) -> Gen Bool)

      let mulAssoc
            :: (Arbitrary a, Eq a, Fractional a, Fractional (RealOf a), IsReal a, Num a, Num (Matrix or a), Ord (RealOf a), Orient or, Show a, Unbox a, Unbox (RealOf a))
            => Matrix or a -> Gen Property
          mulAssoc a = do
            let (_, m) = orientSwap (orient a) (odim a, idim a)
            n <- arbdim
            b <- arbitraryMatrix m n
            p <- arbdim
            c <- arbitraryMatrix n p
            return $ ((a * b) * c) ~== (a * (b * c))
      it "(a * b) * c == a * (b * c) :: Matrix Row Double"
        $ property (mulAssoc :: Matrix Row Double -> Gen Property)
      it "(a * b) * c == a * (b * c) :: Matrix Row (Complex Double)"
        $ property (mulAssoc :: Matrix Row (Complex Double) -> Gen Property)
      it "(a * b) * c == a * (b * c) :: Matrix Col Double"
        $ property (mulAssoc :: Matrix Col Double -> Gen Property)
      it "(a * b) * c == a * (b * c) :: Matrix Col (Complex Double)"
        $ property (mulAssoc :: Matrix Col (Complex Double) -> Gen Property)

      let arbitraryMul
            :: (Arbitrary a, Num a, Num (Matrix or a), Orient or, Unbox a)
            => Gen (Matrix or a)
          arbitraryMul = do
            m <- arbdim
            n <- arbdim
            a <- arbitraryMatrix m n
            p <- arbdim
            b <- arbitraryMatrix n p
            return $! a * b
      checkMatrixRowR arbitraryMul
      checkMatrixRowZ arbitraryMul
      checkMatrixColR arbitraryMul
      checkMatrixColZ arbitraryMul

    describe "fromBlocksDiag" $ do
      let arbitraryFromBlocksDiag
            :: (Arbitrary a, Num a, Unbox a) => Gen (Matrix Row a)
          arbitraryFromBlocksDiag = do
            n <- arbdim
            mats <- vectorOf n arbitrary
            return $ fromBlocksDiag
              $ (map Just mats)
              : replicate (n - 1) (replicate n Nothing)
      checkMatrixRowR arbitraryFromBlocksDiag
      checkMatrixRowZ arbitraryFromBlocksDiag

  describe "Data.Matrix.Sparse.Foreign" $ do
    it "fromForeign . withConstMatrix == id (Double)"
      $ property (prop_withConstFromForeign :: Matrix Col Double -> Bool)
    it "fromForeign . withConstMatrix == id (Complex Double)"
      $ property (prop_withConstFromForeign :: Matrix Col (Complex Double) -> Bool)

prop_withConstFromForeign
  :: (Eq a, Num a, Storable a, Unbox a) => Matrix Col a -> Bool
prop_withConstFromForeign mat =
  unsafePerformIO (withConstMatrix mat $ fromForeign True) == mat
