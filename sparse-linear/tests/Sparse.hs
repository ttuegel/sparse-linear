{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.QuickCheck

import Data.Matrix.Sparse
import Data.Matrix.Sparse.Foreign
import Test.LinearAlgebra

main :: IO ()
main = hspec $ do
  describe "Data.Matrix.Sparse" $ do
    describe "fromTriples" (checkMatrix arbitrary)

    describe "kronecker" $ do
      it "assembles identity matrices" $ property $ do
        m <- arbdim
        n <- arbdim
        return $ kronecker (ident m) (ident n) === (ident (m * n) :: Matrix Vector Int)

      checkMatrix (kronecker <$> arbitrary <*> arbitrary)

    describe "diag" $ do
      it "takeDiag (diag v) == v" $ property $ do
        m <- arbdim
        v <- V.fromList <$> vectorOf m arbitrary
        return $ takeDiag (diag v) == (v :: Vector Int)

      checkMatrix (diag <$> arbitrary)

    describe "mulV" $ do
      it "ident `mulV` v == v" $ property $ do
        m <- arbdim
        v <- V.fromList <$> vectorOf m arbitrary
        let mat :: Matrix Vector Int
            mat = ident m
        return $ mat `mulV` v == v

    describe "addition" $ do
      it "a + zeros == a" prop_add_ident
      it "a - a == 0" prop_add_inv
      it "a + b == b + a" prop_add_commute
      it "a + (b + c) == (a + b) + c" prop_add_assoc
      checkMatrix $ (\(a, b) -> a + b) <$> arbitraryAdd2

    describe "transpose" $ do
      it "transpose . diag == diag" $ property $ do
        mat <- diag <$> arbitrary :: Gen (Matrix Vector Int)
        return (transpose mat === mat)

    describe "ctrans" $ do
      it "preserves hermitian matrices" $ do
        let m :: Matrix Vector (Complex Double)
            m = fromTriples 2 2 [(0, 0, 2), (0, 1, -1), (1, 0, -1), (1, 1, 2)]
        m `shouldBe` ctrans m
      it "preserves sigma_x" $ do
        let m :: Matrix Vector (Complex Double)
            m = fromTriples 2 2 [(0, 1, 1), (1, 0, 1)]
        m `shouldBe` ctrans m
      it "preserves sigma_y" $ do
        let m :: Matrix Vector (Complex Double)
            m = fromTriples 2 2 [(0, 1, 0 :+ (-1)), (1, 0, 0 :+ 1)]
        m `shouldBe` ctrans m

    describe "mul" $ do
      let mulIdentL :: Matrix Vector Int -> Property
          mulIdentL a = ident (nrows a) * a === a
      it "ident * a == a" (property mulIdentL)

      let mulIdentR :: Matrix Vector Int -> Property
          mulIdentR a = (a * ident (ncols a) === a)
      it "a * ident == a" (property mulIdentR)

      let mulAssoc :: Matrix Vector Int -> Gen Property
          mulAssoc a = do
            let m = ncols a
            n <- arbdim
            b <- arbitraryMatrix m n
            p <- arbdim
            c <- arbitraryMatrix n p
            return $ ((a * b) * c) === (a * (b * c))
      it "(a * b) * c == a * (b * c)" (property mulAssoc)

      let arbitraryMul :: Gen (Matrix Vector Int)
          arbitraryMul = do
            m <- arbdim
            n <- arbdim
            a <- arbitraryMatrix m n
            p <- arbdim
            b <- arbitraryMatrix n p
            return $! a * b
      checkMatrix arbitraryMul

    describe "fromBlocksDiag" $ do

      it "assembles identity matrices" $ property $ do
        m <- arbdim
        n <- arbdim
        let assembled = fromBlocksDiag
                        [ [Just (ident m), Just (ident n)]
                        , [Nothing, Nothing]
                        ]
        return $ assembled === (ident (m + n) :: Matrix Vector Int)

      let arbSymBlock :: Matrix Vector Double -> Gen Property
          arbSymBlock arbMN = do
            arbM <- arbitraryMatrix (nrows arbMN) (nrows arbMN)
            arbN <- arbitraryMatrix (ncols arbMN) (ncols arbMN)
            let arbSymM = arbM + ctrans arbM
                arbSymN = arbN + ctrans arbN
                assembled = fromBlocksDiag
                            [ [Just arbSymM, Just arbSymN]
                            , [Just arbMN, Just (ctrans arbMN)]
                            ]
            return $ assembled === ctrans assembled

      it "symmetric blockwise" (property arbSymBlock)

      let arbitraryFromBlocksDiag :: Gen (Matrix Vector Int)
          arbitraryFromBlocksDiag = do
            n <- arbdim
            mats <- vectorOf n arbitrary
            return $ fromBlocksDiag
              $ (map Just mats)
              : replicate (n - 1) (replicate n Nothing)
      checkMatrix arbitraryFromBlocksDiag

  describe "Data.Matrix.Sparse.Foreign" $ do
    it "fromForeign . withConstMatrix == id"
      (property (prop_withConstFromForeign :: Matrix Vector Int -> Bool))

prop_withConstFromForeign
  :: (Eq a, Num a, Storable a, Unbox a) => Matrix Vector a -> Bool
prop_withConstFromForeign mat =
  unsafePerformIO (withConstMatrix mat $ fromForeign True) == mat

prop_add_commute :: Property
prop_add_commute = property $ do
  (a, b) <- arbitraryAdd2
  return (a + b === b + a)

prop_add_inv :: Property
prop_add_inv = property $ do
  a <- arbitrary :: Gen (Matrix Vector Int)
  return (a - a === cmap (const 0) a)

prop_add_ident :: Property
prop_add_ident = property $ do
  a <- arbitrary :: Gen (Matrix Vector Int)
  return (a + zeros (nrows a) (ncols a) === a)

prop_add_assoc :: Property
prop_add_assoc = property $ do
  (a, b, c) <- arbitraryAdd3
  return (a + (b + c) === (a + b) + c)

arbitraryAdd2 :: Gen (Matrix Vector Int, Matrix Vector Int)
arbitraryAdd2 = do
  a <- arbitrary
  b <- arbitraryMatrix (nrows a) (ncols a)
  return (a, b)

arbitraryAdd3 :: Gen (Matrix Vector Int, Matrix Vector Int, Matrix Vector Int)
arbitraryAdd3 = do
  a <- arbitrary
  b <- arbitraryMatrix (nrows a) (ncols a)
  c <- arbitraryMatrix (nrows a) (ncols a)
  return (a, b, c)
