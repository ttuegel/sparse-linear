module Main where

import qualified Data.Vector.Storable as V
import Test.Hspec
import Test.QuickCheck

import Numeric.LinearAlgebra.Sparse

main :: IO ()
main = hspec $ do
  describe "Numeric.LinearAlgebra.Sparse.kronecker" $ do
    it "kronecker (ident x) (ident y) == ident (x * y)"
      $ property $ \x y -> (x > 0 && y > 0) ==>
          let lhs :: Matrix (Complex Double)
              lhs = kronecker (ident x) (ident y)
              rhs = ident (x * y)
          in lhs == rhs
  describe "Numeric.LinearAlgebra.Sparse.diag" $ do
    it "takeDiag . diag == id"
      $ property $ \xs -> length xs > 0 ==>
        V.toList (takeDiag $ diag $ V.fromList xs) == (xs :: [Complex Double])
  describe "Numeric.LinearAlgebra.Sparse.mulV" $ do
    it "ident n `mulV` xs == xs"
      $ property $ \xs -> length xs > 0 ==>
        let v = V.fromList xs
            m :: Matrix (Complex Double)
            m = ident $ length xs
        in mulV m v == v
  describe "Numeric.LinearAlgebra.Sparse.lin" $ do
    it "lin 1 a (-1) a == 0"
      $ property $ \xs -> length xs > 0 ==>
        let m :: Matrix (Complex Double)
            m = diag $ V.fromList xs
            m0 = cmap (const 0) m
        in lin 1 m (-1) m == m0
  describe "Numeric.LinearAlgebra.Sparse.transpose" $ do
    it "transpose . transpose == id"
      $ property $ \xs -> length xs > 0 ==>
        let m :: Matrix (Complex Double)
            m = diag $ V.fromList xs
        in transpose (transpose m) == m
  describe "Numeric.LinearAlgebra.Sparse.ctrans" $ do
    it "ctrans . ctrans == id"
      $ property $ \xs -> length xs > 0 ==>
        let m :: Matrix (Complex Double)
            m = diag $ V.fromList xs
        in ctrans (ctrans m) == m
