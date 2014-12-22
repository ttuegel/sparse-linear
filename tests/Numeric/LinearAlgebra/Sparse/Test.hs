module Main where

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
