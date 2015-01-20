{-# LANGUAGE DataKinds #-}

module Main where

import qualified Data.Vector.Storable as V
import Test.Hspec

import Data.Matrix.Sparse
import Numeric.LinearAlgebra.Feast

main :: IO ()
main = hspec $ do
  describe "Numeric.LinearAlgebra.Feast" $ do
    it "ident :: Matrix Col (Complex Double)" $ do
      let m :: Matrix Col (Complex Double)
          m = fromTriples 2 2 [(0, 0, 2), (0, 1, -1), (1, 0, -1), (1, 1, 2)]
          eigenvalues = fst $ eigSH 2 (0, 4) m
          correct = V.fromList [1, 3]
      V.length eigenvalues `shouldBe` V.length correct
      V.all (< 1E-12) (V.map abs $ V.zipWith (-) eigenvalues correct) `shouldBe` True
