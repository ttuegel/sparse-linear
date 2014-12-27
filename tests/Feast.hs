module Main where

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Unboxed as U
import Test.Hspec

import Numeric.LinearAlgebra.Feast
import Numeric.LinearAlgebra.Sparse

main :: IO ()
main = hspec $ do
  describe "Numeric.LinearAlgebra.Feast" $ do
    it "ident" $ do
      let m :: Matrix (Complex Double)
          m = compress 2 2 $ U.fromList [(0, 0, 2), (0, 1, -1), (1, 0, -1), (1, 1, 2)]
          eigenvalues = fst $ eigH 2 (-5, 5) m
          res = V.map abs $ V.zipWith (-) eigenvalues (V.fromList [1, 3])
      V.all (< 1E-12) res `shouldBe` True
