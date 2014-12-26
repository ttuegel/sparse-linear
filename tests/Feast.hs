module Main where

import qualified Data.Vector.Storable as V
import Test.Hspec

import Numeric.LinearAlgebra.Feast
import Numeric.LinearAlgebra.Sparse

main :: IO ()
main = do
    let m :: Matrix (Complex Double)
        m = ident 5
    print $ eigH 4 (0, 2) m
{-
main = hspec $ do
  describe "Numeric.LinearAlgebra.Feast" $ do
    it "ident" $ do
      let m :: Matrix (Complex Double)
          m = ident 5
      fst (eigH 4 (0, 2) m) `shouldBe` V.replicate 4 1
-}
