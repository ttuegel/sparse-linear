{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Vector.Storable as V
import Test.Hspec
import Test.QuickCheck

import Data.Matrix.Sparse
import Numeric.LinearAlgebra.Feast

(~==) :: (IsReal a, Eq a, Fractional a, Fractional (RealOf a), Num a, Ord (RealOf a), Show a, V.Storable a, V.Storable (RealOf a)) => V.Vector a -> V.Vector a -> Property
(~==) a b =
  counterexample (show a ++ " /= " ++ show b)
  $ V.and (V.zipWith (\x y -> x == y || x `closeEnoughTo` y) a b)
  where
    closeEnoughTo x y = mag (x - y) / mag (x + y) < 1E-10

main :: IO ()
main = hspec $ do
  describe "eigSH" $ do
    let m :: Matrix (Complex Double)
        m = fromTriples 2 2 [(0, 0, 2), (0, 1, -1), (1, 0, -1), (1, 1, 2)]
        params = defaultFeastParams { feastDebug = True }
        eigenvalues = fst $ eigSHParams params 2 (0, 4) m
        correct = V.fromList [1, 3]
    it "gives the correct number of eigenvalues"
      $ property (V.length eigenvalues === V.length correct)
    it "gives the correct eigenvalues"
      $ property (eigenvalues ~== correct)
