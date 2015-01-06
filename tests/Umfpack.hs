module Main where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Test.Hspec
import Test.QuickCheck

import Data.Matrix.Sparse
import Numeric.LinearAlgebra.Umfpack
import Test.LinearAlgebra ()

main :: IO ()
main = hspec $ do
  describe "Numeric.LinearAlgebra.Umfpack" $ do

    it "ident <\\> v == v" $ property prop_linSolveId

prop_linSolveId :: Vector (Complex Double) -> Bool
prop_linSolveId v = ident (V.length v) <\> v == v
