module Main where

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Test.Hspec
import Test.QuickCheck

import Numeric.LinearAlgebra.Sparse
import Numeric.LinearAlgebra.Umfpack
import Test.QuickCheck.Arbitrary.LinearAlgebra ()

main :: IO ()
main = hspec $ do
  describe "Numeric.LinearAlgebra.Umfpack" $ do

    it "ident <\\> v == v" $ property prop_linSolveId

prop_linSolveId :: Vector (Complex Double) -> Bool
prop_linSolveId v = ident (V.length v) <\> v == v
