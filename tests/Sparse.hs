module Main where

import Data.MonoTraversable
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Test.Hspec
import Test.QuickCheck

import Numeric.LinearAlgebra.Sparse
import Test.QuickCheck.Arbitrary.LinearAlgebra ()

main :: IO ()
main = hspec $ do
  describe "Numeric.LinearAlgebra.Sparse" $ do
    describe "kronecker" $ do
      it "assembes identity matrices" $ property prop_kroneckerIdent

    describe "takeDiag" $ do
      it "returns what diag is given" $ property prop_takeDiag

    describe "mulV" $ do
      it "identity" $ property prop_identMulV

    describe "lin" $ do
      it "additive inverse" $ property prop_addInv
      it "additive identity" $ property prop_addId

    describe "transpose" $ do
      it "self-inverse" $ property prop_transposeId
      it "preserves diagonal" $ property prop_transposeDiag

    describe "ctrans" $ do
      it "self-inverse" $ property prop_ctransId

    describe "mul" $ do
      it "identity on matrices" $ property prop_mulId

    describe "fromBlocks" $ do
      it "assembles identity matrices" $ property prop_fromBlocksId

    describe "fromBlocksDiag" $ do
      it "assembles identity matrices" $ property prop_fromBlocksDiagId

prop_kroneckerIdent :: Int -> Int -> Property
prop_kroneckerIdent x y = (x > 0 && y > 0) ==> lhs == rhs
  where
    lhs :: Matrix (Complex Double)
    lhs = kronecker (ident x) (ident y)
    rhs = ident (x * y)

prop_takeDiag :: Vector (Complex Double) -> Bool
prop_takeDiag v = takeDiag (diag v) == v

prop_identMulV :: Vector (Complex Double) -> Bool
prop_identMulV v = mulV identM v == v
  where
    identM = ident (V.length v)

prop_addInv :: Matrix (Complex Double) -> Bool
prop_addInv m = lin 1 m (-1) m == m0
  where
    m0 = omap (const 0) m

prop_addId :: Matrix (Complex Double) -> Bool
prop_addId m = add m (zeros (nRows m) (nColumns m)) == m

prop_transposeId :: Matrix (Complex Double) -> Bool
prop_transposeId m = transpose (transpose m) == m

prop_transposeDiag :: Vector Double -> Bool
prop_transposeDiag v = m == transpose m
  where
    m = diag v

prop_ctransId :: Matrix (Complex Double) -> Bool
prop_ctransId m = ctrans (ctrans m) == m

prop_mulId :: Matrix (Complex Double) -> Bool
prop_mulId m = m `mul` (ident $ nColumns m) == m

prop_fromBlocksId :: Int -> Int -> Property
prop_fromBlocksId x y = (x > 0 && y > 0) ==> lhs === ident (x + y)
  where
    lhs :: Matrix (Complex Double)
    lhs = fromBlocks [[Just (ident x), Nothing], [Nothing, Just (ident y)]]

prop_fromBlocksDiagId :: Int -> Int -> Property
prop_fromBlocksDiagId x y = (x > 0 && y > 0) ==> lhs === ident (x + y)
  where
    lhs :: Matrix (Complex Double)
    lhs = fromBlocksDiag [[Just (ident x), Just (ident y)], [Nothing, Nothing]]
