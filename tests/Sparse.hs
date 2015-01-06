{-# LANGUAGE DataKinds #-}

module Main where

import Data.MonoTraversable
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as V
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.QuickCheck

import Data.Matrix.Sparse
import Data.Matrix.Sparse.Foreign
import Test.LinearAlgebra

main :: IO ()
main = hspec $ do
  describe "Numeric.LinearAlgebra.Sparse" $ do
    describe "fromTriples -> Matrix Col Double"
      $ checkFunMat1 (id :: Matrix Col Double -> Matrix Col Double)
    describe "fromTriples -> Matrix Row Double"
      $ checkFunMat1 (id :: Matrix Row Double -> Matrix Row Double)
    describe "fromTriples -> Matrix Col (Complex Double)"
      $ checkFunMat1 (id :: Matrix Col (Complex Double) -> Matrix Col (Complex Double))
    describe "fromTriples -> Matrix Row (Complex Double)"
      $ checkFunMat1 (id :: Matrix Row (Complex Double) -> Matrix Row (Complex Double))

    describe "kronecker" $ do
      it "assembles identity matrices" $
        property prop_kroneckerIdent
      it "row indices increasing" $
        property prop_kroneckerIndicesIncreasing
      it "column pointers nondecreasing" $
        property prop_kroneckerPointersNondecreasing
      it "column pointers length" $
        property prop_kroneckerPointersLength
      it "values length" $
        property prop_kroneckerValuesLength

    describe "takeDiag" $ do
      it "returns what diag is given" $ property prop_takeDiag

    describe "mulV" $ do
      it "identity" $ property prop_identMulV

    describe "lin" $ do
      it "additive inverse" $
        property prop_addInv
      it "additive identity" $
        property prop_addId
      it "row indices increasing" $
        property prop_linIndicesIncreasing
      it "column pointers nondecreasing" $
        property prop_linPointersNondecreasing
      it "column pointers length" $
        property prop_linPointersLength
      it "values length" $
        property prop_linValuesLength

    describe "transpose" $ do
      it "self-inverse" $ property prop_transposeId
      it "preserves diagonal" $ property prop_transposeDiag

      describe "transpose -> Matrix Col Double"
        $ checkFunMat1 (transpose :: Matrix Row Double -> Matrix Col Double)
      describe "transpose -> Matrix Row Double"
        $ checkFunMat1 (transpose :: Matrix Col Double -> Matrix Row Double)
      describe "transpose -> Matrix Col (Complex Double)"
        $ checkFunMat1 (transpose :: Matrix Row (Complex Double) -> Matrix Col (Complex Double))
      describe "transpose -> Matrix Row (Complex Double)"
        $ checkFunMat1 (transpose :: Matrix Col (Complex Double) -> Matrix Row (Complex Double))

    describe "ctrans" $ do
      it "self-inverse" $ property prop_ctransId
      it "preserves diagonal of real matrices" $ property prop_ctransDiag
      it "preserves hermitian matrices" $ do
        let m :: Matrix Col (Complex Double)
            m = fromTriples 2 2 [(0, 0, 2), (0, 1, -1), (1, 0, -1), (1, 1, 2)]
        m `shouldBe` reorient (ctrans m)
      it "preserves sigma_x" $ do
        let m :: Matrix Col (Complex Double)
            m = fromTriples 2 2 [(0, 1, 1), (1, 0, 1)]
        m `shouldBe` reorient (ctrans m)
      it "preserves sigma_y" $ do
        let m :: Matrix Col (Complex Double)
            m = fromTriples 2 2 [(0, 1, 0 :+ (-1)), (1, 0, 0 :+ 1)]
        m `shouldBe` reorient (ctrans m)

    describe "mul" $ do
      it "identity on matrices" $ property prop_mulId

    describe "fromBlocks" $ do
      it "assembles identity matrices" $
        property prop_fromBlocksId
      it "row indices increasing" $
        property prop_fromBlocksIndicesIncreasing
      it "column pointers nondecreasing"
        $ property prop_fromBlocksPointersNondecreasing
      it "column pointers length" $
        property prop_fromBlocksPointersLength
      it "values length" $
        property prop_fromBlocksValuesLength

    describe "fromBlocksDiag" $ do
      it "assembles identity matrices" $
        property prop_fromBlocksDiagId
      it "row indices increasing" $
        property prop_fromBlocksDiagIndicesIncreasing
      it "column pointers nondecreasing" $
        property prop_fromBlocksDiagPointersNondecreasing
      it "column pointers length" $
        property prop_fromBlocksDiagPointersLength
      it "values length" $
        property prop_fromBlocksDiagValuesLength
      it "produces hermitian matrices" $
        property prop_fromBlocksDiagHermitian

    describe "Data.Matrix.Sparse.Foreign" $ do
      it "fromForeign . withConstMatrix == id (Double)"
        $ property (prop_withConstFromForeign :: Matrix Col Double -> Bool)
      it "fromForeign . withConstMatrix == id (Complex Double)"
        $ property (prop_withConstFromForeign :: Matrix Col (Complex Double) -> Bool)

prop_kroneckerIdent :: Int -> Int -> Property
prop_kroneckerIdent x y = (x > 0 && y > 0) ==> lhs == rhs
  where
    lhs :: Matrix Col (Complex Double)
    lhs = kronecker (ident x) (ident y)
    rhs = ident (x * y)

prop_kroneckerIndicesIncreasing
  :: Matrix Col (Complex Double) -> Matrix Col (Complex Double) -> Bool
prop_kroneckerIndicesIncreasing a b =
  prop_indicesIncreasing $ kronecker a b

prop_kroneckerPointersNondecreasing
  :: Matrix Col (Complex Double) -> Matrix Col (Complex Double) -> Bool
prop_kroneckerPointersNondecreasing a b =
  prop_pointersNondecreasing $ kronecker a b

prop_kroneckerPointersLength
  :: Matrix Col (Complex Double) -> Matrix Col (Complex Double) -> Bool
prop_kroneckerPointersLength a b =
  prop_pointersLength $ kronecker a b

prop_kroneckerValuesLength
  :: Matrix Col (Complex Double) -> Matrix Col (Complex Double) -> Property
prop_kroneckerValuesLength a b =
  let k = kronecker a b
  in counterexample (show k) $ prop_valuesLength k

prop_takeDiag :: Vector (Complex Double) -> Bool
prop_takeDiag v = takeDiag (diag v) == v

prop_identMulV :: Vector (Complex Double) -> Bool
prop_identMulV v = mulV identM v == v
  where
    identM :: Matrix Row (Complex Double)
    identM = ident (V.length v)

prop_addInv :: Matrix Col (Complex Double) -> Property
prop_addInv m = counterexample (show subtracted) (subtracted == m0)
  where
    subtracted = lin 1 m (-1) m
    m0 = omap (const 0) m

prop_addId :: Matrix Col (Complex Double) -> Bool
prop_addId m = add m (zeros (minDim m) (majDim m)) == m

prop_linIndicesIncreasing
  :: Complex Double -> Matrix Col (Complex Double) -> Bool
prop_linIndicesIncreasing c mat =
  prop_indicesIncreasing
  $ lin c mat (0.5683358478038576 :+ 1.9175490512894502) mat

prop_linPointersNondecreasing
  :: Complex Double -> Matrix Col (Complex Double) -> Bool
prop_linPointersNondecreasing c mat =
  prop_pointersNondecreasing $ lin c mat (-1) mat

prop_linPointersLength
  :: Complex Double -> Matrix Col (Complex Double) -> Bool
prop_linPointersLength c mat =
  prop_pointersLength $ lin c mat (-1) mat

prop_linValuesLength
  :: Complex Double -> Matrix Col (Complex Double) -> Bool
prop_linValuesLength c mat =
  prop_valuesLength $ lin c mat (-1) mat

prop_transposeId :: Matrix Col (Complex Double) -> Bool
prop_transposeId m = transpose (transpose m) == m

prop_transposeDiag :: Vector Double -> Bool
prop_transposeDiag v = m == transpose m
  where
    m = diag v

prop_ctransId :: Matrix Col (Complex Double) -> Bool
prop_ctransId m = ctrans (ctrans m) == m

prop_ctransDiag :: Vector Double -> Bool
prop_ctransDiag v = m == ctrans m
  where
    m = diag v

prop_mulId :: Matrix Col (Complex Double) -> Property
prop_mulId m = counterexample (show n) $ m == n
  where n = m `mul` (ident $ majDim m)

prop_fromBlocksId :: Int -> Int -> Property
prop_fromBlocksId x y = (x > 0 && y > 0) ==> lhs === ident (x + y)
  where
    lhs :: Matrix Row (Complex Double)
    lhs = fromBlocks [[Just (ident x), Nothing], [Nothing, Just (ident y)]]

prop_fromBlocksIndicesIncreasing
  :: Matrix Col (Complex Double) -> Matrix Col (Complex Double) -> Bool
prop_fromBlocksIndicesIncreasing x y =
  prop_indicesIncreasing $ fromBlocks [[Just x, Nothing], [Nothing, Just y]]

prop_fromBlocksPointersNondecreasing
  :: Matrix Col (Complex Double) -> Matrix Col (Complex Double) -> Bool
prop_fromBlocksPointersNondecreasing x y =
  prop_pointersNondecreasing
  $ fromBlocks [[Just x, Nothing], [Nothing, Just y]]

prop_fromBlocksPointersLength
  :: Matrix Col (Complex Double) -> Matrix Col (Complex Double) -> Bool
prop_fromBlocksPointersLength x y =
  prop_pointersLength
  $ fromBlocks [[Just x, Nothing], [Nothing, Just y]]

prop_fromBlocksValuesLength
  :: Matrix Col (Complex Double) -> Matrix Col (Complex Double) -> Bool
prop_fromBlocksValuesLength x y =
  prop_valuesLength
  $ fromBlocks [[Just x, Nothing], [Nothing, Just y]]

prop_fromBlocksDiagId :: Int -> Int -> Property
prop_fromBlocksDiagId x y = (x > 0 && y > 0) ==> lhs === ident (x + y)
  where
    lhs :: Matrix Row (Complex Double)
    lhs = fromBlocksDiag [[Just (ident x), Just (ident y)], [Nothing, Nothing]]

prop_fromBlocksDiagIndicesIncreasing
  :: Matrix Col (Complex Double) -> Matrix Col (Complex Double) -> Bool
prop_fromBlocksDiagIndicesIncreasing x y =
  prop_indicesIncreasing
  $ fromBlocksDiag [[Just x, Just y], [Nothing, Nothing]]

prop_fromBlocksDiagPointersNondecreasing
  :: Matrix Col (Complex Double) -> Matrix Col (Complex Double) -> Bool
prop_fromBlocksDiagPointersNondecreasing x y =
  prop_pointersNondecreasing
  $ fromBlocksDiag [[Just x, Just y], [Nothing, Nothing]]

prop_fromBlocksDiagPointersLength
  :: Matrix Col (Complex Double) -> Matrix Col (Complex Double) -> Bool
prop_fromBlocksDiagPointersLength x y =
  prop_pointersLength
  $ fromBlocksDiag [[Just x, Just y], [Nothing, Nothing]]

prop_fromBlocksDiagValuesLength
  :: Matrix Col (Complex Double) -> Matrix Col (Complex Double) -> Bool
prop_fromBlocksDiagValuesLength x y =
  prop_valuesLength
  $ fromBlocksDiag [[Just x, Just y], [Nothing, Nothing]]

prop_fromBlocksDiagHermitian :: Matrix Col (Complex Double) -> Bool
prop_fromBlocksDiagHermitian a =
  hermitian $ fromBlocksDiag [[Nothing, Nothing], [Just a, Just $ reorient $ ctrans a]]

prop_withConstFromForeign
  :: (Eq a, Num a, Storable a, Unbox a) => Matrix Col a -> Bool
prop_withConstFromForeign mat =
  unsafePerformIO (withConstMatrix mat $ fromForeign True) == mat
