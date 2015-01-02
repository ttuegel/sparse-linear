module Main where

import Data.MonoTraversable
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Test.Hspec
import Test.QuickCheck

import Numeric.LinearAlgebra.Sparse
import Test.LinearAlgebra

main :: IO ()
main = hspec $ do
  describe "Numeric.LinearAlgebra.Sparse" $ do
    describe "fromTriples" $ do
      it "row indices non-negative" $ property (prop_rowIndicesNonNegative :: Matrix (Complex Double) -> Bool)
      it "row indices in range" $ property (prop_rowIndicesInRange :: Matrix (Complex Double) -> Bool)
      it "row indices increasing" $ property (prop_rowIndicesIncreasing :: Matrix (Complex Double) -> Bool)
      it "values length" $ property (prop_valuesLength :: Matrix (Complex Double) -> Bool)
      it "column pointers length" $ property (prop_columnPointersLength :: Matrix (Complex Double) -> Bool)
      it "column pointers nondecreasing" $ property (prop_columnPointersNondecreasing :: Matrix (Complex Double) -> Bool)

    describe "kronecker" $ do
      it "assembles identity matrices" $ property prop_kroneckerIdent
      it "row indices increasing" $ property prop_kroneckerRowIndicesIncreasing
      it "column pointers nondecreasing"
        $ property prop_kroneckerColumnPointersNondecreasing
      it "column pointers length" $ property prop_kroneckerColumnPointersLength
      it "values length" $ property prop_kroneckerValuesLength

    describe "takeDiag" $ do
      it "returns what diag is given" $ property prop_takeDiag

    describe "mulV" $ do
      it "identity" $ property prop_identMulV

    describe "lin" $ do
      it "additive inverse" $ property prop_addInv
      it "additive identity" $ property prop_addId
      it "row indices increasing" $ property prop_linRowIndicesIncreasing
      it "column pointers nondecreasing"
        $ property prop_linColumnPointersNondecreasing
      it "column pointers length" $ property prop_linColumnPointersLength
      it "values length" $ property prop_linValuesLength

    describe "transpose" $ do
      it "self-inverse" $ property prop_transposeId
      it "preserves diagonal" $ property prop_transposeDiag

    describe "ctrans" $ do
      it "self-inverse" $ property prop_ctransId
      it "preserves diagonal of real matrices" $ property prop_ctransDiag
      it "preserves hermitian matrices" $ do
          let m :: Matrix (Complex Double)
              m = fromTriples 2 2 [(0, 0, 2), (0, 1, -1), (1, 0, -1), (1, 1, 2)]
          m `shouldBe` ctrans m

    describe "mul" $ do
      it "identity on matrices" $ property prop_mulId

    describe "fromBlocks" $ do
      it "assembles identity matrices" $ property prop_fromBlocksId
      it "row indices increasing" $ property prop_fromBlocksRowIndicesIncreasing
      it "column pointers nondecreasing"
        $ property prop_fromBlocksColumnPointersNondecreasing
      it "column pointers length" $ property prop_fromBlocksColumnPointersLength
      it "values length" $ property prop_fromBlocksValuesLength

    describe "fromBlocksDiag" $ do
      it "assembles identity matrices" $ property prop_fromBlocksDiagId
      it "row indices increasing"
        $ property prop_fromBlocksDiagRowIndicesIncreasing
      it "column pointers nondecreasing"
        $ property prop_fromBlocksDiagColumnPointersNondecreasing
      it "column pointers length"
        $ property prop_fromBlocksDiagColumnPointersLength
      it "values length" $ property prop_fromBlocksDiagValuesLength

prop_kroneckerIdent :: Int -> Int -> Property
prop_kroneckerIdent x y = (x > 0 && y > 0) ==> lhs == rhs
  where
    lhs :: Matrix (Complex Double)
    lhs = kronecker (ident x) (ident y)
    rhs = ident (x * y)

prop_kroneckerRowIndicesIncreasing
  :: Matrix (Complex Double) -> Matrix (Complex Double) -> Bool
prop_kroneckerRowIndicesIncreasing a b =
  prop_rowIndicesIncreasing $ kronecker a b

prop_kroneckerColumnPointersNondecreasing
  :: Matrix (Complex Double) -> Matrix (Complex Double) -> Bool
prop_kroneckerColumnPointersNondecreasing a b =
  prop_columnPointersNondecreasing $ kronecker a b

prop_kroneckerColumnPointersLength
  :: Matrix (Complex Double) -> Matrix (Complex Double) -> Bool
prop_kroneckerColumnPointersLength a b =
  prop_columnPointersLength $ kronecker a b

prop_kroneckerValuesLength
  :: Matrix (Complex Double) -> Matrix (Complex Double) -> Property
prop_kroneckerValuesLength a b =
  let k = kronecker a b
  in counterexample (show k) $ prop_valuesLength k

prop_takeDiag :: Vector (Complex Double) -> Bool
prop_takeDiag v = takeDiag (diag v) == v

prop_identMulV :: Vector (Complex Double) -> Bool
prop_identMulV v = mulV identM v == v
  where
    identM = ident (V.length v)

prop_addInv :: Matrix (Complex Double) -> Property
prop_addInv m = counterexample (show subtracted) (subtracted == m0)
  where
    subtracted = lin 1 m (-1) m
    m0 = omap (const 0) m

prop_addId :: Matrix (Complex Double) -> Bool
prop_addId m = add m (zeros (nRows m) (nColumns m)) == m

prop_linRowIndicesIncreasing
  :: Complex Double -> Matrix (Complex Double) -> Bool
prop_linRowIndicesIncreasing c mat =
  prop_rowIndicesIncreasing
  $ lin c mat (0.5683358478038576 :+ 1.9175490512894502) mat

prop_linColumnPointersNondecreasing
  :: Complex Double -> Matrix (Complex Double) -> Bool
prop_linColumnPointersNondecreasing c mat =
  prop_columnPointersNondecreasing $ lin c mat (-1) mat

prop_linColumnPointersLength
  :: Complex Double -> Matrix (Complex Double) -> Bool
prop_linColumnPointersLength c mat =
  prop_columnPointersLength $ lin c mat (-1) mat

prop_linValuesLength
  :: Complex Double -> Matrix (Complex Double) -> Bool
prop_linValuesLength c mat =
  prop_valuesLength $ lin c mat (-1) mat

prop_transposeId :: Matrix (Complex Double) -> Bool
prop_transposeId m = transpose (transpose m) == m

prop_transposeDiag :: Vector Double -> Bool
prop_transposeDiag v = m == transpose m
  where
    m = diag v

prop_ctransId :: Matrix (Complex Double) -> Bool
prop_ctransId m = ctrans (ctrans m) == m

prop_ctransDiag :: Vector Double -> Bool
prop_ctransDiag v = m == ctrans m
  where
    m = diag v

prop_mulId :: Matrix (Complex Double) -> Bool
prop_mulId m = m `mul` (ident $ nColumns m) == m

prop_fromBlocksId :: Int -> Int -> Property
prop_fromBlocksId x y = (x > 0 && y > 0) ==> lhs === ident (x + y)
  where
    lhs :: Matrix (Complex Double)
    lhs = fromBlocks [[Just (ident x), Nothing], [Nothing, Just (ident y)]]

prop_fromBlocksRowIndicesIncreasing
  :: Matrix (Complex Double) -> Matrix (Complex Double) -> Bool
prop_fromBlocksRowIndicesIncreasing x y =
  prop_rowIndicesIncreasing $ fromBlocks [[Just x, Nothing], [Nothing, Just y]]

prop_fromBlocksColumnPointersNondecreasing
  :: Matrix (Complex Double) -> Matrix (Complex Double) -> Bool
prop_fromBlocksColumnPointersNondecreasing x y =
  prop_columnPointersNondecreasing
  $ fromBlocks [[Just x, Nothing], [Nothing, Just y]]

prop_fromBlocksColumnPointersLength
  :: Matrix (Complex Double) -> Matrix (Complex Double) -> Bool
prop_fromBlocksColumnPointersLength x y =
  prop_columnPointersLength
  $ fromBlocks [[Just x, Nothing], [Nothing, Just y]]

prop_fromBlocksValuesLength
  :: Matrix (Complex Double) -> Matrix (Complex Double) -> Bool
prop_fromBlocksValuesLength x y =
  prop_valuesLength
  $ fromBlocks [[Just x, Nothing], [Nothing, Just y]]

prop_fromBlocksDiagId :: Int -> Int -> Property
prop_fromBlocksDiagId x y = (x > 0 && y > 0) ==> lhs === ident (x + y)
  where
    lhs :: Matrix (Complex Double)
    lhs = fromBlocksDiag [[Just (ident x), Just (ident y)], [Nothing, Nothing]]

prop_fromBlocksDiagRowIndicesIncreasing
  :: Matrix (Complex Double) -> Matrix (Complex Double) -> Bool
prop_fromBlocksDiagRowIndicesIncreasing x y =
  prop_rowIndicesIncreasing
  $ fromBlocksDiag [[Just x, Just y], [Nothing, Nothing]]

prop_fromBlocksDiagColumnPointersNondecreasing
  :: Matrix (Complex Double) -> Matrix (Complex Double) -> Bool
prop_fromBlocksDiagColumnPointersNondecreasing x y =
  prop_columnPointersNondecreasing
  $ fromBlocksDiag [[Just x, Just y], [Nothing, Nothing]]

prop_fromBlocksDiagColumnPointersLength
  :: Matrix (Complex Double) -> Matrix (Complex Double) -> Bool
prop_fromBlocksDiagColumnPointersLength x y =
  prop_columnPointersLength
  $ fromBlocksDiag [[Just x, Just y], [Nothing, Nothing]]

prop_fromBlocksDiagValuesLength
  :: Matrix (Complex Double) -> Matrix (Complex Double) -> Bool
prop_fromBlocksDiagValuesLength x y =
  prop_valuesLength
  $ fromBlocksDiag [[Just x, Just y], [Nothing, Nothing]]
