{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Lens
import Data.Complex
import Data.Monoid
import Data.Proxy.PolyKind
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as U
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck

import Numeric.LinearAlgebra.Sparse
import Numeric.LinearAlgebra.Sparse.QuickCheck ()

main :: IO ()
main = defaultMain $ testGroup "Data Invariants"
    [ testGroup "Matrix C Row Double"
        [ QC.testProperty "Arbitrary"
          (invariants :: Matrix C Row Double -> Property)
        , QC.testProperty "a <> a"
          (mappend_invariants :: Matrix C Row Double -> Property)
        , QC.testProperty "a + a"
          (add_invariants :: Matrix C Row Double -> Property)
        , QC.testProperty "a + a"
          (from_reorder_invariants :: Matrix C Row Double -> Property)
        ]
    , testGroup "Matrix C Col Double"
        [ QC.testProperty "Arbitrary"
          (invariants :: Matrix C Col Double -> Property)
        , QC.testProperty "a <> a"
          (mappend_invariants :: Matrix C Col Double -> Property)
        , QC.testProperty "a + a"
          (add_invariants :: Matrix C Col Double -> Property)
        , QC.testProperty "a * a"
          (mul_invariants :: Matrix C Col Double -> Property)
        , QC.testProperty "reorder a"
          (reorder_invariants :: Matrix C Col Double -> Property)
        ]
    , testGroup "Matrix C Row (Complex Double)"
        [ QC.testProperty "Arbitrary"
          (invariants :: Matrix C Row (Complex Double) -> Property)
        , QC.testProperty "a <> a"
          (mappend_invariants :: Matrix C Row (Complex Double) -> Property)
        , QC.testProperty "a + a"
          (add_invariants :: Matrix C Row (Complex Double) -> Property)
        , QC.testProperty "a + a"
          (from_reorder_invariants :: Matrix C Row (Complex Double) -> Property)
        ]
    , testGroup "Matrix C Col (Complex Double)"
        [ QC.testProperty "Arbitrary"
          (invariants :: Matrix C Col (Complex Double) -> Property)
        , QC.testProperty "a <> a"
          (mappend_invariants :: Matrix C Col (Complex Double) -> Property)
        , QC.testProperty "a + a"
          (add_invariants :: Matrix C Col (Complex Double) -> Property)
        , QC.testProperty "a * a"
          (mul_invariants :: Matrix C Col (Complex Double) -> Property)
        , QC.testProperty "reorder a"
          (reorder_invariants :: Matrix C Col (Complex Double) -> Property)
        ]
    ]

mappend_invariants :: (Format fmt, Orient or, Unbox a)
                   => Matrix fmt or a -> Property
mappend_invariants a = invariants $ mappend a a

add_invariants :: (Format fmt, Num a, Orient or, Show a, Unbox a)
               => Matrix fmt or a -> Property
add_invariants a = invariants $ add a a

mul_invariants :: (Num a, Show a, Unbox a) => Matrix C Col a -> Property
mul_invariants a =
    (invariants :: Unbox a => Matrix C Col a -> Property)
    $ mul a (a ^. from transpose)

reorder_invariants :: (Format fmt, Unbox a) => Matrix fmt Col a -> Property
reorder_invariants = invariants . view reorder

from_reorder_invariants :: (Format fmt, Unbox a) => Matrix fmt Row a -> Property
from_reorder_invariants = invariants . view (from reorder)

invariants :: (Format fmt, Orient or, Unbox a) => Matrix fmt or a -> Property
invariants = view $ formats (to invariantsU) (to invariantsC)

nondecreasingBy :: (Unbox a) => (a -> a -> Bool) -> a -> Vector a -> Bool
nondecreasingBy f x = snd . U.foldl' go (x, True)
  where
    go (acc, False) _ = (acc, False)
    go (acc, True) next = (next, f acc next)

invariantsU :: (Orient or, Unbox a) => Matrix U or a -> Property
invariantsU mat@(MatU ux) =
    let Ux nr nc _ = untag ux
    in conjoin
    [ counterexample "number of rows must be positive"
      $ property (nr > 0)
    , counterexample "number of columns must be positive"
      $ property (nc > 0)
    , counterexample "slices must all be non-decreasing"
      $ property $ all (nondecreasingBy (<=) 0 . fst . U.unzip)
      $ toListOf _slices mat
    ]

invariantsC :: (Orient or, Unbox a) => Matrix C or a -> Property
invariantsC mat@(MatC cx) =
    let Cx minor starts slices = untag cx
    in conjoin
    [ counterexample "minor dimension is negative" $ property (minor >= 0)
    , counterexample "starts must be non-decreasing"
      $ property $ nondecreasingBy (<=) 0 starts
    , counterexample "starts must be non-negative"
      $ property $ U.all (>= 0) starts
    , counterexample "starts must be in range"
      $ property $ U.all (<= U.length slices) starts
    , counterexample "minors must be in range"
      $ property $ U.all ((< minor) . fst) slices
    , counterexample "minors must be non-negative"
      $ property $ U.all ((>= 0) . fst) slices
    , counterexample "slices must all be non-decreasing"
      $ property $ all (nondecreasingBy (<=) 0 . fst . U.unzip)
      $ toListOf _slices mat
    ]
