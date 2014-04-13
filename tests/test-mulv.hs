{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Lens
import Data.Complex
import Data.MorallyEq
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck

import Numeric.LinearAlgebra.Sparse
import Numeric.LinearAlgebra.Sparse.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "Vector Properties"
    [ testGroup "Matrix C Row Double"
        [ QC.testProperty "(m1 + m2) .* v == (m1 .* v) + (m2 .* v)"
            (prop_mulv_distrib :: Prop_mulv_distrib Double)
        , QC.testProperty "(m1 * m2) .* v == m1 .* m2 .* v"
            (prop_mulv_assoc :: Prop_mulv_assoc Double)
        , QC.testProperty "1 * v == v"
            (prop_mulv_ident :: Prop_mulv_ident Double)
        , QC.testProperty "mulV m v == mulVM m v"
            (prop_mulv_mut :: Prop_mulv_mut Double)
        ]
    , testGroup "Matrix C Row (Complex Double)"
        [ QC.testProperty "(m1 + m2) .* v == (m1 .* v) + (m2 .* v)"
            (prop_mulv_distrib :: Prop_mulv_distrib (Complex Double))
        , QC.testProperty "(m1 * m2) .* v == m1 .* m2 .* v"
            (prop_mulv_assoc :: Prop_mulv_assoc (Complex Double))
        , QC.testProperty "1 * v == v"
            (prop_mulv_ident :: Prop_mulv_ident (Complex Double))
        , QC.testProperty "mulV m v == mulVM m v"
            (prop_mulv_mut :: Prop_mulv_mut (Complex Double))
        ]
    ]

type Constrain fmt or a =
    (Eq a, Format fmt, MorallyEq a, Num a, Orient or, Show a, Show (Matrix fmt or a), Unbox a)

type Prop_mulv_distrib a =
    (Matrix C Row a, Matrix C Row a, Vector a) -> Property

prop_mulv_distrib :: (Constrain C Col a, Constrain C Row a)
                  => Prop_mulv_distrib a
prop_mulv_distrib (matchDimsV2 -> (a, b, v)) =
    ((a `add` b) `mulV` v) ~== (U.zipWith (+) (a `mulV` v) (b `mulV` v))

type Prop_mulv_assoc a =
    (Matrix C Col a, Matrix C Row a, Vector a) -> Property

prop_mulv_assoc :: Constrain C Row a => Prop_mulv_assoc a
prop_mulv_assoc (matchDimsV2 -> (a, b, v)) =
    ((a `mul` b) `mulV` v) ~== (view reorder a `mulV` (b `mulV` v))

type Prop_mulv_ident a = Vector a -> Property

prop_mulv_ident :: Constrain C Row a => Prop_mulv_ident a
prop_mulv_ident v = v ~== (ident (U.length v) `mulV` v)

type Prop_mulv_mut a = (Matrix C Row a, Vector a) -> Property

prop_mulv_mut :: Constrain C Row a => Prop_mulv_mut a
prop_mulv_mut (matchDimsV -> (m, v)) = (m `mulV` v) ~== v'
  where
    v' = U.create $ do
        v_ <- U.thaw v
        v'_ <- MU.new $ view (dim . _1) m
        MU.set v'_ 0
        mulVM m v_ v'_
        return v'_
