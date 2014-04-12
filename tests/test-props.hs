{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Lens
import Data.Complex
import Data.MorallyEq
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck

import Debug.Trace

import Numeric.LinearAlgebra.Sparse
import Numeric.LinearAlgebra.Sparse.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "Properties"
    [ testGroup "LeftModule"
        [ QC.testProperty
            "(m1 + m2) .* v == (m1 .* v) + (m2 .* v) :: Matrix C Row Double"
            (prop_mod_distrib :: Prop3V Row Row Double)
        , QC.testProperty
            ("(m1 + m2) .* v == (m1 .* v) + (m2 .* v)"
                ++ ":: Matrix C Row (Complex Double)")
            (prop_mod_distrib :: Prop3V Row Row (Complex Double))
        , QC.testProperty
            "(m1 * m2) .* v == m1 .* m2 .* v :: Matrix C Row Double"
            (prop_mod_assoc :: Prop3V Col Row Double)
        , QC.testProperty
            "(m1 * m2) .* v == m1 .* m2 .* v :: Matrix C Row (Complex Double)"
            (prop_mod_assoc :: Prop3V Col Row (Complex Double))
        , QC.testProperty
            "1 * v == v :: Matrix C Row Double"
            (prop_mod_ident :: Prop2V Row Double)
        , QC.testProperty
            "1 * v == v :: Matrix C Row (Complex Double)"
            (prop_mod_ident :: Prop2V Row (Complex Double))
        ]
    ]

type Prop3 fmt ord a =
    (Matrix fmt ord a, Matrix fmt ord a, Matrix fmt ord a) -> Property

type Prop2 fmt ord a = (Matrix fmt ord a, Matrix fmt ord a) -> Property

type Prop3' a = (Matrix C Col a, Matrix C Row a, Matrix C Row a) -> Property

type Prop3V ord ord' a = (Matrix C ord a, Matrix C ord' a, Vector a) -> Property
type Prop2V ord a = (Matrix C ord a, Vector a) -> Property

prop_mod_distrib :: (Fractional a, MorallyEq a, Num a, Show a, Unbox a)
                 => Prop3V Row Row a
prop_mod_distrib (matchDimsV2 -> (m1, m2, v)) =
    ((m1 `add` m2) `mulV` v) ~== (U.zipWith (+) (m1 `mulV` v) (m2 `mulV` v))

prop_mod_assoc :: (Fractional a, MorallyEq a, Num a, Show a, Unbox a)
               => Prop3V Col Row a
prop_mod_assoc (matchDimsV2 -> (review transpose -> a, b, v)) =
    ((review reorder a `mul` b) `mulV` v) ~== (a `mulV` (b `mulV` v))

prop_mod_ident :: (Fractional a, MorallyEq a, Num a, Show a, Unbox a)
               => Prop2V Row a
prop_mod_ident (_, v) = v ~== (ident (U.length v) `mulV` v)

prop_mod_mut :: (Num a, Fractional a, MorallyEq a, Ord a, Show a, Unbox a)
             => Prop2V Row a
prop_mod_mut (matchDimsV -> (m, v)) = (m `mulV` v) ~== v'
  where
    v' = U.create $ do
        v_ <- U.thaw v
        v'_ <- MU.new $ view (dim . _1) m
        MU.set v'_ 0
        mulVM m v_ v'_
        return v'_
