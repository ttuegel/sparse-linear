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
    [ testGroup "Multiplicative"
        [ QC.testProperty
            "a * 1 == a :: Matrix C Col Double"
            (prop_mul_ident_r :: Prop2 C Col Double)
        , QC.testProperty
            "a * 1 == a :: Matrix C Col (Complex Double)"
            (prop_mul_ident_r :: Prop2 C Col (Complex Double))
        , QC.testProperty
            "1 * a == a :: Matrix C Row Double"
            (prop_mul_ident_l :: Prop2 C Row Double)
        , QC.testProperty
            "1 * a == a :: Matrix C Row (Complex Double)"
            (prop_mul_ident_l :: Prop2 C Row (Complex Double))
        , QC.testProperty
            "(a * b) * c == a * (b * c) :: Matrix C ord Double"
            (prop_mul_assoc :: Prop3' Double)
        , QC.testProperty
            "(a * b) * c == a * (b * c) :: Matrix C ord (Complex Double)"
            (prop_mul_assoc :: Prop3' (Complex Double))
        , QC.testProperty
            ("transpose (a * b) == transpose a * transpose b"
                ++ " :: Matrix C Col Double")
            (prop_mul_trans :: Prop2 C Col Double)
        , QC.testProperty
            ("transpose (a * b) == transpose a * transpose b"
                ++ " :: Matrix C Col (Complex Double)")
            (prop_mul_trans :: Prop2 C Col (Complex Double))
        , QC.testProperty
            ("adjoint (a * b) == adjoint a * adjoint b"
                ++ " :: Matrix C Col (Complex Double)")
            (prop_mul_adj :: Prop2 C Col (Complex Double))
        ]
    , testGroup "LeftModule"
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

prop_mul_ident_r :: (Eq a, Num a, Show a, Unbox a) => Prop2 C Col a
prop_mul_ident_r (a, _) = a `mul` (ident $ view (dim . _2) a) === a

prop_mul_ident_l :: (Eq a, Num a, Show a, Unbox a) => Prop2 C Row a
prop_mul_ident_l (a, _) = (ident $ view (dim . _1) a) `mul` a === a

type Prop3' a = (Matrix C Col a, Matrix C Row a, Matrix C Row a) -> Property

prop_mul_assoc :: (MorallyEq (Matrix C Col a), Num a, Show a, Unbox a)
               => Prop3' a
prop_mul_assoc (matchDims3 -> (a, view transpose -> b, c)) = ab_c ~== a_bc
  where
    ab_c = (a `mul` view reorder b) `mul` c `asTypeOf` a
    a_bc = a `mul` (b `mul` c)

prop_mul_trans :: (MorallyEq (Matrix C Col a), Num a, Show a, Unbox a)
               => Prop2 C Col a
prop_mul_trans (matchDims2 -> (a, review transpose -> b)) =
    c ~== (a `mul` b)
  where
    c = view transpose (view transpose b `mul` review transpose a) `asTypeOf` a

prop_mul_adj :: Prop2 C Col (Complex Double)
prop_mul_adj (matchDims2 -> (a, review transpose -> b)) =
    c ~== (a `mul` b)
  where
    c = view adjoint (view adjoint b `mul` review adjoint a) `asTypeOf` a

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
