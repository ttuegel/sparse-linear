{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Lens
import qualified Data.AEq as AEq
import Data.Complex
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
    [ testGroup "Equality"
        [ QC.testProperty
            "a == a :: Matrix C Row Double"
            (prop_eq_trans :: Prop2Bool C Row Double)
        , QC.testProperty
            "a == a :: Matrix C Col Double"
            (prop_eq_trans :: Prop2Bool C Col Double)
        , QC.testProperty
            "a == a :: Matrix U Row Double"
            (prop_eq_trans :: Prop2Bool U Row Double)
        , QC.testProperty
            "a == a :: Matrix U Col Double"
            (prop_eq_trans :: Prop2Bool U Col Double)
        , QC.testProperty
            "a == a :: Matrix C Row (Complex Double)"
            (prop_eq_trans :: Prop2Bool C Row (Complex Double))
        , QC.testProperty
            "a == a :: Matrix C Col (Complex Double)"
            (prop_eq_trans :: Prop2Bool C Col (Complex Double))
        , QC.testProperty
            "a == a :: Matrix U Row (Complex Double)"
            (prop_eq_trans :: Prop2Bool U Row (Complex Double))
        , QC.testProperty
            "a == a :: Matrix U Col (Complex Double)"
            (prop_eq_trans :: Prop2Bool U Col (Complex Double))

        , QC.testProperty
            "a ~== a :: Matrix C Row Double"
            (prop_aeq_trans :: Prop2 C Row Double)
        , QC.testProperty
            "a ~== a :: Matrix C Col Double"
            (prop_aeq_trans :: Prop2 C Col Double)
        , QC.testProperty
            "a ~== a :: Matrix U Row Double"
            (prop_aeq_trans :: Prop2 U Row Double)
        , QC.testProperty
            "a ~== a :: Matrix U Col Double"
            (prop_aeq_trans :: Prop2 U Col Double)
        , QC.testProperty
            "a ~== a :: Matrix C Row (Complex Double)"
            (prop_aeq_trans :: Prop2 C Row (Complex Double))
        , QC.testProperty
            "a ~== a :: Matrix C Col (Complex Double)"
            (prop_aeq_trans :: Prop2 C Col (Complex Double))
        , QC.testProperty
            "a ~== a :: Matrix U Row (Complex Double)"
            (prop_aeq_trans :: Prop2 U Row (Complex Double))
        , QC.testProperty
            "a ~== a :: Matrix U Col (Complex Double)"
            (prop_aeq_trans :: Prop2 U Col (Complex Double))

        , QC.testProperty
            "a === a :: Matrix C Row Double"
            (prop_eeq_trans :: Prop2Bool C Row Double)
        , QC.testProperty
            "a === a :: Matrix C Col Double"
            (prop_eeq_trans :: Prop2Bool C Col Double)
        , QC.testProperty
            "a === a :: Matrix U Row Double"
            (prop_eeq_trans :: Prop2Bool U Row Double)
        , QC.testProperty
            "a === a :: Matrix U Col Double"
            (prop_eeq_trans :: Prop2Bool U Col Double)
        , QC.testProperty
            "a === a :: Matrix C Row (Complex Double)"
            (prop_eeq_trans :: Prop2Bool C Row (Complex Double))
        , QC.testProperty
            "a === a :: Matrix C Col (Complex Double)"
            (prop_eeq_trans :: Prop2Bool C Col (Complex Double))
        , QC.testProperty
            "a === a :: Matrix U Row (Complex Double)"
            (prop_eeq_trans :: Prop2Bool U Row (Complex Double))
        , QC.testProperty
            "a === a :: Matrix U Col (Complex Double)"
            (prop_eeq_trans :: Prop2Bool U Col (Complex Double))
        ]
    , testGroup "Format"
        [ QC.testProperty
            "compress . decompress == id :: Matrix C Row Double)"
            (prop_fmt_id_C :: Prop2 C Row Double)
        , QC.testProperty
            "compress . decompress == id :: Matrix C Row (Complex Double))"
            (prop_fmt_id_C :: Prop2 C Row (Complex Double))
        , QC.testProperty
            "compress . decompress == id :: Matrix C Col Double)"
            (prop_fmt_id_C :: Prop2 C Col Double)
        , QC.testProperty
            "compress . decompress == id :: Matrix C Col (Complex Double))"
            (prop_fmt_id_C :: Prop2 C Col (Complex Double))

        , QC.testProperty
            "decompress . compress == id :: Matrix U Row Double)"
            (prop_fmt_id_U :: Prop2 U Row Double)
        , QC.testProperty
            "decompress . compress == id :: Matrix U Row (Complex Double))"
            (prop_fmt_id_U :: Prop2 U Row (Complex Double))
        , QC.testProperty
            "decompress . compress == id :: Matrix U Col Double)"
            (prop_fmt_id_U :: Prop2 U Col Double)
        , QC.testProperty
            "decompress . compress == id :: Matrix U Col (Complex Double))"
            (prop_fmt_id_U :: Prop2 U Col (Complex Double))
        ]
    , testGroup "Additive"
        [ QC.testProperty
            "(a + b) + c == a + (b + c) :: Matrix C Row Double"
            (prop_add_assoc :: Prop3 C Row Double)
        , QC.testProperty
            "(a + b) + c == a + (b + c) :: Matrix C Row (Complex Double)"
            (prop_add_assoc :: Prop3 C Row (Complex Double))
        , QC.testProperty
            "(a + b) + c == a + (b + c) :: Matrix C Col Double"
            (prop_add_assoc :: Prop3 C Col Double)
        , QC.testProperty
            "(a + b) + c == a + (b + c) :: Matrix C Col (Complex Double)"
            (prop_add_assoc :: Prop3 C Col (Complex Double))

        , QC.testProperty
            "a + 0 == a :: Matrix C Row Double"
            (prop_add_ident :: Prop2 C Row Double)
        , QC.testProperty
            "a + 0 == a :: Matrix C Row (Complex Double)"
            (prop_add_ident :: Prop2 C Row (Complex Double))
        , QC.testProperty
            "a + 0 == a :: Matrix C Col Double"
            (prop_add_ident :: Prop2 C Col Double)
        , QC.testProperty
            "a + 0 == a :: Matrix C Col (Complex Double)"
            (prop_add_ident :: Prop2 C Col (Complex Double))

        , QC.testProperty
            "a - a == 0 :: Matrix C Row Double"
            (prop_add_inv :: Prop2 C Row Double)
        , QC.testProperty
            "a - a == 0 :: Matrix C Row (Complex Double)"
            (prop_add_inv :: Prop2 C Row (Complex Double))
        , QC.testProperty
            "a - a == 0 :: Matrix C Col Double"
            (prop_add_inv :: Prop2 C Col Double)
        , QC.testProperty
            "a - a == 0 :: Matrix C Col (Complex Double)"
            (prop_add_inv :: Prop2 C Col (Complex Double))

        , QC.testProperty
            "a + b == b + a :: Matrix C Row Double"
            (prop_add_commute :: Prop2 C Row Double)
        , QC.testProperty
            "a + b == b + a :: Matrix C Row (Complex Double)"
            (prop_add_commute :: Prop2 C Row (Complex Double))
        , QC.testProperty
            "a + b == b + a :: Matrix C Col Double"
            (prop_add_commute :: Prop2 C Col Double)
        , QC.testProperty
            "a + b == b + a :: Matrix C Col (Complex Double)"
            (prop_add_commute :: Prop2 C Col (Complex Double))

        , QC.testProperty
            "c(a + b) == cb + ca (Scalar) :: Matrix C Row Double"
            (prop_add_linear :: Double -> Prop2 C Row Double)
        , QC.testProperty
            "c(a + b) == cb + ca (Scalar) :: Matrix C Row (Complex Double)"
            (prop_add_linear :: Complex Double -> Prop2 C Row (Complex Double))
        , QC.testProperty
            "c(a + b) == cb + ca (Scalar) :: Matrix C Col Double"
            (prop_add_linear :: Double -> Prop2 C Col Double)
        , QC.testProperty
            "c(a + b) == cb + ca (Scalar) :: Matrix C Col (Complex Double)"
            (prop_add_linear :: Complex Double -> Prop2 C Col (Complex Double))
        ]
    , testGroup "Involutive"
        [ QC.testProperty
            "transpose (transpose a) == a :: Matrix C Row Double"
            (prop_trans_trans :: Prop2 C Row Double)
        , QC.testProperty
            "transpose (transpose a) == a :: Matrix C Col Double"
            (prop_trans_trans :: Prop2 C Col Double)
        , QC.testProperty
            "transpose (transpose a) == a :: Matrix U Row Double"
            (prop_trans_trans :: Prop2 U Row Double)
        , QC.testProperty
            "transpose (transpose a) == a :: Matrix U Col Double"
            (prop_trans_trans :: Prop2 U Col Double)
        , QC.testProperty
            "transpose (transpose a) == a :: Matrix C Row (Complex Double)"
            (prop_adj_adj :: Prop2 C Row (Complex Double))
        , QC.testProperty
            "transpose (transpose a) == a :: Matrix C Col (Complex Double)"
            (prop_adj_adj :: Prop2 C Col (Complex Double))
        , QC.testProperty
            "transpose (transpose a) == a :: Matrix U Row (Complex Double)"
            (prop_adj_adj :: Prop2 U Row (Complex Double))
        , QC.testProperty
            "transpose (transpose a) == a :: Matrix U Col (Complex Double)"
            (prop_adj_adj :: Prop2 U Col (Complex Double))
        ]
    , testGroup "Multiplicative"
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
            "transpose (a * b) == transpose a * transpose b :: Matrix C Col Double"
            (prop_mul_trans :: Prop2 C Col Double)
        , QC.testProperty
            "transpose (a * b) == transpose a * transpose b :: Matrix C Col (Complex Double)"
            (prop_mul_trans :: Prop2 C Col (Complex Double))
        , QC.testProperty
            "adjoint (a * b) == adjoint a * adjoint b :: Matrix C Col (Complex Double)"
            (prop_mul_adj :: Prop2 C Col (Complex Double))
        ]
    , testGroup "LeftModule"
        [ QC.testProperty
            "(m1 + m2) .* v == (m1 .* v) + (m2 .* v) :: Matrix C Row Double"
            (prop_mod_distrib :: Prop3V Row Row Double)
        , QC.testProperty
            "(m1 + m2) .* v == (m1 .* v) + (m2 .* v) :: Matrix C Row (Complex Double)"
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

type Prop2Bool fmt ord a = (Matrix fmt ord a, Matrix fmt ord a) -> Bool

prop_eq_trans :: (Eq a, FormatR fmt, Unbox a) => Prop2Bool fmt ord a
prop_eq_trans (a, _) = a == a

prop_aeq_trans :: (AEq.AEq a, FormatR fmt, Show (Matrix fmt ord a), Unbox a) => Prop2 fmt ord a
prop_aeq_trans (a, _) = a ~== a

prop_eeq_trans :: (AEq.AEq a, FormatR fmt, Unbox a) => Prop2Bool fmt ord a
prop_eeq_trans (a, _) = a AEq.=== a

type Prop2 fmt ord a = (Matrix fmt ord a, Matrix fmt ord a) -> Property

prop_fmt_id_C :: (Eq a, Orient ord, Show a, Unbox a) => Prop2 C ord a
prop_fmt_id_C (a, _) = a === (compress . decompress) a

prop_fmt_id_U :: (Eq a, Orient ord, Show a, Unbox a) => Prop2 U ord a
prop_fmt_id_U (a, _) = a === (decompress . compress) a

type Prop3 fmt ord a = (Matrix fmt ord a, Matrix fmt ord a, Matrix fmt ord a) -> Property
type Prop3Bool fmt ord a = (Matrix fmt ord a, Matrix fmt ord a, Matrix fmt ord a) -> Bool

prop_add_assoc :: (AEq.AEq a, Num a, Orient ord, Show a, Unbox a) => Prop3 C ord a
prop_add_assoc (matchDims3 -> (a, b, c)) =
    ((a `add` b) `add` c) ~== (a `add` (b `add` c))

prop_add_ident :: (Eq a, Num a, Orient ord, Show a, Unbox a) => Prop2 C ord a
prop_add_ident (a, _) = (add a $ set dim (view dim a) empty) === a

prop_add_inv :: (Eq a, Num a, Orient ord, Show a, Unbox a) => Prop2 C ord a
prop_add_inv (a, _) = add a (over each negate a) === (over each (const 0) a)

prop_add_commute :: (AEq.AEq a, Num a, Orient ord, Show a, Unbox a) => Prop2 C ord a
prop_add_commute (matchDims2 -> (a, b)) = add a b ~== add b a

prop_add_linear :: (MorallyEq (Matrix C ord a), Num a, Orient ord, Show a, Unbox a) => a -> Prop2 C ord a
prop_add_linear factor (matchDims2 -> (a, b)) =
    morallyEq (scale (add a b)) (add (scale a) (scale b))
  where scale = over each (* factor)

prop_trans_trans  :: (FormatR fmt, Orient ord, RealFloat a, Show (Matrix fmt ord a), Unbox a)
                  => Prop2 fmt ord a
prop_trans_trans (a, _) = transpose (transpose a) === a

prop_adj_adj  :: (FormatR fmt, Orient ord, RealFloat a, Show (Matrix fmt ord (Complex a)), Unbox a)
              => Prop2 fmt ord (Complex a)
prop_adj_adj (a, _) = adjoint (adjoint a) === a

prop_mul_ident_r :: (Eq a, Num a, Show a, Unbox a) => Prop2 C Col a
prop_mul_ident_r (a, _) = a `mul` (ident $ view (dim . _2) a) === a

prop_mul_ident_l :: (Eq a, Num a, Show a, Unbox a) => Prop2 C Row a
prop_mul_ident_l (a, _) = (ident $ view (dim . _1) a) `mul` a === a

type Prop3' a = (Matrix C Col a, Matrix C Row a, Matrix C Row a) -> Property

prop_mul_assoc :: (MorallyEq (Matrix C Col a), Num a, Show a, Unbox a) => Prop3' a
prop_mul_assoc (matchDims3 -> (a, transpose -> b, c)) =
    counterexample (show ab_c ++ " /= " ++ show a_bc)
    $ morallyEq ab_c a_bc
  where
    ab_c = (a `mul` b) `mul` c `asTypeOf` a
    a_bc = a `mul` (reorder b `mul` c)

prop_mul_trans :: (AEq.AEq a, Num a, Show a, Unbox a) => Prop2 C Col a
prop_mul_trans (matchDims2 -> (a, b)) = c ~== (transpose b `mul` reorder a)
  where
    c = transpose (transpose a `mul` reorder b) `asTypeOf` a

prop_mul_adj :: (AEq.AEq (Complex a), Num a, RealFloat a, Show a, Unbox a) => Prop2 C Col (Complex a)
prop_mul_adj (matchDims2 -> (a, b)) = c ~== (adjoint b `mul` reorder a)
  where
    c = adjoint (adjoint a `mul` reorder b) `asTypeOf` a

type Prop3V ord ord' a = (Matrix C ord a, Matrix C ord' a, Vector a) -> Property
type Prop2V ord a = (Matrix C ord a, Vector a) -> Property

prop_mod_distrib :: (AEq.AEq a, Num a, Show a, Unbox a) => Prop3V Row Row a
prop_mod_distrib (matchDimsV2 -> (m1, m2, v)) = U.toList ((m1 `add` m2) `mulV` v) ~== U.toList (U.zipWith (+) (m1 `mulV` v) (m2 `mulV` v))

prop_mod_assoc :: (AEq.AEq a, Num a, Show a, Unbox a) => Prop3V Col Row a
prop_mod_assoc (matchDimsV2 -> (transpose -> a, b, v)) =
    ((a `mul` b) `mulV` v) ~== (reorder a `mulV` (b `mulV` v))

prop_mod_ident :: (AEq.AEq a, Num a, Show a, Unbox a) => Prop2V Row a
prop_mod_ident (_, v) = v ~== (ident (U.length v) `mulV` v)

prop_mod_mut :: (AEq.AEq a, Num a, Show a, Unbox a) => Prop2V Row a
prop_mod_mut (matchDimsV -> (m, v)) = U.toList (m `mulV` v) ~== U.toList v'
  where
    v' = U.create $ do
        v_ <- U.thaw v
        v'_ <- MU.new $ view (dim . _1) m
        MU.set v'_ 0
        mulVM m v_ v'_
        return v'_

(~==) :: (AEq.AEq a, Show a) => a -> a -> Property
(~==) a b = counterexample (show a ++ " /= " ++ show b) (property $ a AEq.~== b)

class MorallyEq a where
    morallyEq :: Show a => a -> a -> Property

instance (FormatR fmt, Orient or) => MorallyEq (Matrix fmt or Double) where
    morallyEq (decompress -> a) (decompress -> b) =
        counterexample (show a ++ " /= " ++ show b)
        $ U.and $ U.zipWith ok (unpack a) (unpack b)
      where
        relativeError x y = 2 * abs (x - y) / (abs x + abs y)
        ok (i, j, x) (m, n, y) =
            i == m && j == n && (x == y || relativeError x y < 1.0E-10)

instance (FormatR fmt, Orient or) => MorallyEq (Matrix fmt or (Complex Double)) where
    morallyEq (decompress -> a) (decompress -> b) =
        counterexample (show a ++ " /= " ++ show b)
        $ U.and $ U.zipWith ok (unpack a) (unpack b)
      where
        relativeError x y = 2 * magnitude (x - y) / (magnitude x + magnitude y)
        ok (i, j, x) (m, n, y) =
            i == m && j == n && (x == y || relativeError x y < 1.0E-10)
