{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PolyKinds #-}
module Main where

import Control.Lens
import qualified Data.AEq as AEq
import Data.Complex
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck

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
            (prop_aeq_trans :: Prop2Bool C Row Double)
        , QC.testProperty
            "a ~== a :: Matrix C Col Double"
            (prop_aeq_trans :: Prop2Bool C Col Double)
        , QC.testProperty
            "a ~== a :: Matrix U Row Double"
            (prop_aeq_trans :: Prop2Bool U Row Double)
        , QC.testProperty
            "a ~== a :: Matrix U Col Double"
            (prop_aeq_trans :: Prop2Bool U Col Double)
        , QC.testProperty
            "a ~== a :: Matrix C Row (Complex Double)"
            (prop_aeq_trans :: Prop2Bool C Row (Complex Double))
        , QC.testProperty
            "a ~== a :: Matrix C Col (Complex Double)"
            (prop_aeq_trans :: Prop2Bool C Col (Complex Double))
        , QC.testProperty
            "a ~== a :: Matrix U Row (Complex Double)"
            (prop_aeq_trans :: Prop2Bool U Row (Complex Double))
        , QC.testProperty
            "a ~== a :: Matrix U Col (Complex Double)"
            (prop_aeq_trans :: Prop2Bool U Col (Complex Double))

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
            (prop_add_assoc :: Prop3Bool C Row Double)
        , QC.testProperty
            "(a + b) + c == a + (b + c) :: Matrix C Row (Complex Double)"
            (prop_add_assoc :: Prop3Bool C Row (Complex Double))
        , QC.testProperty
            "(a + b) + c == a + (b + c) :: Matrix C Col Double"
            (prop_add_assoc :: Prop3Bool C Col Double)
        , QC.testProperty
            "(a + b) + c == a + (b + c) :: Matrix C Col (Complex Double)"
            (prop_add_assoc :: Prop3Bool C Col (Complex Double))

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
            (prop_add_linear :: Double -> Prop2Bool C Row Double)
        , QC.testProperty
            "c(a + b) == cb + ca (Scalar) :: Matrix C Row (Complex Double)"
            (prop_add_linear :: Complex Double -> Prop2Bool C Row (Complex Double))
        , QC.testProperty
            "c(a + b) == cb + ca (Scalar) :: Matrix C Col Double"
            (prop_add_linear :: Double -> Prop2Bool C Col Double)
        , QC.testProperty
            "c(a + b) == cb + ca (Scalar) :: Matrix C Col (Complex Double)"
            (prop_add_linear :: Complex Double -> Prop2Bool C Col (Complex Double))
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
            (prop_mul_assoc :: Prop3'Bool Double)
        , QC.testProperty
            "(a * b) * c == a * (b * c) :: Matrix C ord (Complex Double)"
            (prop_mul_assoc :: Prop3'Bool (Complex Double))
        , QC.testProperty
            "transpose (a * b) == transpose a * transpose b :: Matrix C Col Double"
            (prop_mul_trans :: Prop2Bool C Col Double)
        , QC.testProperty
            "transpose (a * b) == transpose a * transpose b :: Matrix C Col (Complex Double)"
            (prop_mul_trans :: Prop2Bool C Col (Complex Double))
        , QC.testProperty
            "adjoint (a * b) == adjoint a * adjoint b :: Matrix C Col (Complex Double)"
            (prop_mul_adj :: Prop2Bool C Col (Complex Double))
        ]
    , testGroup "LeftModule"
        [
        ]
    , testGroup "RightModule"
        [
        ]
    ]

type Prop2Bool fmt ord a = (Matrix fmt ord a, Matrix fmt ord a) -> Bool

prop_eq_trans :: (Eq a, FormatR fmt, Unbox a) => Prop2Bool fmt ord a
prop_eq_trans (a, _) = a == a

prop_aeq_trans :: (AEq.AEq a, FormatR fmt, Unbox a) => Prop2Bool fmt ord a
prop_aeq_trans (a, _) = a AEq.~== a

prop_eeq_trans :: (AEq.AEq a, FormatR fmt, Unbox a) => Prop2Bool fmt ord a
prop_eeq_trans (a, _) = a AEq.=== a

type Prop2 fmt ord a = (Matrix fmt ord a, Matrix fmt ord a) -> Property

prop_fmt_id_C :: (Eq a, OrderR ord, Show a, Unbox a) => Prop2 C ord a
prop_fmt_id_C (a, _) = a === (compress . decompress) a

prop_fmt_id_U :: (Eq a, OrderR ord, Show a, Unbox a) => Prop2 U ord a
prop_fmt_id_U (a, _) = a === (decompress . compress) a

type Prop3 fmt ord a = (Matrix fmt ord a, Matrix fmt ord a, Matrix fmt ord a) -> Property
type Prop3Bool fmt ord a = (Matrix fmt ord a, Matrix fmt ord a, Matrix fmt ord a) -> Bool

prop_add_assoc :: (AEq.AEq a, Num a, OrderR ord, Show a, Unbox a) => Prop3Bool C ord a
prop_add_assoc (a, b, c) = (a `add` b) `add` c AEq.~== a `add` (b `add` c)

prop_add_ident :: (Eq a, Num a, OrderR ord, Show a, Unbox a) => Prop2 C ord a
prop_add_ident (a, _) = (add a $ set dim (view dim a) empty) === a

prop_add_inv :: (Eq a, Num a, OrderR ord, Show a, Unbox a) => Prop2 C ord a
prop_add_inv (a, _) = add a (over each negate a) === (over each (const 0) a)

prop_add_commute :: (Eq a, Num a, OrderR ord, Show a, Unbox a) => Prop2 C ord a
prop_add_commute (a, b) = add a b === add b a

prop_add_linear :: (AEq.AEq a, Num a, OrderR ord, Show a, Unbox a) => a -> Prop2Bool C ord a
prop_add_linear factor (a, b) = scale (add a b) AEq.~== add (scale a) (scale b)
  where scale = over each (* factor)

prop_trans_trans  :: (FormatR fmt, OrderR ord, RealFloat a, Show (Matrix fmt ord a), Unbox a)
                  => Prop2 fmt ord a
prop_trans_trans (a, _) = transpose (transpose a) === a

prop_adj_adj  :: (FormatR fmt, OrderR ord, RealFloat a, Show (Matrix fmt ord (Complex a)), Unbox a)
              => Prop2 fmt ord (Complex a)
prop_adj_adj (a, _) = adjoint (adjoint a) === a

prop_mul_ident_r :: (Eq a, Num a, Show a, Unbox a) => Prop2 C Col a
prop_mul_ident_r (a, _) = a `mul` (ident $ view (dim . _2) a) === a

prop_mul_ident_l :: (Eq a, Num a, Show a, Unbox a) => Prop2 C Row a
prop_mul_ident_l (a, _) = (ident $ view (dim . _1) a) `mul` a === a

type Prop3' a = (Matrix C Col a, Matrix C Row a, Matrix C Row a) -> Property
type Prop3'Bool a = (Matrix C Col a, Matrix C Row a, Matrix C Row a) -> Bool

prop_mul_assoc :: (AEq.AEq a, Num a, Show a, Unbox a) => Prop3'Bool a
prop_mul_assoc (a, b, c) = ab a b c AEq.~== bc a b c
  where
    ab :: (Num a, Show a, Unbox a) => Matrix C Col a -> Matrix C Row a -> Matrix C Row a -> Matrix C Row a
    ab d e f = (d `mul` transpose e) `mul` f
    bc :: (Num a, Show a, Unbox a) => Matrix C Col a -> Matrix C Row a -> Matrix C Row a -> Matrix C Row a
    bc d e f = d `mul` (reorder (transpose e) `mul` f)

prop_mul_trans :: (AEq.AEq a, Num a, Unbox a) => Prop2Bool C Col a
prop_mul_trans (a, b) = c AEq.~== transpose b `mul` reorder a
  where
    c = transpose (transpose a `mul` reorder b)
    _ = c `add` a

prop_mul_adj :: (AEq.AEq (Complex a), Num a, RealFloat a, Unbox a) => Prop2Bool C Col (Complex a)
prop_mul_adj (a, b) = c AEq.~== adjoint b `mul` reorder a
  where
    c = adjoint (adjoint a `mul` reorder b)
    _ = c `add` a
