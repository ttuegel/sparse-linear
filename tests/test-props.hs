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
        [
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

prop_add_assoc :: (Eq a, Num a, OrderR ord, Show a, Unbox a) => Prop3 C ord a
prop_add_assoc (a, b, c) = (a `add` b) `add` c === a `add` (b `add` c)

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
