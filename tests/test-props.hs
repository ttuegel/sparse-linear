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
            (prop_eq_trans :: Prop_Eq_Trans C Row Double)
        , QC.testProperty
            "a == a :: Matrix C Col Double"
            (prop_eq_trans :: Prop_Eq_Trans C Col Double)
        , QC.testProperty
            "a == a :: Matrix U Row Double"
            (prop_eq_trans :: Prop_Eq_Trans U Row Double)
        , QC.testProperty
            "a == a :: Matrix U Col Double"
            (prop_eq_trans :: Prop_Eq_Trans U Col Double)
        , QC.testProperty
            "a == a :: Matrix C Row (Complex Double)"
            (prop_eq_trans :: Prop_Eq_Trans C Row (Complex Double))
        , QC.testProperty
            "a == a :: Matrix C Col (Complex Double)"
            (prop_eq_trans :: Prop_Eq_Trans C Col (Complex Double))
        , QC.testProperty
            "a == a :: Matrix U Row (Complex Double)"
            (prop_eq_trans :: Prop_Eq_Trans U Row (Complex Double))
        , QC.testProperty
            "a == a :: Matrix U Col (Complex Double)"
            (prop_eq_trans :: Prop_Eq_Trans U Col (Complex Double))

        , QC.testProperty
            "a ~== a :: Matrix C Row Double"
            (prop_aeq_trans :: Prop_Eq_Trans C Row Double)
        , QC.testProperty
            "a ~== a :: Matrix C Col Double"
            (prop_aeq_trans :: Prop_Eq_Trans C Col Double)
        , QC.testProperty
            "a ~== a :: Matrix U Row Double"
            (prop_aeq_trans :: Prop_Eq_Trans U Row Double)
        , QC.testProperty
            "a ~== a :: Matrix U Col Double"
            (prop_aeq_trans :: Prop_Eq_Trans U Col Double)
        , QC.testProperty
            "a ~== a :: Matrix C Row (Complex Double)"
            (prop_aeq_trans :: Prop_Eq_Trans C Row (Complex Double))
        , QC.testProperty
            "a ~== a :: Matrix C Col (Complex Double)"
            (prop_aeq_trans :: Prop_Eq_Trans C Col (Complex Double))
        , QC.testProperty
            "a ~== a :: Matrix U Row (Complex Double)"
            (prop_aeq_trans :: Prop_Eq_Trans U Row (Complex Double))
        , QC.testProperty
            "a ~== a :: Matrix U Col (Complex Double)"
            (prop_aeq_trans :: Prop_Eq_Trans U Col (Complex Double))

        , QC.testProperty
            "a === a :: Matrix C Row Double"
            (prop_eeq_trans :: Prop_Eq_Trans C Row Double)
        , QC.testProperty
            "a === a :: Matrix C Col Double"
            (prop_eeq_trans :: Prop_Eq_Trans C Col Double)
        , QC.testProperty
            "a === a :: Matrix U Row Double"
            (prop_eeq_trans :: Prop_Eq_Trans U Row Double)
        , QC.testProperty
            "a === a :: Matrix U Col Double"
            (prop_eeq_trans :: Prop_Eq_Trans U Col Double)
        , QC.testProperty
            "a === a :: Matrix C Row (Complex Double)"
            (prop_eeq_trans :: Prop_Eq_Trans C Row (Complex Double))
        , QC.testProperty
            "a === a :: Matrix C Col (Complex Double)"
            (prop_eeq_trans :: Prop_Eq_Trans C Col (Complex Double))
        , QC.testProperty
            "a === a :: Matrix U Row (Complex Double)"
            (prop_eeq_trans :: Prop_Eq_Trans U Row (Complex Double))
        , QC.testProperty
            "a === a :: Matrix U Col (Complex Double)"
            (prop_eeq_trans :: Prop_Eq_Trans U Col (Complex Double))
        ]
    , testGroup "Format"
        [ QC.testProperty
            "compress . decompress == id :: Matrix C Row Double)"
            (prop_fmt_id_C :: Prop_Fmt_Id C Row Double)
        , QC.testProperty
            "compress . decompress == id :: Matrix C Row (Complex Double))"
            (prop_fmt_id_C :: Prop_Fmt_Id C Row (Complex Double))
        , QC.testProperty
            "compress . decompress == id :: Matrix C Col Double)"
            (prop_fmt_id_C :: Prop_Fmt_Id C Col Double)
        , QC.testProperty
            "compress . decompress == id :: Matrix C Col (Complex Double))"
            (prop_fmt_id_C :: Prop_Fmt_Id C Col (Complex Double))

        , QC.testProperty
            "decompress . compress == id :: Matrix U Row Double)"
            (prop_fmt_id_U :: Prop_Fmt_Id U Row Double)
        , QC.testProperty
            "decompress . compress == id :: Matrix U Row (Complex Double))"
            (prop_fmt_id_U :: Prop_Fmt_Id U Row (Complex Double))
        , QC.testProperty
            "decompress . compress == id :: Matrix U Col Double)"
            (prop_fmt_id_U :: Prop_Fmt_Id U Col Double)
        , QC.testProperty
            "decompress . compress == id :: Matrix U Col (Complex Double))"
            (prop_fmt_id_U :: Prop_Fmt_Id U Col (Complex Double))
        ]
    , testGroup "Additive"
        [ QC.testProperty
            "(a + b) + c == a + (b + c) :: Matrix C Row Double"
            (prop_add_assoc :: Prop_Add_Assoc Row Double)
        , QC.testProperty
            "(a + b) + c == a + (b + c) :: Matrix C Row (Complex Double)"
            (prop_add_assoc :: Prop_Add_Assoc Row (Complex Double))
        , QC.testProperty
            "(a + b) + c == a + (b + c) :: Matrix C Col Double"
            (prop_add_assoc :: Prop_Add_Assoc Col Double)
        , QC.testProperty
            "(a + b) + c == a + (b + c) :: Matrix C Col (Complex Double)"
            (prop_add_assoc :: Prop_Add_Assoc Col (Complex Double))

        , QC.testProperty
            "a + 0 == a :: Matrix C Row Double"
            (prop_add_ident :: Prop_Add Row Double)
        , QC.testProperty
            "a + 0 == a :: Matrix C Row (Complex Double)"
            (prop_add_ident :: Prop_Add Row (Complex Double))
        , QC.testProperty
            "a + 0 == a :: Matrix C Col Double"
            (prop_add_ident :: Prop_Add Col Double)
        , QC.testProperty
            "a + 0 == a :: Matrix C Col (Complex Double)"
            (prop_add_ident :: Prop_Add Col (Complex Double))

        , QC.testProperty
            "a - a == 0 :: Matrix C Row Double"
            (prop_add_inv :: Prop_Add Row Double)
        , QC.testProperty
            "a - a == 0 :: Matrix C Row (Complex Double)"
            (prop_add_inv :: Prop_Add Row (Complex Double))
        , QC.testProperty
            "a - a == 0 :: Matrix C Col Double"
            (prop_add_inv :: Prop_Add Col Double)
        , QC.testProperty
            "a - a == 0 :: Matrix C Col (Complex Double)"
            (prop_add_inv :: Prop_Add Col (Complex Double))

        , QC.testProperty
            "a + b == b + a :: Matrix C Row Double"
            (prop_add_commute :: Prop_Add Row Double)
        , QC.testProperty
            "a + b == b + a :: Matrix C Row (Complex Double)"
            (prop_add_commute :: Prop_Add Row (Complex Double))
        , QC.testProperty
            "a + b == b + a :: Matrix C Col Double"
            (prop_add_commute :: Prop_Add Col Double)
        , QC.testProperty
            "a + b == b + a :: Matrix C Col (Complex Double)"
            (prop_add_commute :: Prop_Add Col (Complex Double))
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
    , testGroup "Involutive"
        [
        ]
    ]

type Prop_Eq_Trans fmt ord a = (Matrix fmt ord a, Matrix fmt ord a) -> Bool

prop_eq_trans :: Eq (Matrix fmt ord a) => Prop_Eq_Trans fmt ord a
prop_eq_trans (a, _) = a == a

prop_aeq_trans :: AEq.AEq (Matrix fmt ord a) => Prop_Eq_Trans fmt ord a
prop_aeq_trans (a, _) = a AEq.~== a

prop_eeq_trans :: AEq.AEq (Matrix fmt ord a) => Prop_Eq_Trans fmt ord a
prop_eeq_trans (a, _) = a AEq.=== a

type Prop_Fmt_Id fmt ord a = (Matrix fmt ord a, Matrix fmt ord a) -> Property

prop_fmt_id_C :: (Eq a, OrderR ord, Show a, Unbox a) => Prop_Fmt_Id C ord a
prop_fmt_id_C (a, _) = a === (compress . decompress) a

prop_fmt_id_U :: (Eq a, OrderR ord, Show a, Unbox a) => Prop_Fmt_Id U ord a
prop_fmt_id_U (a, _) = a === (decompress . compress) a

type Prop_Add_Assoc ord a = (Matrix C ord a, Matrix C ord a, Matrix C ord a) -> Property

prop_add_assoc :: (Eq a, Eq (Matrix C ord a), Num a, OrderR ord, Show a, Unbox a) => Prop_Add_Assoc ord a
prop_add_assoc (a, b, c) = (a `add` b) `add` c === a `add` (b `add` c)

type Prop_Add ord a = (Matrix C ord a, Matrix C ord a) -> Property

prop_add_ident :: (Eq a, Eq (Matrix C ord a), Num a, OrderR ord, Show a, Unbox a) => Prop_Add ord a
prop_add_ident (a, _) = (add a $ set dim (view dim a) empty) === a

prop_add_inv :: (Eq a, Eq (Matrix C ord a), Num a, OrderR ord, Show a, Unbox a) => Prop_Add ord a
prop_add_inv (a, _) = add a (over each negate a) === (over each (const 0) a)

prop_add_commute :: (Eq a, Eq (Matrix C ord a), Num a, OrderR ord, Show a, Unbox a) => Prop_Add ord a
prop_add_commute (a, b) = add a b === add b a
