{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PolyKinds #-}
module Main where

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
        [
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
