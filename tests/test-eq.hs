{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Complex
import Data.MorallyEq
import Data.Vector.Unboxed (Unbox)
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck

import Numeric.LinearAlgebra.Sparse
import Numeric.LinearAlgebra.Sparse.QuickCheck ()

main :: IO ()
main = defaultMain $ testGroup "Eq Properties"
    [ testGroup "Matrix C Row Double"
        [ QC.testProperty "a == a"
            (prop_eq_trans :: Prop_eq C Row Double)
        , QC.testProperty "not (morallyEq a b) ==> a /= b"
            (prop_morallyneq_implies_neq :: Prop_morallyeq C Row Double)
        ]
    , testGroup "Matrix C Col Double"
        [ QC.testProperty "a == a"
            (prop_eq_trans :: Prop_eq C Col Double)
        , QC.testProperty "not (morallyEq a b) ==> a /= b"
            (prop_morallyneq_implies_neq :: Prop_morallyeq C Col Double)
        ]
    , testGroup "Matrix C Row (Complex Double)"
        [ QC.testProperty "a == a"
            (prop_eq_trans :: Prop_eq C Row (Complex Double))
        , QC.testProperty "not (morallyEq a b) ==> a /= b"
            (prop_morallyneq_implies_neq :: Prop_morallyeq C Row (Complex Double))
        ]
    , testGroup "Matrix C Col (Complex Double)"
        [ QC.testProperty "a == a"
            (prop_eq_trans :: Prop_eq C Col (Complex Double))
        , QC.testProperty "not (morallyEq a b) ==> a /= b"
            (prop_morallyneq_implies_neq :: Prop_morallyeq C Col (Complex Double))
        ]
    , testGroup "Matrix U Row Double"
        [ QC.testProperty "a == a"
            (prop_eq_trans :: Prop_eq U Row Double)
        , QC.testProperty "not (morallyEq a b) ==> a /= b"
            (prop_morallyneq_implies_neq :: Prop_morallyeq U Row Double)
        ]
    , testGroup "Matrix U Col Double"
        [ QC.testProperty "a == a"
            (prop_eq_trans :: Prop_eq U Col Double)
        , QC.testProperty "not (morallyEq a b) ==> a /= b"
            (prop_morallyneq_implies_neq :: Prop_morallyeq U Col Double)
        ]
    , testGroup "Matrix U Row (Complex Double)"
        [ QC.testProperty "a == a"
            (prop_eq_trans :: Prop_eq U Row (Complex Double))
        , QC.testProperty "not (morallyEq a b) ==> a /= b"
            (prop_morallyneq_implies_neq :: Prop_morallyeq U Row (Complex Double))
        ]
    , testGroup "Matrix U Col (Complex Double)"
        [ QC.testProperty "a == a"
            (prop_eq_trans :: Prop_eq U Col (Complex Double))
        , QC.testProperty "not (morallyEq a b) ==> a /= b"
            (prop_morallyneq_implies_neq :: Prop_morallyeq U Col (Complex Double))
        ]
    ]

type Constrain fmt or a =
    (Eq a, Format fmt, Orient or, Show (Matrix fmt or a), Unbox a)

type Prop_eq fmt ord a = Matrix fmt ord a -> Property

prop_eq_trans :: Constrain fmt or a => Prop_eq fmt or a
prop_eq_trans a = a === a

type Prop_morallyeq fmt ord a = (Matrix fmt ord a, Matrix fmt ord a) -> Property

prop_morallyneq_implies_neq :: (MorallyEq a, Constrain fmt or a, Show a)
                            => Prop_morallyeq fmt or a
prop_morallyneq_implies_neq (a, b) =
    counterexample (show a ++ " == " ++ show b)
    $ not (morallyEq a b) ==> a /= b
