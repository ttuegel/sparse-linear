{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Lens
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
    [ testGroup "Deduplication"
        [ QC.testProperty
            "dedup a == dedup (dedup a) :: Matrix C Row Double"
            (prop_dedup_idem :: PropEq C Row Double)
        , QC.testProperty
            "dedup a == dedup (dedup a) :: Matrix C Row Double"
            (prop_dedup_idem :: PropEq C Col Double)
        , QC.testProperty
            "dedup a == dedup (dedup a) :: Matrix C Row Double"
            (prop_dedup_idem :: PropEq C Row (Complex Double))
        , QC.testProperty
            "dedup a == dedup (dedup a) :: Matrix C Row Double"
            (prop_dedup_idem :: PropEq C Col (Complex Double))
        , QC.testProperty
            "dedup a == dedup (dedup a) :: Matrix U Row Double"
            (prop_dedup_idem :: PropEq U Row Double)
        , QC.testProperty
            "dedup a == dedup (dedup a) :: Matrix U Row Double"
            (prop_dedup_idem :: PropEq U Col Double)
        , QC.testProperty
            "dedup a == dedup (dedup a) :: Matrix U Row Double"
            (prop_dedup_idem :: PropEq U Row (Complex Double))
        , QC.testProperty
            "dedup a == dedup (dedup a) :: Matrix U Row Double"
            (prop_dedup_idem :: PropEq U Col (Complex Double))
        ]
    ]

type PropEq fmt ord a = (Matrix fmt ord a, Matrix fmt ord a) -> Property
prop_dedup_idem :: (Eq a, Format fmt, Num a, Orient or, Show a, Show (Matrix fmt or a), Unbox a) => PropEq fmt or a
prop_dedup_idem (a, _) = deduplicate a === deduplicate (deduplicate a)
