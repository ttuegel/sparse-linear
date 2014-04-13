{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Lens
import Data.Complex
import Data.Vector.Unboxed (Unbox)
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
            "dim (a) == dim (a + a) :: Matrix C Row Double"
            (prop_add :: Prop C Row Double)
        , QC.testProperty
            "dim (a) == dim (a + a) :: Matrix C Row Double"
            (prop_add :: Prop C Col Double)
        , QC.testProperty
            "dim (a) == dim (a + a) :: Matrix C Row (Complex Double)"
            (prop_add :: Prop C Row (Complex Double))
        , QC.testProperty
            "dim (a) == dim (a + a) :: Matrix C Row (Complex Double)"
            (prop_add :: Prop C Col (Complex Double))

        , QC.testProperty
            "dim (a) == dim (a * a) :: Matrix C Row Double"
            (prop_mul :: Prop C Row Double)
        , QC.testProperty
            "dim (a) == dim (a * a) :: Matrix C Row (Complex Double)"
            (prop_mul :: Prop C Row (Complex Double))

        , QC.testProperty
            "dim (a) == dim (a/2 + a/2) :: Matrix C Row Double"
            (prop_half_add :: Prop C Row Double)
        , QC.testProperty
            "dim (a) == dim (a/2 + a/2) :: Matrix C Row Double"
            (prop_half_add :: Prop C Col Double)
        , QC.testProperty
            "dim (a) == dim (a/2 + a/2) :: Matrix C Row (Complex Double)"
            (prop_half_add :: Prop C Row (Complex Double))
        , QC.testProperty
            "dim (a) == dim (a/2 + a/2) :: Matrix C Row (Complex Double)"
            (prop_half_add :: Prop C Col (Complex Double))
        ]
    ]

type Prop fmt or a = Matrix fmt or a -> Property

prop_add :: (Format fmt, Num a, Orient or, Show a, Show (Matrix fmt or a), Unbox a) => Prop fmt or a
prop_add a = a ^. dim === (a `add` a) ^. dim

prop_mul :: (Num a, Show a, Show (Matrix C Row a), Unbox a) => Prop C Row a
prop_mul a = dim' === b ^. dim
  where
    dim' = (a ^. dim . _2, a ^. dim . _2)
    b = ((a ^. transpose) `mul` a) `asTypeOf` a

prop_half_add :: (Fractional a, Orient or, Num a, Show a, Unbox a) => Prop C or a
prop_half_add a = view dim (half_a `add` half_a) === view dim a
  where
    half_a = over each (* 0.5) a
