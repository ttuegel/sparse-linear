{-# LANGUAGE ConstraintKinds #-}
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
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck

import Numeric.LinearAlgebra.Sparse
import Numeric.LinearAlgebra.Sparse.QuickCheck ()

main :: IO ()
main = defaultMain $ testGroup "Lens Properties"
    [ testGroup "Matrix C Row Double"
        [ QC.testProperty "over _slices id == id"
            (prop_over_slices_id :: Prop_over_slices C Row Double)
        , QC.testProperty "(over _slices (const empty) x) ^. dim == x ^. dim"
            (prop_over_slices_dim :: Prop_over_slices C Row Double)
        ]
    , testGroup "Matrix C Col Double"
        [ QC.testProperty "over _slices id == id"
            (prop_over_slices_id :: Prop_over_slices C Col Double)
        , QC.testProperty "(over _slices (const empty) x) ^. dim == x ^. dim"
            (prop_over_slices_dim :: Prop_over_slices C Col Double)
        ]
    , testGroup "Matrix C Row (Complex Double)"
        [ QC.testProperty "over _slices id == id"
            (prop_over_slices_id :: Prop_over_slices C Row (Complex Double))
        , QC.testProperty "(over _slices (const empty) x) ^. dim == x ^. dim"
            (prop_over_slices_dim :: Prop_over_slices C Row (Complex Double))
        ]
    , testGroup "Matrix C Col (Complex Double)"
        [ QC.testProperty "over _slices id == id"
            (prop_over_slices_id :: Prop_over_slices C Col (Complex Double))
        , QC.testProperty "(over _slices (const empty) x) ^. dim == x ^. dim"
            (prop_over_slices_dim :: Prop_over_slices C Col (Complex Double))
        ]
    , testGroup "Matrix U Row Double"
        [ QC.testProperty "over _slices id == id"
            (prop_over_slices_id :: Prop_over_slices U Row Double)
        , QC.testProperty "(over _slices (const empty) x) ^. dim == x ^. dim"
            (prop_over_slices_dim :: Prop_over_slices U Row Double)
        ]
    , testGroup "Matrix U Col Double"
        [ QC.testProperty "over _slices id == id"
            (prop_over_slices_id :: Prop_over_slices U Col Double)
        , QC.testProperty "(over _slices (const empty) x) ^. dim == x ^. dim"
            (prop_over_slices_dim :: Prop_over_slices U Col Double)
        ]
    , testGroup "Matrix U Row (Complex Double)"
        [ QC.testProperty "over _slices id == id"
            (prop_over_slices_id :: Prop_over_slices U Row (Complex Double))
        , QC.testProperty "(over _slices (const empty) x) ^. dim == x ^. dim"
            (prop_over_slices_dim :: Prop_over_slices U Row (Complex Double))
        ]
    , testGroup "Matrix U Col (Complex Double)"
        [ QC.testProperty "over _slices id == id"
            (prop_over_slices_id :: Prop_over_slices U Col (Complex Double))
        , QC.testProperty "(over _slices (const empty) x) ^. dim == x ^. dim"
            (prop_over_slices_dim :: Prop_over_slices U Col (Complex Double))
        ]
    ]

type Constrain fmt or a =
    (Eq a, Format fmt, Orient or, Show (Matrix fmt or a), Unbox a)

type Prop_over_slices fmt or a =
    Matrix fmt or a -> Property

prop_over_slices_id :: Constrain fmt or a => Prop_over_slices fmt or a
prop_over_slices_id x = over _slices id x === x

prop_over_slices_dim :: Constrain fmt or a => Prop_over_slices fmt or a
prop_over_slices_dim x = y ^. dim === x ^. dim
  where
    y = (over _slices (const U.empty) x) `asTypeOf` x
