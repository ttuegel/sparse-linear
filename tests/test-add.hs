{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Lens
import Data.Complex
import Data.MorallyEq
import Data.Vector.Unboxed (Unbox)
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck

import Numeric.LinearAlgebra.Sparse
import Numeric.LinearAlgebra.Sparse.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "Additive Properties"
    [ testGroup "Matrix C Row Double"
        [ QC.testProperty "(a + b) + c == a + (b + c)"
            (prop_add_assoc :: Prop_add_assoc C Row Double)
        , QC.testProperty "a + 0 == a"
            (prop_add_ident :: Prop_add_ident C Row Double)
        , QC.testProperty "a - a == 0"
            (prop_add_inv :: Prop_add_ident C Row Double)
        , QC.testProperty "a + b == b + a"
            (prop_add_commute :: Prop_add_commute C Row Double)
        , QC.testProperty "c(a + b) == cb + ca"
            (prop_add_linear :: Prop_add_linear C Row Double)
        ]
    , testGroup "Matrix C Col Double"
        [ QC.testProperty "(a + b) + c == a + (b + c)"
            (prop_add_assoc :: Prop_add_assoc C Col Double)
        , QC.testProperty "a + 0 == a"
            (prop_add_ident :: Prop_add_ident C Col Double)
        , QC.testProperty "a - a == 0"
            (prop_add_inv :: Prop_add_ident C Col Double)
        , QC.testProperty "a + b == b + a"
            (prop_add_commute :: Prop_add_commute C Col Double)
        , QC.testProperty "c(a + b) == cb + ca"
            (prop_add_linear :: Prop_add_linear C Col Double)
        ]
    , testGroup "Matrix C Row (Complex Double)"
        [ QC.testProperty "(a + b) + c == a + (b + c)"
            (prop_add_assoc :: Prop_add_assoc C Row (Complex Double))
        , QC.testProperty "a + 0 == a"
            (prop_add_ident :: Prop_add_ident C Row (Complex Double))
        , QC.testProperty "a - a == 0"
            (prop_add_inv :: Prop_add_ident C Row (Complex Double))
        , QC.testProperty "a + b == b + a"
            (prop_add_commute :: Prop_add_commute C Row (Complex Double))
        , QC.testProperty "c(a + b) == cb + ca"
            (prop_add_linear :: Prop_add_linear C Row (Complex Double))
        ]
    , testGroup "Matrix C Col (Complex Double)"
        [ QC.testProperty "(a + b) + c == a + (b + c)"
            (prop_add_assoc :: Prop_add_assoc C Col (Complex Double))
        , QC.testProperty "a + 0 == a"
            (prop_add_ident :: Prop_add_ident C Col (Complex Double))
        , QC.testProperty "a - a == 0"
            (prop_add_inv :: Prop_add_ident C Col (Complex Double))
        , QC.testProperty "a + b == b + a"
            (prop_add_commute :: Prop_add_commute C Col (Complex Double))
        , QC.testProperty "c(a + b) == cb + ca"
            (prop_add_linear :: Prop_add_linear C Col (Complex Double))
        ]
    , testGroup "Matrix U Row Double"
        [ QC.testProperty "(a + b) + c == a + (b + c)"
            (prop_add_assoc :: Prop_add_assoc U Row Double)
        , QC.testProperty "a + 0 == a"
            (prop_add_ident :: Prop_add_ident U Row Double)
        , QC.testProperty "a - a == 0"
            (prop_add_inv :: Prop_add_ident U Row Double)
        , QC.testProperty "a + b == b + a"
            (prop_add_commute :: Prop_add_commute U Row Double)
        , QC.testProperty "c(a + b) == cb + ca"
            (prop_add_linear :: Prop_add_linear U Row Double)
        ]
    , testGroup "Matrix U Col Double"
        [ QC.testProperty "(a + b) + c == a + (b + c)"
            (prop_add_assoc :: Prop_add_assoc U Col Double)
        , QC.testProperty "a + 0 == a"
            (prop_add_ident :: Prop_add_ident U Col Double)
        , QC.testProperty "a - a == 0"
            (prop_add_inv :: Prop_add_ident U Col Double)
        , QC.testProperty "a + b == b + a"
            (prop_add_commute :: Prop_add_commute U Col Double)
        , QC.testProperty "c(a + b) == cb + ca"
            (prop_add_linear :: Prop_add_linear U Col Double)
        ]
    , testGroup "Matrix U Row (Complex Double)"
        [ QC.testProperty "(a + b) + c == a + (b + c)"
            (prop_add_assoc :: Prop_add_assoc U Row (Complex Double))
        , QC.testProperty "a + 0 == a"
            (prop_add_ident :: Prop_add_ident U Row (Complex Double))
        , QC.testProperty "a - a == 0"
            (prop_add_inv :: Prop_add_ident U Row (Complex Double))
        , QC.testProperty "a + b == b + a"
            (prop_add_commute :: Prop_add_commute U Row (Complex Double))
        , QC.testProperty "c(a + b) == cb + ca"
            (prop_add_linear :: Prop_add_linear U Row (Complex Double))
        ]
    , testGroup "Matrix U Col (Complex Double)"
        [ QC.testProperty "(a + b) + c == a + (b + c)"
            (prop_add_assoc :: Prop_add_assoc U Col (Complex Double))
        , QC.testProperty "a + 0 == a"
            (prop_add_ident :: Prop_add_ident U Col (Complex Double))
        , QC.testProperty "a - a == 0"
            (prop_add_inv :: Prop_add_ident U Col (Complex Double))
        , QC.testProperty "a + b == b + a"
            (prop_add_commute :: Prop_add_commute U Col (Complex Double))
        , QC.testProperty "c(a + b) == cb + ca"
            (prop_add_linear :: Prop_add_linear U Col (Complex Double))
        ]
    ]

type Constrain fmt or a =
    (Eq a, Format fmt, MorallyEq a, Num a, Orient or, Show a, Show (Matrix fmt or a), Unbox a)

type Prop_add_ident fmt or a = Matrix fmt or a -> Property

prop_add_ident :: Constrain fmt or a => Prop_add_ident fmt or a
prop_add_ident a = (add a $ over each (const 0) a) === a

prop_add_inv :: Constrain fmt or a => Prop_add_ident fmt or a
prop_add_inv a = add a (over each negate a) === (over each (const 0) a)

type Prop_add_commute fmt or a = (Matrix fmt or a, Matrix fmt or a) -> Property

prop_add_commute :: Constrain fmt or a => Prop_add_commute fmt or a
prop_add_commute (matchDims2 -> (a, b)) = (add a b) ~== (add b a)

type Prop_add_linear fmt or a =
    (Matrix fmt or a, Matrix fmt or a) -> a -> Property

prop_add_linear :: Constrain fmt or a => Prop_add_linear fmt or a
prop_add_linear (matchDims2 -> (a, b)) factor =
    (scale (add a b)) ~== (add (scale a) (scale b))
  where scale = over each (* factor)

type Prop_add_assoc fmt or a =
    (Matrix fmt or a, Matrix fmt or a, Matrix fmt or a) -> Property

prop_add_assoc :: Constrain fmt or a => Prop_add_assoc fmt or a
prop_add_assoc (matchDims3 -> (a, b, c)) =
    ((a `add` b) `add` c) ~== (a `add` (b `add` c))
