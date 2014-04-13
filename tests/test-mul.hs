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
main = defaultMain $ testGroup "Multiplicative Properties"
    [ testGroup "Matrix C Row Double"
        [ QC.testProperty "1 * a == a"
            (prop_mul_ident_l :: Prop_mul_ident Row Double)
        ]
    , testGroup "Matrix C Col Double"
        [ QC.testProperty "a * 1 == a"
            (prop_mul_ident_r :: Prop_mul_ident Col Double)
        , QC.testProperty "(a * b) * c == a * (b * c)"
            (prop_mul_assoc :: Prop_mul_assoc Double)
        , QC.testProperty "transpose (a * b) == (transpose b * transpose a)"
            (prop_mul_trans :: Prop_mul_trans Double)
        ]
    , testGroup "Matrix C Row (Complex Double)"
        [ QC.testProperty "1 * a == a"
            (prop_mul_ident_l :: Prop_mul_ident Row (Complex Double))
        ]
    , testGroup "Matrix C Col (Complex Double)"
        [ QC.testProperty "a * 1 == a"
            (prop_mul_ident_r :: Prop_mul_ident Col (Complex Double))
        , QC.testProperty "(a * b) * c == a * (b * c)"
            (prop_mul_assoc :: Prop_mul_assoc (Complex Double))
        , QC.testProperty "adjoint (a * b) == (adjoint b * adjoint a)"
            (prop_mul_adj :: Prop_mul_trans (Complex Double))
        ]
    ]

type Constrain fmt or a =
    (Eq a, Format fmt, MorallyEq a, Num a, Orient or, Show a, Show (Matrix fmt or a), Unbox a)

type Prop_mul_ident or a = Matrix C or a -> Property

prop_mul_ident_r :: Constrain C Col a => Prop_mul_ident Col a
prop_mul_ident_r a = a `mul` (ident $ view (dim . _2) a) === a

prop_mul_ident_l :: Constrain C Row a => Prop_mul_ident Row a
prop_mul_ident_l a = (ident $ view (dim . _1) a) `mul` a === a

type Prop_mul_assoc a =
    (Matrix C Col a, Matrix C Row a, Matrix C Col a) -> Property

prop_mul_assoc :: (Constrain C Col a, Constrain C Row a) => Prop_mul_assoc a
prop_mul_assoc (matchDims3 -> (a, b, view reorder -> c)) = ab_c ~== a_bc
  where
    ab_c = (a `mul` b) `mul` c `asTypeOf` a
    a_bc = a `mul` (view (from reorder) b `mul` c)

type Prop_mul_trans a = (Matrix C Col a, Matrix C Row a) -> Property
prop_mul_trans :: (Constrain C Col a, Constrain C Row a) => Prop_mul_trans a
prop_mul_trans (matchDims2 -> (a, b)) =
    (view transpose (a `mul` b)) ~== (view transpose b `mul` view (from transpose) a)

prop_mul_adj :: (Constrain C Col (Complex a), Constrain C Row (Complex a), RealFloat a, Unbox a)
             => Prop_mul_trans (Complex a)
prop_mul_adj (matchDims2 -> (a, b)) =
    (view adjoint (a `mul` b)) ~== (view adjoint b `mul` view (from adjoint) a)
