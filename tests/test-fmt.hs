{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Lens
import Data.Complex
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck

import Numeric.LinearAlgebra.Sparse
import Numeric.LinearAlgebra.Sparse.QuickCheck ()

main :: IO ()
main = defaultMain $ testGroup "Format Properties"
    [ testGroup "compress . decompress == id"
        [ QC.testProperty "Matrix C Row Double"
            (prop_fmt_id_C :: Prop_fmt C Row Double)
        , QC.testProperty "Matrix C Row (Complex Double))"
            (prop_fmt_id_C :: Prop_fmt C Row (Complex Double))
        , QC.testProperty "Matrix C Col Double"
            (prop_fmt_id_C :: Prop_fmt C Col Double)
        , QC.testProperty "Matrix C Col (Complex Double)"
            (prop_fmt_id_C :: Prop_fmt C Col (Complex Double))
        ]
    , testGroup "decompress . compress == id"
        [ QC.testProperty "Matrix U Row Double"
            (prop_fmt_id_U :: Prop_fmt U Row Double)
        , QC.testProperty "Matrix U Row (Complex Double))"
            (prop_fmt_id_U :: Prop_fmt U Row (Complex Double))
        , QC.testProperty "Matrix U Col Double"
            (prop_fmt_id_U :: Prop_fmt U Col Double)
        , QC.testProperty "Matrix U Col (Complex Double)"
            (prop_fmt_id_U :: Prop_fmt U Col (Complex Double))
        ]
    , testGroup "transpose . from transpose == id"
        [ QC.testProperty
            "transpose (transpose a) == a :: Matrix C Row Double"
            (prop_trans_from_trans :: Prop_fmt C Row Double)
        , QC.testProperty
            "transpose (transpose a) == a :: Matrix U Row Double"
            (prop_trans_from_trans :: Prop_fmt U Row Double)
        ]
    , testGroup "from transpose . transpose == id"
        [ QC.testProperty
            "transpose (transpose a) == a :: Matrix C Col Double"
            (prop_from_trans_trans :: Prop_fmt C Col Double)
        , QC.testProperty
            "transpose (transpose a) == a :: Matrix U Col Double"
            (prop_from_trans_trans :: Prop_fmt U Col Double)
        ]
    , testGroup "adjoint . from adjoint == id"
        [ QC.testProperty
            "transpose (transpose a) == a :: Matrix C Row (Complex Double)"
            (prop_adj_from_adj :: Prop_fmt C Row (Complex Double))
        , QC.testProperty
            "transpose (transpose a) == a :: Matrix U Row (Complex Double)"
            (prop_adj_from_adj :: Prop_fmt U Row (Complex Double))
        ]
    , testGroup "from adjoint . adjoint == id"
        [ QC.testProperty
            "transpose (transpose a) == a :: Matrix C Col (Complex Double)"
            (prop_from_adj_adj :: Prop_fmt C Col (Complex Double))
        , QC.testProperty
            "transpose (transpose a) == a :: Matrix U Col (Complex Double)"
            (prop_from_adj_adj :: Prop_fmt U Col (Complex Double))
        ]
    ]

type Constrain fmt or a =
    (Eq a, Format fmt, Orient or, Show (Matrix fmt or a), Unbox a)

type Prop_fmt fmt ord a = Matrix fmt ord a -> Property

prop_fmt_id_C :: Constrain C ord a => Prop_fmt C ord a
prop_fmt_id_C a = a === view (from compress . compress) a

prop_fmt_id_U :: Constrain U ord a => Prop_fmt U ord a
prop_fmt_id_U a = a === view (compress . from compress) a

prop_trans_from_trans  :: Constrain fmt Row a => Prop_fmt fmt Row a
prop_trans_from_trans a = view (transpose . from transpose) a === a

prop_from_trans_trans :: Constrain fmt Col a => Prop_fmt fmt Col a
prop_from_trans_trans a = view (from transpose . transpose) a === a

prop_adj_from_adj :: (Constrain fmt Row (Complex a), RealFloat a, Unbox a)
                  => Prop_fmt fmt Row (Complex a)
prop_adj_from_adj a = view (adjoint . from adjoint) a === a

prop_from_adj_adj :: (Constrain fmt Col (Complex a), RealFloat a, Unbox a)
                  => Prop_fmt fmt Col (Complex a)
prop_from_adj_adj a = view (from adjoint . adjoint) a === a
