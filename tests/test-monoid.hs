{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Complex
import Data.Monoid
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck

import Numeric.LinearAlgebra.Sparse
import Numeric.LinearAlgebra.Sparse.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "Monoid Properties"
    [ testGroup "Matrix C Row Double"
        [ QC.testProperty "mappend mempty x == x"
            (prop_mempty_id_left :: Prop_mempty_id C Row Double)
        , QC.testProperty "mappend x mempty == x"
            (prop_mempty_id_right :: Prop_mempty_id C Row Double)
        , QC.testProperty "mappend x (mappend y z) == mappend (mappend x y) z"
            (prop_mappend_assoc :: Prop_mappend_assoc C Row Double)
        ]
    , testGroup "Matrix C Col Double"
        [ QC.testProperty "mappend mempty x == x"
            (prop_mempty_id_left :: Prop_mempty_id C Col Double)
        , QC.testProperty "mappend x mempty == x"
            (prop_mempty_id_right :: Prop_mempty_id C Col Double)
        , QC.testProperty "mappend x (mappend y z) == mappend (mappend x y) z"
            (prop_mappend_assoc :: Prop_mappend_assoc C Col Double)
        ]
    , testGroup "Matrix C Row (Complex Double)"
        [ QC.testProperty "mappend mempty x == x"
            (prop_mempty_id_left :: Prop_mempty_id C Row (Complex Double))
        , QC.testProperty "mappend x mempty == x"
            (prop_mempty_id_right :: Prop_mempty_id C Row (Complex Double))
        , QC.testProperty "mappend x (mappend y z) == mappend (mappend x y) z"
            (prop_mappend_assoc :: Prop_mappend_assoc C Row (Complex Double))
        ]
    , testGroup "Matrix C Col (Complex Double)"
        [ QC.testProperty "mappend mempty x == x"
            (prop_mempty_id_left :: Prop_mempty_id C Col (Complex Double))
        , QC.testProperty "mappend x mempty == x"
            (prop_mempty_id_right :: Prop_mempty_id C Col (Complex Double))
        , QC.testProperty "mappend x (mappend y z) == mappend (mappend x y) z"
            (prop_mappend_assoc :: Prop_mappend_assoc C Col (Complex Double))
        ]
    , testGroup "Matrix U Row Double"
        [ QC.testProperty "mappend mempty x == x"
            (prop_mempty_id_left :: Prop_mempty_id U Row Double)
        , QC.testProperty "mappend x mempty == x"
            (prop_mempty_id_right :: Prop_mempty_id U Row Double)
        , QC.testProperty "mappend x (mappend y z) == mappend (mappend x y) z"
            (prop_mappend_assoc :: Prop_mappend_assoc U Row Double)
        ]
    , testGroup "Matrix U Col Double"
        [ QC.testProperty "mappend mempty x == x"
            (prop_mempty_id_left :: Prop_mempty_id U Col Double)
        , QC.testProperty "mappend x mempty == x"
            (prop_mempty_id_right :: Prop_mempty_id U Col Double)
        , QC.testProperty "mappend x (mappend y z) == mappend (mappend x y) z"
            (prop_mappend_assoc :: Prop_mappend_assoc U Col Double)
        ]
    , testGroup "Matrix U Row (Complex Double)"
        [ QC.testProperty "mappend mempty x == x"
            (prop_mempty_id_left :: Prop_mempty_id U Row (Complex Double))
        , QC.testProperty "mappend x mempty == x"
            (prop_mempty_id_right :: Prop_mempty_id U Row (Complex Double))
        , QC.testProperty "mappend x (mappend y z) == mappend (mappend x y) z"
            (prop_mappend_assoc :: Prop_mappend_assoc U Row (Complex Double))
        ]
    , testGroup "Matrix U Col (Complex Double)"
        [ QC.testProperty "mappend mempty x == x"
            (prop_mempty_id_left :: Prop_mempty_id U Col (Complex Double))
        , QC.testProperty "mappend x mempty == x"
            (prop_mempty_id_right :: Prop_mempty_id U Col (Complex Double))
        , QC.testProperty "mappend x (mappend y z) == mappend (mappend x y) z"
            (prop_mappend_assoc :: Prop_mappend_assoc U Col (Complex Double))
        ]
    ]

type Constrain fmt or a =
    (Eq a, Format fmt, Orient or, Show (Matrix fmt or a), Unbox a)

type Prop_mempty_id fmt or a = Matrix fmt or a -> Property

prop_mempty_id_left :: Constrain fmt or a => Prop_mempty_id fmt or a
prop_mempty_id_left x = mappend mempty x === x

prop_mempty_id_right :: Constrain fmt or a => Prop_mempty_id fmt or a
prop_mempty_id_right x = mappend x mempty === x

type Prop_mappend_assoc fmt or a =
    (Matrix fmt or a, Matrix fmt or a, Matrix fmt or a) -> Property

prop_mappend_assoc :: Constrain fmt or a => Prop_mappend_assoc fmt or a
prop_mappend_assoc (matchDims3 -> (x, y, z)) =
    mappend x (mappend y z) === mappend (mappend x y) z
