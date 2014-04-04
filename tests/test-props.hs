module Main where

import Test.Tasty

import Numeric.LinearAlgebra.Sparse.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "Properties"
    [ testGroup "Format"
        [
        ]
    , testGroup "Equality"
        [
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
