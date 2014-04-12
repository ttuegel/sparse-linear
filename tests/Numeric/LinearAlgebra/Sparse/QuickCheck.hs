{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.LinearAlgebra.Sparse.QuickCheck where

import Control.Applicative
import Control.Lens
import Data.Maybe (catMaybes)
import Data.MorallyEq
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as U
import Test.QuickCheck

import Numeric.LinearAlgebra.Sparse

instance (Show (Matrix fmt or a), Arbitrary a, Format fmt, Num a, Orient or, Show a, Unbox a) => Arbitrary (Matrix fmt or a) where
    arbitrary = do
        r <- abs <$> arbitrarySizedIntegral `suchThat` (> 0)
        c <- abs <$> arbitrarySizedIntegral `suchThat` (> 0)
        let nnz = r * c
        rows <- vectorOf nnz $ arbitrary `suchThat` (\i -> i < r && i >= 0)
        cols <- vectorOf nnz $ arbitrary `suchThat` (\i -> i < c && i >= 0)
        coeffs <- vector nnz
        let indices (i, j, _) (k, l, _) = i == k && j == l
        return $ deduplicate $ pack r c $ U.fromList $ zip3 rows cols coeffs

    shrink mat =
        let (r, c) = mat ^. dim
            shrinkRow
                | r > 1 = Just $ mat & dim .~ (r - 1, c)
                | otherwise = Nothing
            shrinkCol
                | c > 1 = Just $ mat & dim .~ (r, c - 1)
                | otherwise = Nothing
        in catMaybes [shrinkRow, shrinkCol]

instance (Arbitrary a, Unbox a) => Arbitrary (Vector a) where
    arbitrary = do
        len <- abs <$> arbitrarySizedIntegral `suchThat` (> 0)
        U.fromList <$> vector len

    shrink v
        | U.length v > 1 = [U.take (U.length v - 1) v]
        | otherwise = []

matchDims2 :: (Format fmt, Orient or, Orient or', Unbox a) => (Matrix fmt or a, Matrix fmt or' a) -> (Matrix fmt or a, Matrix fmt or' a)
matchDims2 (a, b) =
    let dima = a ^. dim
        dimb = b ^. dim
    in (a & dim .~ min dima dimb, b & dim .~ min dima dimb)

matchDims3 :: (Format fmt, Orient or, Orient or', Orient or'', Unbox a) => (Matrix fmt or a, Matrix fmt or' a, Matrix fmt or'' a) -> (Matrix fmt or a, Matrix fmt or' a, Matrix fmt or'' a)
matchDims3 (a, b, c) =
    let dim' = minimum [a ^. dim, b ^. dim, c ^. dim]
    in (a & dim .~ dim', b & dim .~ dim', c & dim .~ dim')

matchDimsV :: (Format fmt, Orient or, Unbox a) => (Matrix fmt or a, Vector a) -> (Matrix fmt or a, Vector a)
matchDimsV (m, v) =
    let c = minimum [U.length v, m ^. dim . _2]
    in (m & dim . _2 .~ c, U.take c v)

matchDimsV2 :: (Format fmt, Orient or, Orient or', Unbox a) => (Matrix fmt or a, Matrix fmt or' a, Vector a) -> (Matrix fmt or a, Matrix fmt or' a, Vector a)
matchDimsV2 (m, n, v) =
    let (m', n') = matchDims2 (m, n)
        c = minimum [U.length v, m' ^. dim . _2, n' ^. dim . _2]
    in (m' & dim . _2 .~ c, n' & dim . _2 .~ c, U.take c v)

(~==) :: (MorallyEq a, Show a) => a -> a -> Property
(~==) a b = counterexample (show a ++ " /= " ++ show b) $ a `morallyEq` b
