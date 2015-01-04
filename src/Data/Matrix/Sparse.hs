{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Matrix.Sparse
    ( Matrix(..), cmap
    , compress, decompress
    , transpose
    , toColumns, assertValid
    , module Data.Cs, fromCs, withConstCs
    ) where

import Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as V
import GHC.Stack (errorWithStackTrace)

import Data.Cs
import Data.Matrix.Sparse.Compress
import Data.Matrix.Sparse.Foreign
import Data.Matrix.Sparse.Type
import qualified Data.Vector.Sparse as S

nondecreasing :: (Ord a, Storable a) => Vector a -> Bool
nondecreasing vec
  | V.null vec = True
  | otherwise = V.and $ V.zipWith (<=) (V.init vec) (V.tail vec)

increasing :: (Ord a, Storable a) => Vector a -> Bool
increasing vec
  | V.null vec = True
  | otherwise = V.and $ V.zipWith (<) (V.init vec) (V.tail vec)

assertValid :: Storable a => Matrix a -> Matrix a
assertValid mat@Matrix{..}
  | not (nondecreasing columnPointers) =
      errorWithStackTrace "column pointers are not nondecreasing"
  | V.length columnPointers /= nColumns + 1 =
      errorWithStackTrace "wrong number of column pointers"
  | V.length values /= (fromIntegral $ V.last columnPointers) =
      errorWithStackTrace "length values is wrong"
  | any (not . increasing . S.indices) (toColumns mat) =
      errorWithStackTrace "row indices are not increasing"
  | otherwise = mat
