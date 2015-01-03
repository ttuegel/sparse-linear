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
    , columnLengths
    , module Data.Cs, fromCs, withConstCs
    ) where

import Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as V
import GHC.Stack (errorWithStackTrace)

import Data.Cs
import Data.Matrix.Sparse.Compress
import Data.Matrix.Sparse.Foreign
import Data.Matrix.Sparse.Type
import qualified Data.Vector.Sparse as SpV

toColumns :: Storable a => Matrix a -> [SpV.Vector a]
toColumns = \Matrix{..} ->
  let starts = map fromIntegral $ V.toList $ V.init columnPointers
      ends = map fromIntegral $ V.toList $ V.tail columnPointers
      lens = zipWith (-) ends starts
      chop :: Storable b => Vector b -> [Vector b]
      chop v = zipWith chop_go starts lens
        where
          chop_go n len
            | len > 0 = V.slice n len v
            | otherwise = V.empty
  in do
    (inds, vals) <- zip (chop rowIndices) (chop values)
    return SpV.Vector
      { SpV.dim = nRows
      , SpV.indices = inds
      , SpV.values = vals
      }

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
  | any (not . increasing . SpV.indices) (toColumns mat) =
      errorWithStackTrace "row indices are not increasing"
  | otherwise = mat

columnLengths :: Matrix a -> Vector CInt
columnLengths Matrix{..} =
  V.zipWith (-) (V.tail columnPointers) (V.init columnPointers)
