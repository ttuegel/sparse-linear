{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Matrix.Sparse
    ( Matrix(..), cmap, nonZero
    , compress, decompress
    , transpose
    , toColumns, fromColumns
    , assertValid
    , module Data.Cs, fromCs, withConstCs
    ) where

import qualified Data.Vector.Generic as V
import Data.Vector.Storable (Storable)
import GHC.Stack (errorWithStackTrace)

import Data.Cs
import Data.Matrix.Sparse.Compress
import Data.Matrix.Sparse.Foreign
import Data.Matrix.Sparse.Type
import qualified Data.Vector.Sparse as S
import Data.Vector.Util (increasing, nondecreasing)

assertValid :: Storable a => Matrix a -> Matrix a
assertValid mat@Matrix{..}
  | not (nondecreasing columnPointers) =
      errorWithStackTrace "column pointers are not nondecreasing"
  | V.length columnPointers /= nColumns + 1 =
      errorWithStackTrace "wrong number of column pointers"
  | V.length values /= (fromIntegral $ V.last columnPointers) =
      errorWithStackTrace "length values is wrong"
  | V.any (not . increasing . S.indices) (toColumns mat) =
      errorWithStackTrace "row indices are not increasing"
  | otherwise = mat
