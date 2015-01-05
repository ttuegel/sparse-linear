{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Matrix.Sparse
    ( Matrix(..), cmap, nonZero
    , Orient(..), Trans, Indices(..), orient
    , compress, decompress
    , transpose, reorient
    , slices, fromSlices, slice
    , assertValid
    ) where

import qualified Data.Vector.Generic as V
import Data.Vector.Storable (Storable)
import GHC.Stack (errorWithStackTrace)

import Data.Matrix.Sparse.Compress
import Data.Matrix.Sparse.Type
import qualified Data.Vector.Sparse as S
import Data.Vector.Util (increasing, nondecreasing)

assertValid :: Storable a => Matrix or a -> Matrix or a
assertValid mat@Matrix{..}
  | not (nondecreasing pointers) =
      errorWithStackTrace "pointers are not nondecreasing"
  | V.length pointers /= dimM + 1 =
      errorWithStackTrace "wrong number of pointers"
  | V.length values /= (fromIntegral $ V.last pointers) =
      errorWithStackTrace "length values is wrong"
  | V.any (not . increasing . S.indices) (slices mat) =
      errorWithStackTrace "indices are not increasing"
  | otherwise = mat
