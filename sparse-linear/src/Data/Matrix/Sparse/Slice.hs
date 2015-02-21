module Data.Matrix.Sparse.Slice where

import Data.Vector.Fusion.Stream (Stream)
import qualified Data.Vector.Generic as G
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Given a vector of pointers to slices in an array, return the indexed slice.
-- The following requirements are not checked:
-- * @index + 1 < length pointers@
-- * @last pointers == length data@
-- * for all @0 <= i < length pointers@, @pointers ! i <= pointers ! (i + 1)@
unsafeSlice
  :: Unbox a
  => Vector Int -- ^ pointers
  -> Int -- ^ index of slice
  -> Vector a -- ^ data
  -> Vector a
{-# INLINE unsafeSlice #-}
unsafeSlice = \ptrs ix dat ->
  let start = U.unsafeIndex ptrs ix
      end = U.unsafeIndex ptrs (ix + 1)
  in U.unsafeSlice start (end - start) dat

-- | Given a vector of pointers to slices in an array, return the indexed slice.
-- The following requirements are not checked:
-- * @index + 1 < length pointers@
-- * @last pointers == length data@
-- * for all @0 <= i < length pointers@, @pointers ! i <= pointers ! (i + 1)@
unsafeMSlice
  :: Unbox a
  => Vector Int -- ^ pointers
  -> Int -- ^ index of slice
  -> MVector s a -- ^ data
  -> MVector s a
{-# INLINE unsafeMSlice #-}
unsafeMSlice = \ptrs ix dat ->
  let start = U.unsafeIndex ptrs ix
      end = U.unsafeIndex ptrs (ix + 1)
  in UM.unsafeSlice start (end - start) dat

streamSlice
  :: Unbox a
  => Vector Int
  -> Int
  -> Vector a
  -> Stream a
{-# INLINE streamSlice #-}
streamSlice = \ptrs ix dat -> G.stream $ unsafeSlice ptrs ix dat
