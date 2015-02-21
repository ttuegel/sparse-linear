module Data.Matrix.Sparse.ScatterGather where

import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Algorithms.Intro as Intro
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as UM

unsafeScatter
  :: (Num a, PrimMonad m, Unbox a)
  => MVector (PrimState m) (Bool, a) -- ^ workspace
  -> a -- ^ multiplicative factor
  -> Vector (Int, a) -- ^ input
  -> MVector (PrimState m) Int -- ^ pattern
  -> Int -- ^ population of pattern
  -> m Int -- ^ population of pattern after scatter
{-# INLINE unsafeScatter #-}
unsafeScatter = \work alpha as ixs _pop -> do
  let (markers, xs) = UM.unzip work
      unsafeScatter_go _pop (i, ax) = do
        marked <- UM.unsafeRead markers i
        if marked
          then do
            x <- UM.unsafeRead xs i
            UM.unsafeWrite xs i $! alpha * ax + x
            return _pop
          else do
            UM.unsafeWrite markers i True
            UM.unsafeWrite ixs _pop i
            UM.unsafeWrite xs i $! alpha * ax
            return $! _pop + 1
  U.foldM' unsafeScatter_go _pop as

unsafeGather
  :: (PrimMonad m, Unbox a)
  => MVector (PrimState m) (Bool, a) -- ^ workspace
  -> MVector (PrimState m) Int -- ^ pattern
  -> MVector (PrimState m) a -- ^ destination
  -> Int -- ^ population
  -> m ()
{-# INLINE unsafeGather #-}
unsafeGather = \work ixs dst pop -> do
  let (markers, xs) = UM.unzip work
  Intro.sortByBounds compare ixs 0 pop
  U.forM_ (U.enumFromN 0 pop) $ \i -> do
    ix <- UM.unsafeRead ixs i
    UM.unsafeRead xs ix >>= UM.unsafeWrite dst i
    UM.unsafeWrite markers ix False
  UM.clear xs
