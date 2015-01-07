module Data.Matrix.Sparse.ScatterGather where

import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Algorithms.Intro as Intro
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MV

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
  let (markers, xs) = MV.unzip work
      unsafeScatter_go _pop (i, ax) = do
        marked <- MV.unsafeRead markers i
        if marked
          then do
            x <- MV.unsafeRead xs i
            MV.unsafeWrite xs i $! alpha * ax + x
            return _pop
          else do
            MV.unsafeWrite markers i True
            MV.unsafeWrite ixs _pop i
            MV.unsafeWrite xs i $! alpha * ax
            return $! _pop + 1
  V.foldM' unsafeScatter_go _pop as

unsafeGather
  :: (PrimMonad m, Unbox a)
  => MVector (PrimState m) (Bool, a) -- ^ workspace
  -> MVector (PrimState m) Int -- ^ pattern
  -> MVector (PrimState m) a -- ^ destination
  -> Int -- ^ population
  -> m ()
{-# INLINE unsafeGather #-}
unsafeGather = \work ixs dst pop -> do
  let (markers, xs) = MV.unzip work
  Intro.sortByBounds compare ixs 0 pop
  V.forM_ (V.enumFromN 0 pop) $ \i -> do
    ix <- MV.unsafeRead ixs i
    MV.unsafeRead xs ix >>= MV.unsafeWrite dst i
    MV.unsafeWrite markers ix False
  MV.clear xs
