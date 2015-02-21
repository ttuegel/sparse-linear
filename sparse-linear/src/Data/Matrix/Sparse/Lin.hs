module Data.Matrix.Sparse.Lin where

import Control.Applicative
import Control.Monad.ST (runST)
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Data.Matrix.Sparse.ScatterGather
import Data.Matrix.Sparse.Slice

unsafeLin
  :: (Num a, Unbox a)
  => Int -> Int
  -> a -> Vector Int -> Vector (Int, a)
  -> a -> Vector Int -> Vector (Int, a)
  -> (Vector Int, Vector (Int, a))
{-# INLINE unsafeLin #-}
unsafeLin = \odim idim a ptrsA entriesA b ptrsB entriesB -> runST $ do
  _ptrs <- UM.new (odim + 1)
  UM.unsafeWrite _ptrs 0 0

  -- This may allocate more memory than required (if the matrix patterns
  -- overlap), but at worst it only allocates min(U.last ptrsA, U.last ptrsB)
  -- more than required. This is much better than incremental allocation with
  -- copying!
  _entries <- UM.new $! U.last ptrsA + U.last ptrsB

  _work <- UM.zip <$> UM.replicate idim False <*> UM.new idim

  let unsafeLin_go n = do
        let sliceA = unsafeSlice ptrsA n entriesA
            sliceB = unsafeSlice ptrsB n entriesB
            dlen = U.length sliceA + U.length sliceB

        -- find the start of the current destination slice
        start <- UM.unsafeRead _ptrs n
        let (ixs, xs) = UM.unzip $ UM.slice start dlen _entries

        -- scatter/gather
        _pop <- unsafeScatter _work a sliceA ixs 0
        _pop <- unsafeScatter _work b sliceB ixs _pop
        unsafeGather _work ixs xs _pop

        -- record the start of the next slice
        UM.unsafeWrite _ptrs (n + 1) $! start + _pop

  U.mapM_ unsafeLin_go $ U.enumFromN 0 odim

  _ptrs <- U.unsafeFreeze _ptrs
  let nz' = U.last _ptrs
  _entries <- U.unsafeFreeze $ UM.unsafeSlice 0 nz' _entries

  return (_ptrs, _entries)
