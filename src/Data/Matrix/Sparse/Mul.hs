module Data.Matrix.Sparse.Mul where

import Control.Applicative
import Control.Monad.ST (runST)
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Data.Matrix.Sparse.ScatterGather
import Data.Matrix.Sparse.Slice

unsafeMul
  :: (Num a, Unbox a)
  => Int -> Int
  -> Vector Int -> Vector (Int, a)
  -> Vector Int -> Vector (Int, a)
  -> (Vector Int, Vector (Int, a))
{-# INLINE unsafeMul #-}
unsafeMul = \rdim cdim ptrsA entriesA ptrsB entriesB -> runST $ do
  _ptrs <- MV.new (cdim + 1)
  MV.unsafeWrite _ptrs 0 0

  work <- MV.zip <$> MV.replicate rdim False <*> MV.new rdim

  _entries <- MV.new 0

  let unsafeMul_go _entries c = do
        let len = MV.length _entries
        start <- MV.unsafeRead _ptrs c

        -- Grow the result if needed to ensure enough space for the incoming
        -- slice, but at least double the size of the result each time to
        -- reduce the number of allocations/copies required. Unlike when we do
        -- addition, we cannot just preallocate the entire result because the
        -- upper bound on the storage requirement is not very restrictive.
        _entries <- if len - start < rdim
                      then MV.unsafeGrow _entries (len + rdim)
                    else return _entries

        let (ixs, xs) = MV.unzip $ MV.unsafeSlice start rdim _entries
            sliceB = unsafeSlice ptrsB c entriesB

            unsafeMul_scatter _pop (p, b) = do
              let sliceA = unsafeSlice ptrsA p entriesA
              unsafeScatter work b sliceA ixs _pop

        pop <- V.foldM' unsafeMul_scatter 0 sliceB
        unsafeGather work ixs xs pop

        MV.unsafeWrite _ptrs (c + 1) (start + pop)

        return _entries

  _entries <- V.foldM' unsafeMul_go _entries $ V.enumFromN 0 cdim

  _ptrs <- V.unsafeFreeze _ptrs
  let nz' = V.last _ptrs

  _entries <- V.unsafeFreeze $ MV.unsafeSlice 0 nz' _entries

  return (_ptrs, _entries)
