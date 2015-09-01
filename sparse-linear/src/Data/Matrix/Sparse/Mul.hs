{-# LANGUAGE CPP #-}

module Data.Matrix.Sparse.Mul where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.ST (runST)
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

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
  _ptrs <- UM.new (cdim + 1)
  UM.unsafeWrite _ptrs 0 0

  work <- UM.zip <$> UM.replicate rdim False <*> UM.new rdim

  _entries <- UM.new 0

  let unsafeMul_go _entries c = do
        let len = UM.length _entries
        start <- UM.unsafeRead _ptrs c

        -- Grow the result if needed to ensure enough space for the incoming
        -- slice, but at least double the size of the result each time to
        -- reduce the number of allocations/copies required. Unlike when we do
        -- addition, we cannot just preallocate the entire result because the
        -- upper bound on the storage requirement is not very restrictive.
        _entries <- if len - start < rdim
                      then UM.unsafeGrow _entries (len + rdim)
                    else return _entries

        let (ixs, xs) = UM.unzip $ UM.unsafeSlice start rdim _entries
            sliceB = unsafeSlice ptrsB entriesB c

            unsafeMul_scatter _pop (p, b) = do
              let sliceA = unsafeSlice ptrsA entriesA p
              unsafeScatter work b sliceA ixs _pop

        pop <- U.foldM' unsafeMul_scatter 0 sliceB
        unsafeGather work ixs xs pop

        UM.unsafeWrite _ptrs (c + 1) (start + pop)

        return _entries

  _entries <- U.foldM' unsafeMul_go _entries $ U.enumFromN 0 cdim

  _ptrs <- U.unsafeFreeze _ptrs
  let nz' = U.last _ptrs

  _entries <- U.force <$> U.unsafeFreeze (UM.unsafeSlice 0 nz' _entries)

  return (_ptrs, _entries)
