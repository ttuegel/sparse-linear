module Data.Matrix.Sparse.Lin where

import Control.Applicative
import Control.Monad.ST (runST)
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Data.Matrix.Sparse.ScatterGather
import Data.Matrix.Sparse.Slice

unsafeLin
  :: (Num a, Unbox a)
  => Int -> Int
  -> a -> Vector Int -> Vector (Int, a)
  -> a -> Vector Int -> Vector (Int, a)
  -> (Vector Int, Vector (Int, a))
unsafeLin = \odim idim a ptrsA entriesA b ptrsB entriesB -> runST $ do
  _ptrs <- MV.new (odim + 1)
  MV.unsafeWrite _ptrs 0 0

  -- This may allocate more memory than required (if the matrix patterns
  -- overlap), but at worst it only allocates min(V.last ptrsA, V.last ptrsB)
  -- more than required. This is much better than incremental allocation with
  -- copying!
  _entries <- MV.new $! V.last ptrsA + V.last ptrsB

  _work <- MV.zip <$> MV.replicate idim False <*> MV.new idim

  let unsafeLin_go n = do
        let sliceA = unsafeSlice ptrsA n entriesA
            sliceB = unsafeSlice ptrsB n entriesB
            dlen = V.length sliceA + V.length sliceB

        -- find the start of the current destination slice
        start <- MV.unsafeRead _ptrs n
        let (ixs, xs) = MV.unzip $ MV.slice start dlen _entries

        -- scatter/gather
        _pop <- unsafeScatter _work a sliceA ixs 0
        _pop <- unsafeScatter _work b sliceB ixs _pop
        unsafeGather _work ixs xs _pop

        -- record the start of the next slice
        MV.unsafeWrite _ptrs (n + 1) $! start + _pop

  V.mapM_ unsafeLin_go $ V.enumFromN 0 odim

  _ptrs <- V.unsafeFreeze _ptrs
  let nz' = V.last _ptrs
  _entries <- V.unsafeFreeze $ MV.unsafeSlice 0 nz' _entries

  return (_ptrs, _entries)
