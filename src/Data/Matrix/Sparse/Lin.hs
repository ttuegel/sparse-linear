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
{-# INLINE unsafeLin #-}
unsafeLin = \odim idim a ptrsA entriesA b ptrsB entriesB -> runST $ do
  ptrs <- MV.new (odim + 1)
  MV.unsafeWrite ptrs 0 0

  _entries <- MV.new 0
  _work <- MV.zip <$> MV.replicate idim False <*> MV.new idim

  let unsafeLin_go _entries n = do
        let len = MV.length _entries
            sliceA = unsafeSlice ptrsA n entriesA
            sliceB = unsafeSlice ptrsB n entriesB
            dlen = V.length sliceA + V.length sliceB

        -- grow destination just enough to accomodate current slice
        _entries <- MV.unsafeGrow _entries dlen

        -- find the start of the current destination slice
        start <- MV.unsafeRead ptrs n
        let (pat, dat) = MV.unzip $ MV.slice start dlen _entries

        -- scatter/gather
        _pop <- unsafeScatter _work a sliceA pat 0
        _pop <- unsafeScatter _work b sliceB pat _pop
        unsafeGather _work pat dat _pop

        -- record the start of the next slice
        MV.unsafeWrite ptrs (n + 1) $! start + _pop

        -- shrink destination
        return $! MV.slice 0 (len + _pop) _entries

  _entries <- V.foldM' unsafeLin_go _entries $ V.enumFromN 0 odim

  (,) <$> V.unsafeFreeze ptrs <*> V.unsafeFreeze _entries
