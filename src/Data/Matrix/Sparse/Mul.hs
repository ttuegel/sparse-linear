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
  ptrs <- MV.new (cdim + 1)
  MV.unsafeWrite ptrs 0 0

  work <- MV.zip <$> MV.replicate rdim False <*> MV.new rdim

  _entries <- MV.new 0

  let unsafeMul_go _entries c = do
        let len = MV.length _entries
        _entries <- MV.unsafeGrow _entries rdim

        start <- MV.unsafeRead ptrs c
        let (pat, dat) = MV.unzip $ MV.unsafeSlice start rdim _entries

        let sliceB = unsafeSlice ptrsB c entriesB
            unsafeMul_scatter _pop (p, b) = do
              let sliceA = unsafeSlice ptrsA p entriesA
              unsafeScatter work b sliceA pat _pop
        pop <- V.foldM' unsafeMul_scatter 0 sliceB
        unsafeGather work pat dat pop

        let len' = len + pop
        MV.unsafeWrite ptrs (c + 1) len'

        return $ MV.slice 0 len' _entries

  _entries <- V.foldM' unsafeMul_go _entries $ V.enumFromN 0 cdim

  (,) <$> V.unsafeFreeze ptrs <*> V.unsafeFreeze _entries
