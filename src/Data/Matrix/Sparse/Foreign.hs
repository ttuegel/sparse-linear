{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Matrix.Sparse.Foreign
       ( fromCs, withConstCs
       ) where

import Control.Applicative
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.ForeignPtr.Safe (newForeignPtr)
import Foreign.Marshal.Alloc (free, finalizerFree)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack (errorWithStackTrace)

import Data.Cs
import Data.Matrix.Sparse.Compress
import Data.Matrix.Sparse.Type

fromCs :: CxSparse a => Bool -> Ptr (Cs a) -> IO (Matrix a)
fromCs doDedup _ptr
  | _ptr == nullPtr = errorWithStackTrace "fromCs: null pointer"
  | otherwise = do
      Cs{..} <- peek _ptr
      let nr = fromIntegral m
          nc = fromIntegral n
          nzmax_ = fromIntegral nzmax
          mkVector ptr len =
            MV.unsafeFromForeignPtr0
            <$> newForeignPtr finalizerFree ptr
            <*> pure len
      _rows <- mkVector i nzmax_
      _vals <- mkVector x nzmax_
      mat <- if nz < 0
                then do
                  cols <- V.unsafeFromForeignPtr0
                          <$> newForeignPtr finalizerFree p
                          <*> pure (nc + 1)
                  if doDedup
                    then deduplicate nr nc cols _rows _vals
                    else do
                      _rows <- V.unsafeFreeze _rows
                      _vals <- V.unsafeFreeze _vals
                      return Matrix
                        { nRows = nr
                        , nColumns = nc
                        , columnPointers = cols
                        , rowIndices = _rows
                        , values = _vals
                        }
             else do
                  cols <- mkVector p nzmax_
                  compress nr nc _rows cols _vals
      free _ptr
      return mat

withConstCs :: CxSparse a => Matrix a -> (Ptr (Cs a) -> IO b) -> IO b
withConstCs Matrix{..} act = do
    let nzmax = fromIntegral $ V.length values
        m = fromIntegral $ nRows
        n = fromIntegral $ nColumns
        nz = -1
    V.unsafeWith (V.map fromIntegral columnPointers) $ \p ->
      V.unsafeWith (V.map fromIntegral rowIndices) $ \i ->
      V.unsafeWith values $ \x ->
      with Cs{..} act
