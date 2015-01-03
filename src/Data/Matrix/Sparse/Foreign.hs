{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Matrix.Sparse.Foreign
       ( fromCs, withConstCs
       ) where

import Control.Applicative
import qualified Data.Vector.Storable as V
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
            V.unsafeFromForeignPtr0
            <$> newForeignPtr finalizerFree ptr
            <*> pure len
      _rows <- mkVector i nzmax_
      _vals <- mkVector x nzmax_
      _cols <- mkVector p (if nz < 0 then nc + 1 else nzmax_)
      mat <- if nz < 0
             then if doDedup
                  then return $ compress nr nc _rows (decompress _cols) _vals
                  else return Matrix
                    { nRows = nr
                    , nColumns = nc
                    , columnPointers = _cols
                    , rowIndices = _rows
                    , values = _vals
                    }
             else return $ compress nr nc _rows _cols _vals
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
