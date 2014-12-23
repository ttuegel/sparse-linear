{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Matrix.Sparse
    ( Cs(), fromCs, cs_free
    , Matrix(..), unsafeWithMatrix
    , unsafeWithTriples
    ) where

import Control.Monad (unless)
import Data.Complex
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.C.Types
import Foreign.ForeignPtr.Safe (FinalizerPtr, newForeignPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack

#include "cs.h"

data Cs a = Cs
    { nzmax :: !CInt  -- ^ maximum number of entries
    , m :: !CInt  -- ^ number of rows
    , n :: !CInt  -- ^ number of columns
    , p :: !(Ptr CInt)  -- ^ column pointers or indices
    , i :: !(Ptr CInt)  -- ^ row indices
    , x :: !(Ptr a)  -- ^ values
    , nz :: !CInt  -- ^ number of entries (triplet) or (-1) for compressed col
    }

instance Storable (Cs (Complex Double)) where
    sizeOf _ = #size cs_ci
    alignment _ = 256

    peek ptr = do
        nzmax <- (#peek cs_ci, nzmax) ptr
        m <- (#peek cs_ci, m) ptr
        n <- (#peek cs_ci, n) ptr
        p <- (#peek cs_ci, p) ptr
        i <- (#peek cs_ci, i) ptr
        x <- (#peek cs_ci, x) ptr
        nz <- (#peek cs_ci, nz) ptr
        return Cs{..}

    poke ptr Cs{..} = do
        (#poke cs_ci, nzmax) ptr nzmax
        (#poke cs_ci, m) ptr m
        (#poke cs_ci, n) ptr n
        (#poke cs_ci, p) ptr p
        (#poke cs_ci, i) ptr i
        (#poke cs_ci, x) ptr x
        (#poke cs_ci, nz) ptr nz

-- | Matrix in compressed sparse column (CSC) format.
data Matrix a = Matrix
    { nRows :: !Int
    , nColumns :: !Int
    , columnPointers :: !(Vector Int)
    , rowIndices :: !(Vector Int)
    , values :: !(Vector a)
    }
  deriving (Eq, Read, Show)

unsafeWithMatrix
  :: (Storable a, Storable (Cs a)) => Matrix a -> (Ptr (Cs a) -> IO b) -> IO b
unsafeWithMatrix Matrix{..} act = do
    let nzmax = fromIntegral $ V.length values
        m = fromIntegral $ nRows
        n = fromIntegral $ nColumns
        nz = -1
    V.unsafeWith (V.map fromIntegral columnPointers) $ \p ->
      V.unsafeWith (V.map fromIntegral rowIndices) $ \i ->
      V.unsafeWith values $ \x ->
      with Cs{..} act
{-# INLINE unsafeWithMatrix #-}

unsafeWithTriples
  :: (Storable a, Storable (Cs a))
  => Int -> Int -> Vector Int -> Vector Int -> Vector a
  -> (Ptr (Cs a) -> IO b) -> IO b
unsafeWithTriples (fromIntegral -> m) (fromIntegral -> n) colps_ rowixs_ vals_ act = do
    let nzmax = fromIntegral $ V.length vals_
        nz = fromIntegral $ V.length vals_
    V.unsafeWith (V.map fromIntegral colps_) $ \p ->
      V.unsafeWith (V.map fromIntegral rowixs_) $ \i ->
      V.unsafeWith vals_ $ \x ->
      with Cs{..} act
{-# INLINE unsafeWithTriples #-}

foreign import ccall "cs.h &cs_ci_free" cs_free :: FinalizerPtr a

fromCs :: (Storable a) => Cs a -> IO (Matrix a)
fromCs Cs{..} = do
    let nRows = fromIntegral m
        nColumns = fromIntegral n
        nzmax_ = fromIntegral nzmax
    unless (nz < 0) $ errorWithStackTrace
      $ "expected compressed matrix, got nz = " ++ show nz
    columnPointers_ <- newForeignPtr cs_free p
    rowIndices_ <- newForeignPtr cs_free i
    values_ <- newForeignPtr cs_free x
    let columnPointers =
          V.map fromIntegral
          $ V.unsafeFromForeignPtr0 columnPointers_ (nColumns + 1)
        rowIndices =
          V.map fromIntegral
          $ V.unsafeFromForeignPtr0 rowIndices_ nzmax_
        values = V.unsafeFromForeignPtr0 values_ nzmax_
    return Matrix{..}
{-# INLINE fromCs #-}
