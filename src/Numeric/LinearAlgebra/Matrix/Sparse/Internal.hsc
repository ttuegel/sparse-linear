{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module Numeric.LinearAlgebra.Matrix.Sparse.Internal
    ( Cs(), fromCs, cs_free
    , Matrix(..), withMatrix, unsafeWithMatrix
    , withTriples, unsafeWithTriples
    ) where

import Control.Monad (unless)
import Data.Complex
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.ForeignPtr.Safe (FinalizerPtr, newForeignPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack

#include "cs.h"

data Cs a = Cs
    { nzmax :: Int  -- ^ maximum number of entries
    , m :: Int  -- ^ number of rows
    , n :: Int  -- ^ number of columns
    , p :: Ptr Int  -- ^ column pointers or indices
    , i :: Ptr Int  -- ^ row indices
    , x :: Ptr a  -- ^ values
    , nz :: Int  -- ^ number of entries (triplet) or (-1) for compressed col
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

data Matrix a = Matrix
    { nrows :: Int
    , ncols :: Int
    , colps :: Vector Int
    , rowixs :: Vector Int
    , vals :: Vector a
    }

withMatrix
  :: (Storable a, Storable (Cs a)) => Matrix a -> (Ptr (Cs a) -> IO b) -> IO b
withMatrix Matrix{..} act = do
    let nzmax = V.length vals
        m = nrows
        n = ncols
        nz = -1
    colps_ <- V.thaw colps
    rowixs_ <- V.thaw rowixs
    vals_ <- V.thaw vals
    MV.unsafeWith colps_ $ \p ->
      MV.unsafeWith rowixs_ $ \i ->
      MV.unsafeWith vals_ $ \x ->
      with Cs{..} act
{-# INLINE withMatrix #-}

unsafeWithMatrix
  :: (Storable a, Storable (Cs a)) => Matrix a -> (Ptr (Cs a) -> IO b) -> IO b
unsafeWithMatrix Matrix{..} act = do
    let nzmax = V.length vals
        m = nrows
        n = ncols
        nz = -1
    V.unsafeWith colps $ \p ->
      V.unsafeWith rowixs $ \i ->
      V.unsafeWith vals $ \x ->
      with Cs{..} act
{-# INLINE unsafeWithMatrix #-}

withTriples
  :: (Storable a, Storable (Cs a))
  => Int -> Int -> Vector Int -> Vector Int -> Vector a
  -> (Ptr (Cs a) -> IO b) -> IO b
withTriples m n colps_ rowixs_ vals_ act = do
    let nzmax = V.length vals_
        nz = V.length vals_
    p_ <- V.thaw colps_
    i_ <- V.thaw rowixs_
    x_ <- V.thaw vals_
    MV.unsafeWith p_ $ \p ->
      MV.unsafeWith i_ $ \i ->
      MV.unsafeWith x_ $ \x ->
      with Cs{..} act
{-# INLINE withTriples #-}

unsafeWithTriples
  :: (Storable a, Storable (Cs a))
  => Int -> Int -> Vector Int -> Vector Int -> Vector a
  -> (Ptr (Cs a) -> IO b) -> IO b
unsafeWithTriples m n colps_ rowixs_ vals_ act = do
    let nzmax = V.length vals_
        nz = V.length vals_
    V.unsafeWith colps_ $ \p ->
      V.unsafeWith rowixs_ $ \i ->
      V.unsafeWith vals_ $ \x ->
      with Cs{..} act
{-# INLINE unsafeWithTriples #-}

foreign import ccall "cs.h &cs_free" cs_free :: FinalizerPtr a

fromCs :: (Storable a) => Cs a -> IO (Matrix a)
fromCs Cs{..} = do
    let nrows = m
        ncols = n
    unless (nz < 0) $ errorWithStackTrace "expected compressed matrix"
    colps_ <- newForeignPtr cs_free p
    rowixs_ <- newForeignPtr cs_free i
    vals_ <- newForeignPtr cs_free x
    let colps = V.unsafeFromForeignPtr0 colps_ (n + 1)
        rowixs = V.unsafeFromForeignPtr0 rowixs_ nzmax
        vals = V.unsafeFromForeignPtr0 vals_ nzmax
    return Matrix{..}
{-# INLINE fromCs #-}
