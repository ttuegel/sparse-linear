{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Matrix.Sparse
    ( Matrix(..), withConstCs, fromCs, withConstTriples, cmap
    , CxSparse(..), RealOf, ComplexOf
    ) where

import Control.Applicative
import Data.MonoTraversable
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.C.Types
import Foreign.ForeignPtr.Safe (newForeignPtr)
import Foreign.Marshal.Alloc (free, finalizerFree)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable

import Data.Complex.Enhanced
import Data.Cs

-- | Matrix in compressed sparse column (CSC) format.
data Matrix a = Matrix
    { nRows :: !Int
    , nColumns :: !Int
    , columnPointers :: !(Vector CInt)
    , rowIndices :: !(Vector CInt)
    , values :: !(Vector a)
    }
  deriving (Eq, Read, Show)

type instance Element (Matrix a) = a

instance Storable a => MonoFunctor (Matrix a) where
  omap = \f mat -> mat { values = omap f $ values mat }
  {-# INLINE omap #-}

instance Storable a => MonoFoldable (Matrix a) where
  ofoldMap = \f Matrix{..} -> ofoldMap f values
  {-# INLINE ofoldMap #-}

  ofoldr = \f r Matrix{..} -> ofoldr f r values
  {-# INLINE ofoldr #-}

  ofoldl' = \f r Matrix{..} -> ofoldl' f r values
  {-# INLINE ofoldl' #-}

  ofoldr1Ex = \f Matrix{..} -> ofoldr1Ex f values
  {-# INLINE ofoldr1Ex #-}

  ofoldl1Ex' = \f Matrix{..} -> ofoldl1Ex' f values
  {-# INLINE ofoldl1Ex' #-}

cmap :: (Storable a, Storable b) => (a -> b) -> Matrix a -> Matrix b
{-# INLINE cmap #-}
cmap = \f m -> m { values = V.map f $ values m }

type CsGaxpy a = Ptr (Cs a) -> Ptr a -> Ptr a -> IO Int
type CsCompress a = Ptr (Cs a) -> IO (Ptr (Cs a))
type CsTranspose a = Ptr (Cs a) -> Int -> IO (Ptr (Cs a))
type CsMultiply a = Ptr (Cs a) -> Ptr (Cs a) -> IO (Ptr (Cs a))
type CsAdd a = Ptr (Cs a) -> Ptr (Cs a) -> Ptr a -> Ptr a -> IO (Ptr (Cs a))
type CsDiag a = Ptr (Cs a) -> IO (Ptr a)

class (Storable a, Storable (Cs a)) => CxSparse a where
    cs_gaxpy :: CsGaxpy a
    cs_compress :: CsCompress a
    cs_transpose :: CsTranspose a
    cs_multiply :: CsMultiply a
    cs_add :: CsAdd a
    cs_kron :: CsMultiply a
    cs_diag :: CsDiag a

foreign import ccall "cs.h cs_ci_gaxpy"
  cs_ci_gaxpy :: CsGaxpy (Complex Double)
foreign import ccall "cs.h cs_ci_compress"
  cs_ci_compress :: CsCompress (Complex Double)
foreign import ccall "cs.h cs_ci_transpose"
  cs_ci_transpose :: CsTranspose (Complex Double)
foreign import ccall "cs.h cs_ci_multiply"
  cs_ci_multiply :: CsMultiply (Complex Double)
foreign import ccall "cs_ci_add_ptrs"
  cs_ci_add :: CsAdd (Complex Double)
foreign import ccall "cs_ci_kron"
  cs_ci_kron :: CsMultiply (Complex Double)
foreign import ccall "cs_ci_diag"
  cs_ci_diag :: CsDiag (Complex Double)

instance CxSparse (Complex Double) where
    {-# INLINE cs_gaxpy #-}
    {-# INLINE cs_compress #-}
    {-# INLINE cs_transpose #-}
    {-# INLINE cs_multiply #-}
    {-# INLINE cs_add #-}
    {-# INLINE cs_kron #-}
    {-# INLINE cs_diag #-}
    cs_gaxpy = cs_ci_gaxpy
    cs_compress = cs_ci_compress
    cs_transpose = cs_ci_transpose
    cs_multiply = cs_ci_multiply
    cs_add = cs_ci_add
    cs_kron = cs_ci_kron
    cs_diag = cs_ci_diag

foreign import ccall "cs.h cs_di_gaxpy"
  cs_di_gaxpy :: CsGaxpy Double
foreign import ccall "cs.h cs_di_compress"
  cs_di_compress :: CsCompress Double
foreign import ccall "cs.h cs_di_transpose"
  cs_di_transpose :: CsTranspose Double
foreign import ccall "cs.h cs_di_multiply"
  cs_di_multiply :: CsMultiply Double
foreign import ccall "cs_di_add_ptrs"
  cs_di_add :: CsAdd Double
foreign import ccall "cs_di_kron"
  cs_di_kron :: CsMultiply Double
foreign import ccall "cs_di_diag"
  cs_di_diag :: CsDiag Double

instance CxSparse Double where
    {-# INLINE cs_gaxpy #-}
    {-# INLINE cs_compress #-}
    {-# INLINE cs_transpose #-}
    {-# INLINE cs_multiply #-}
    {-# INLINE cs_add #-}
    {-# INLINE cs_kron #-}
    {-# INLINE cs_diag #-}
    cs_gaxpy = cs_di_gaxpy
    cs_compress = cs_di_compress
    cs_transpose = cs_di_transpose
    cs_multiply = cs_di_multiply
    cs_add = cs_di_add
    cs_kron = cs_di_kron
    cs_diag = cs_di_diag

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
{-# INLINE withConstCs #-}

withConstTriples
  :: CxSparse a
  => Int -> Int -> Vector Int -> Vector Int -> Vector a
  -> (Ptr (Cs a) -> IO b) -> IO b
withConstTriples (fromIntegral -> m) (fromIntegral -> n) colps_ rowixs_ vals_ act = do
    let nzmax = fromIntegral $ V.length vals_
        nz = fromIntegral $ V.length vals_
    V.unsafeWith (V.map fromIntegral colps_) $ \p ->
      V.unsafeWith (V.map fromIntegral rowixs_) $ \i ->
      V.unsafeWith vals_ $ \x ->
      with Cs{..} act
{-# INLINE withConstTriples #-}

fromCs :: CxSparse a => Ptr (Cs a) -> IO (Matrix a)
fromCs _ptr = do
  cs <- peek _ptr
  _ptr <- if nz cs < 0
             then return _ptr
          else do
            ptr' <- cs_compress _ptr
            free (p cs) >> free (i cs) >> free (x cs) >> free _ptr
            return ptr'
  Cs{..} <- peek _ptr
  let nRows = fromIntegral m
      nColumns = fromIntegral n
      nzmax_ = fromIntegral nzmax
  columnPointers <-
    V.unsafeFromForeignPtr0
    <$> newForeignPtr finalizerFree p
    <*> pure (nColumns + 1)
  rowIndices <-
    V.unsafeFromForeignPtr0
    <$> newForeignPtr finalizerFree i
    <*> pure nzmax_
  values <-
    V.unsafeFromForeignPtr0
    <$> newForeignPtr finalizerFree x
    <*> pure nzmax_
  free _ptr
  return Matrix{..}
{-# INLINE fromCs #-}
