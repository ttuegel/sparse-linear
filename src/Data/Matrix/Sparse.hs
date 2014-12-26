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

import Control.Applicative
import Control.Monad (unless)
import Data.Complex
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.C.Types
import Foreign.ForeignPtr.Safe (FinalizerPtr, newForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack
import System.IO.Unsafe (unsafePerformIO)

data Cs a = Cs
    { nzmax :: !CInt  -- ^ maximum number of entries
    , m :: !CInt  -- ^ number of rows
    , n :: !CInt  -- ^ number of columns
    , p :: !(Ptr CInt)  -- ^ column pointers or indices
    , i :: !(Ptr CInt)  -- ^ row indices
    , x :: !(Ptr a)  -- ^ values
    , nz :: !CInt  -- ^ number of entries (triplet) or (-1) for compressed col
    }

foreign import ccall "&sizeof_cs_ci" sizeof_cs_ci :: Ptr CInt
foreign import ccall "mk_cs_ci" mk_cs_ci :: Ptr (Cs (Complex Double)) -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr (Complex Double) -> CInt -> IO ()
foreign import ccall "match_cs_ci" match_cs_ci :: Ptr (Cs (Complex Double)) -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr (Ptr CInt) -> Ptr (Ptr CInt) -> Ptr (Ptr (Complex Double)) -> Ptr CInt -> IO ()

instance Storable (Cs (Complex Double)) where
  sizeOf _ = unsafePerformIO $ fromIntegral <$> peek sizeof_cs_ci
  {-# INLINE sizeOf #-}

  alignment _ = 8
  {-# INLINE alignment #-}

  peek ptr =
    alloca $ \nzmax_ ->
    alloca $ \m_ ->
    alloca $ \n_ ->
    alloca $ \p_ ->
    alloca $ \i_ ->
    alloca $ \x_ ->
    alloca $ \nz_ -> do
      match_cs_ci ptr nzmax_ m_ n_ p_ i_ x_ nz_
      nzmax <- peek nzmax_
      m <- peek m_
      n <- peek n_
      p <- peek p_
      i <- peek i_
      x <- peek x_
      nz <- peek nz_
      return Cs{..}
  {-# INLINE peek #-}

  poke ptr Cs{..} = do
    mk_cs_ci ptr nzmax m n p i x nz
  {-# INLINE poke #-}

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
