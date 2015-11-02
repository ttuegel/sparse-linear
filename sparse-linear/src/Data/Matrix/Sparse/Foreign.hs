{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Matrix.Sparse.Foreign
       ( Storable
       , withConstMatrix
       , fromForeign
       ) where

import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Vector.Storable (Storable)
import qualified Data.Vector.Storable as VS
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree)
import Foreign.Marshal.Array (copyArray, mallocArray)
import Foreign.Ptr (Ptr)

import Data.Matrix.Sparse

withConstMatrix
  :: (Storable a, Unbox a, Vector v a)
  => Matrix v a
  -> (CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr a -> IO b)
  -> IO b
{-# SPECIALIZE withConstMatrix :: Matrix VU.Vector Double -> (CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO b) -> IO b #-}
{-# SPECIALIZE withConstMatrix :: Matrix VU.Vector (Complex Double) -> (CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr (Complex Double) -> IO b) -> IO b #-}
{-# SPECIALIZE withConstMatrix :: Matrix VS.Vector Double -> (CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO b) -> IO b #-}
{-# SPECIALIZE withConstMatrix :: Matrix VS.Vector (Complex Double) -> (CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr (Complex Double) -> IO b) -> IO b #-}
withConstMatrix Matrix {..} action =
  VS.unsafeWith _ptrs $ \_ptrs ->
  VS.unsafeWith _rows $ \_rows ->
  VS.unsafeWith _vals $ \_vals ->
    action (fromIntegral nrows) (fromIntegral ncols) _ptrs _rows _vals
  where
    _ptrs = VS.map fromIntegral $ VS.convert pointers
    _rows = VS.map fromIntegral $ VS.convert indices
    _vals = VS.convert values

fromForeign
  :: (Num a, Storable a, Unbox a, Vector v a)
  => Bool -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr a -> IO (Matrix v a)
{-# SPECIALIZE fromForeign :: Bool -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO (Matrix VU.Vector Double) #-}
{-# SPECIALIZE fromForeign :: Bool -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr (Complex Double) -> IO (Matrix VU.Vector (Complex Double)) #-}
{-# SPECIALIZE fromForeign :: Bool -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO (Matrix VS.Vector Double) #-}
{-# SPECIALIZE fromForeign :: Bool -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr (Complex Double) -> IO (Matrix VS.Vector (Complex Double)) #-}
fromForeign copy (fromIntegral -> nrows) (fromIntegral -> ncols) ptrs rows vals
  = do
    let maybeCopyArray src len
          | copy = do
              dst <- mallocArray len
              copyArray dst src len
              return dst
          | otherwise = return src
        toForeignPtr src len
          = maybeCopyArray src len >>= newForeignPtr finalizerFree

    let nptrs = ncols + 1
    _ptrs <- toForeignPtr ptrs nptrs
    let pointers
          = (VU.convert . VS.map fromIntegral)
            (VS.unsafeFromForeignPtr0 _ptrs nptrs)

    let nz = VU.last pointers
    _rows <- toForeignPtr rows nz
    _rows <- (VU.unsafeThaw . VU.convert . VS.map fromIntegral)
             (VS.unsafeFromForeignPtr0 _rows nz)

    _vals <- toForeignPtr vals nz
    _vals <- (VU.unsafeThaw . VU.convert)
             (VS.unsafeFromForeignPtr0 _vals nz)

    let _entries = UM.zip _rows _vals

    VU.forM_ (VU.enumFromN 0 ncols) $ \m -> do
      start <- VU.unsafeIndexM pointers m
      end <- VU.unsafeIndexM pointers (m + 1)
      let len = end - start
      dedupInPlace nrows (UM.unsafeSlice start len _entries)

    entries <- VU.unsafeFreeze _entries
    let (indices, VG.convert -> values) = VU.unzip entries
    return Matrix {..}
