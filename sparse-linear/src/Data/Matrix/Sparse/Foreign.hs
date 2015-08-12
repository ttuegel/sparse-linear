{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Matrix.Sparse.Foreign
       ( Storable
       , withConstMatrix
       , fromForeign
       ) where

import qualified Data.Vector.Unboxed as U
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
  :: (Storable a, Unbox a)
  => Matrix a
  -> (CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr a -> IO b)
  -> IO b
{-# SPECIALIZE withConstMatrix :: Matrix Double -> (CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO b) -> IO b #-}
{-# SPECIALIZE withConstMatrix :: Matrix (Complex Double) -> (CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr (Complex Double) -> IO b) -> IO b #-}
withConstMatrix Matrix {..} action =
  VS.unsafeWith _ptrs $ \_ptrs ->
  VS.unsafeWith _rows $ \_rows ->
  VS.unsafeWith _vals $ \_vals ->
    action (fromIntegral nrows) (fromIntegral ncols) _ptrs _rows _vals
  where
    _ptrs = VS.map fromIntegral $ VS.convert pointers
    _rows = VS.map fromIntegral $ VS.convert $ fst $ U.unzip entries
    _vals = VS.convert $ snd $ U.unzip entries

fromForeign
  :: (Num a, Storable a, Unbox a)
  => Bool -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr a -> IO (Matrix a)
{-# SPECIALIZE fromForeign :: Bool -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO (Matrix Double) #-}
{-# SPECIALIZE fromForeign :: Bool -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr (Complex Double) -> IO (Matrix (Complex Double)) #-}
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
          = (U.convert . VS.map fromIntegral)
            (VS.unsafeFromForeignPtr0 _ptrs nptrs)

    let nz = U.last pointers
    _rows <- toForeignPtr rows nz
    _rows <- (U.unsafeThaw . U.convert . VS.map fromIntegral)
             (VS.unsafeFromForeignPtr0 _rows nz)

    _vals <- toForeignPtr vals nz
    _vals <- (U.unsafeThaw . U.convert)
             (VS.unsafeFromForeignPtr0 _vals nz)

    let _entries = UM.zip _rows _vals

    U.forM_ (U.enumFromN 0 ncols) $ \m -> do
      start <- U.unsafeIndexM pointers m
      end <- U.unsafeIndexM pointers (m + 1)
      let len = end - start
      dedupInPlace nrows (UM.unsafeSlice start len _entries)

    entries <- U.unsafeFreeze _entries
    return Matrix {..}
