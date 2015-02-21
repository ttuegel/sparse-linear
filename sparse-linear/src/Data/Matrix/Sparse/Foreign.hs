{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Matrix.Sparse.Foreign
       ( Storable
       , withConstMatrix
       , fromForeign
       ) where

import Data.Proxy
import Data.Vector.Unboxed (Unbox)
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
  => Matrix Col a
  -> (CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr a -> IO b)
  -> IO b
{-# SPECIALIZE withConstMatrix :: Matrix Col Double -> (CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO b) -> IO b #-}
{-# SPECIALIZE withConstMatrix :: Matrix Col (Complex Double) -> (CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr (Complex Double) -> IO b) -> IO b #-}
withConstMatrix Matrix{..} action =
  VS.unsafeWith _ptrs $ \_ptrs ->
  VS.unsafeWith _rows $ \_rows ->
  VS.unsafeWith _vals $ \_vals ->
    action nr nc _ptrs _rows _vals
  where
    _ptrs = VS.map fromIntegral $ VS.convert pointers
    _rows = VS.map fromIntegral $ VS.convert $ fst $ U.unzip entries
    _vals = VS.convert $ snd $ U.unzip entries
    nr = fromIntegral idim
    nc = fromIntegral odim

fromForeign
  :: (Num a, Storable a, Unbox a)
  => Bool -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr a -> IO (Matrix Col a)
{-# SPECIALIZE fromForeign :: Bool -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO (Matrix Col Double) #-}
{-# SPECIALIZE fromForeign :: Bool -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr (Complex Double) -> IO (Matrix Col (Complex Double)) #-}
fromForeign copy nRows nCols ptrs rows vals = do
  let (odim, idim) =
        orientSwap (Proxy :: Proxy Col) (fromIntegral nRows, fromIntegral nCols)

  _ptrs <- if copy
           then do _ptrs <- mallocArray (odim + 1)
                   copyArray _ptrs ptrs (odim + 1)
                   return _ptrs
           else return ptrs
  _ptrs <- newForeignPtr finalizerFree _ptrs

  let pointers = U.convert . VS.map fromIntegral
                 $ VS.unsafeFromForeignPtr0 _ptrs (odim + 1)
      nz = U.last pointers

  _rows <- if copy
           then do _rows <- mallocArray nz
                   copyArray _rows rows nz
                   return _rows
           else return rows
  _rows <- newForeignPtr finalizerFree _rows
  _rows <- U.unsafeThaw
           $ U.convert . VS.map fromIntegral
           $ VS.unsafeFromForeignPtr0 _rows nz

  _vals <- if copy
           then do _vals <- mallocArray nz
                   copyArray _vals vals nz
                   return _vals
           else return vals
  _vals <- newForeignPtr finalizerFree _vals
  _vals <- U.unsafeThaw
           $ U.convert
           $ VS.unsafeFromForeignPtr0 _vals nz

  let _entries = UM.zip _rows _vals

  U.forM_ (U.enumFromN 0 odim) $ \m -> do
    start <- U.unsafeIndexM pointers m
    end <- U.unsafeIndexM pointers (m + 1)
    let len = end - start
    dedupInPlace idim $ UM.unsafeSlice start len _entries

  entries <- U.unsafeFreeze _entries
  return Matrix{..}
