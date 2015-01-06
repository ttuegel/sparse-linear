{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Matrix.Sparse.Foreign
       ( Storable
       , withConstMatrix
       , fromForeign
       ) where

import Data.Proxy
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Vector.Storable (Storable)
import qualified Data.Vector.Storable as S
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
  S.unsafeWith _ptrs $ \_ptrs ->
  S.unsafeWith _rows $ \_rows ->
  S.unsafeWith _vals $ \_vals ->
    action nr nc _ptrs _rows _vals
  where
    _ptrs = S.map fromIntegral $ S.convert pointers
    _rows = S.map fromIntegral $ S.convert $ fst $ V.unzip entries
    _vals = S.convert $ snd $ V.unzip entries
    nr = fromIntegral minDim
    nc = fromIntegral majDim

fromForeign
  :: (Num a, Storable a, Unbox a)
  => Bool -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr a -> IO (Matrix Col a)
{-# SPECIALIZE fromForeign :: Bool -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr Double -> IO (Matrix Col Double) #-}
{-# SPECIALIZE fromForeign :: Bool -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr (Complex Double) -> IO (Matrix Col (Complex Double)) #-}
fromForeign copy nRows nCols ptrs rows vals = do
  let (majDim, minDim) =
        orientSwap (Proxy :: Proxy Col) (fromIntegral nRows, fromIntegral nCols)

  _ptrs <- if copy
           then do _ptrs <- mallocArray (majDim + 1)
                   copyArray _ptrs ptrs (majDim + 1)
                   return _ptrs
           else return ptrs
  _ptrs <- newForeignPtr finalizerFree _ptrs

  let pointers = V.convert . S.map fromIntegral
                 $ S.unsafeFromForeignPtr0 _ptrs (majDim + 1)
      nz = V.last pointers

  _rows <- if copy
           then do _rows <- mallocArray nz
                   copyArray _rows rows nz
                   return _rows
           else return rows
  _rows <- newForeignPtr finalizerFree _rows
  _rows <- V.unsafeThaw
           $ V.convert . S.map fromIntegral
           $ S.unsafeFromForeignPtr0 _rows nz

  _vals <- if copy
           then do _vals <- mallocArray nz
                   copyArray _vals vals nz
                   return _vals
           else return vals
  _vals <- newForeignPtr finalizerFree _vals
  _vals <- V.unsafeThaw
           $ V.convert
           $ S.unsafeFromForeignPtr0 _vals nz

  let _entries = VM.zip _rows _vals

  V.forM_ (V.enumFromN 0 majDim) $ \m -> do
    start <- V.unsafeIndexM pointers m
    end <- V.unsafeIndexM pointers (m + 1)
    let len = end - start
    dedupInPlace minDim $ VM.slice start len _entries

  entries <- V.unsafeFreeze _entries
  return Matrix{..}
