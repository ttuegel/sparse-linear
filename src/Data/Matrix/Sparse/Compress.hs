{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Matrix.Sparse.Compress
       ( compress, decompress
       , transpose, reorient
       ) where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Function (fix)
import Data.Vector.Algorithms.Search (binarySearchL)
import Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector.Unboxed as U

import Data.Cs
import Data.Complex.Enhanced
import Data.Matrix.Sparse.Type
import qualified Data.Vector.Sparse as S
import Data.Vector.Util

compress
  :: (Indices or, Num a, Storable a)
  => Int  -- ^ number of rows
  -> Int  -- ^ number of columns
  -> Vector CInt  -- ^ row indices
  -> Vector CInt  -- ^ column indices
  -> Vector a  -- ^ values
  -> Matrix or a
{-# SPECIALIZE
    compress
      :: Int -> Int
      -> Vector CInt -> Vector CInt
      -> Vector Double -> Matrix Row Double
  #-}
{-# SPECIALIZE
    compress
      :: Int -> Int
      -> Vector CInt -> Vector CInt
      -> Vector (Complex Double) -> Matrix Row (Complex Double)
  #-}
{-# SPECIALIZE
    compress
      :: Int -> Int
      -> Vector CInt -> Vector CInt
      -> Vector Double -> Matrix Col Double
  #-}
{-# SPECIALIZE
    compress
      :: Int -> Int
      -> Vector CInt -> Vector CInt
      -> Vector (Complex Double) -> Matrix Col (Complex Double)
  #-}
compress nRows nColumns rows cols vals = fix $ \mat -> runST $ do
  let nz = V.length vals
      dimM = ixsM (orient mat) nRows nColumns
      dimN = ixsN (orient mat) nRows nColumns
      mixs = ixsM (orient mat) rows cols
      nixs = ixsN (orient mat) rows cols
      ptrs = computePtrs dimM mixs

  ixs <- MV.replicate nz $ fromIntegral dimN
  xs <- MV.new nz
  dels <- MV.replicate dimM 0

  let dedupInsert !ixN (fromIntegral -> !ixM) !x = do
        start <- fromIntegral <$> V.unsafeIndexM ptrs ixM
        end <- fromIntegral <$> V.unsafeIndexM ptrs (ixM + 1)
        let len = end - start
            ixs' = MV.slice start len ixs
            xs' = MV.slice start len xs
        ix <- binarySearchL ixs' ixN
        ixN' <- MV.unsafeRead ixs' ix
        if ixN == ixN'
          then do
            _ <- preincrement dels ixM
            x' <- MV.unsafeRead xs' ix
            MV.unsafeWrite xs' ix $! x + x'
          else do
            shiftR ixs' ix 1
            MV.unsafeWrite ixs' ix ixN
            shiftR xs' ix 1
            MV.unsafeWrite xs' ix x
  zipWithM3_ dedupInsert nixs mixs vals
  shifts <- V.scanl' (+) 0 <$> V.unsafeFreeze dels

  let pointers = V.zipWith (-) ptrs shifts
      nz' = fromIntegral $ V.last pointers

  U.forM_ (U.enumFromN 0 dimM) $ \ixM -> do
    shift <- fromIntegral <$> V.unsafeIndexM shifts ixM
    when (shift > 0) $ do
      start <- fromIntegral <$> V.unsafeIndexM ptrs ixM
      end <- fromIntegral <$> V.unsafeIndexM ptrs (ixM + 1)
      let len = end - start
          start' = start - shift
      MV.move (MV.slice start' len ixs) (MV.slice start len ixs)
      MV.move (MV.slice start' len xs) (MV.slice start len xs)

  indices <- V.unsafeFreeze $ MV.slice 0 nz' ixs
  values <- V.unsafeFreeze $ MV.slice 0 nz' xs

  return Matrix{..}

computePtrs :: Int -> Vector CInt -> Vector CInt
{-# INLINE computePtrs #-}
computePtrs n indices = runST $ do
  counts <- MV.replicate n 0
  -- scan the indices once, counting the occurrences of each index
  V.forM_ indices $ \(fromIntegral -> ix) -> do
    count <- MV.unsafeRead counts ix
    MV.unsafeWrite counts ix $! count + 1
  -- compute the index pointers by prefix-summing the occurrence counts
  V.scanl (+) 0 <$> V.unsafeFreeze counts

decompress :: Vector CInt -> Vector CInt
{-# INLINE decompress #-}
decompress = \ptrs -> V.create $ do
  indices <- MV.new $ fromIntegral $ V.last ptrs
  U.forM_ (U.enumFromN 0 $ V.length ptrs - 1) $ \c -> do
    start <- fromIntegral <$> V.unsafeIndexM ptrs c
    end <- fromIntegral <$> V.unsafeIndexM ptrs (c + 1)
    MV.set (MV.slice start (end - start) indices) $ fromIntegral c
  return indices

transpose :: Matrix or a -> Matrix (Trans or) a
{-# INLINE transpose #-}
transpose = \Matrix{..} -> Matrix {..}

reorient :: Storable a => Matrix (Trans or) a -> Matrix or a
{-# INLINE reorient #-}
reorient mat@Matrix{..} = runST $ do
  let ptrs' = computePtrs dimN indices
  -- re-initialize row counts from row pointers
  count <- V.thaw $ V.map fromIntegral $ V.slice 0 dimN ptrs'

  let nz = V.length values
  _ixs <- MV.new nz
  _xs <- MV.new nz

  -- copy each column into place
  -- "major" and "minor" indices refer to the orientation of the original matrix
  U.forM_ (U.enumFromN 0 dimM) $ \ !ixM -> do
    S.iforM_ (slice mat ixM) $ \(fromIntegral -> !ixN) !x -> do
      ix <- preincrement count ixN
      MV.unsafeWrite _ixs ix $ fromIntegral ixM
      MV.unsafeWrite _xs ix x

  _ixs <- V.unsafeFreeze _ixs
  _xs <- V.unsafeFreeze _xs

  return Matrix
    { dimM = dimN
    , dimN = dimM
    , pointers = ptrs'
    , indices = _ixs
    , values = _xs
    }
