{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Matrix.Sparse.Compress
       ( compress, decompress
       , transpose
       ) where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Vector.Algorithms.Search (binarySearchL)
import Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector.Unboxed as U

import Data.Cs
import Data.Matrix.Sparse.Type
import qualified Data.Vector.Sparse as S
import Data.Vector.Util

compress
  :: (Num a, Storable a)
  => Int  -- ^ number of rows
  -> Int  -- ^ number of columns
  -> Vector CInt  -- ^ row indices
  -> Vector CInt  -- ^ column indices
  -> Vector a  -- ^ values
  -> Matrix a
compress nRows nColumns rows cols vals = runST $ do
  let nz = V.length vals
      ptrs = computePtrs nColumns cols

  rows' <- MV.replicate nz $ fromIntegral nRows
  vals' <- MV.new nz
  dels <- MV.replicate nColumns 0

  let dedupInsert !r (fromIntegral -> !c) !x = do
        start <- fromIntegral <$> V.unsafeIndexM ptrs c
        end <- fromIntegral <$> V.unsafeIndexM ptrs (c + 1)
        let len = end - start
            rs = MV.slice start len rows'
            xs = MV.slice start len vals'
        ix <- binarySearchL rs r
        r' <- MV.unsafeRead rs ix
        if r == r'
          then do
            _ <- preincrement dels c
            x' <- MV.unsafeRead xs ix
            MV.unsafeWrite xs ix $! x + x'
          else do
            shiftR rs ix 1
            MV.unsafeWrite rs ix r
            shiftR xs ix 1
            MV.unsafeWrite xs ix x
  zipWithM3_ dedupInsert rows cols vals
  shifts <- V.scanl' (+) 0 <$> V.unsafeFreeze dels

  let columnPointers = V.zipWith (-) ptrs shifts
      nz' = fromIntegral $ V.last columnPointers

  U.forM_ (U.enumFromN 0 nColumns) $ \c -> do
    shift <- fromIntegral <$> V.unsafeIndexM shifts c
    when (shift > 0) $ do
      start <- fromIntegral <$> V.unsafeIndexM ptrs c
      end <- fromIntegral <$> V.unsafeIndexM ptrs (c + 1)
      let len = end - start
          start' = start - shift
      MV.move (MV.slice start' len rows') (MV.slice start len rows')
      MV.move (MV.slice start' len vals') (MV.slice start len vals')

  rowIndices <- V.unsafeFreeze $ MV.slice 0 nz' rows'
  values <- V.unsafeFreeze $ MV.slice 0 nz' vals'

  return Matrix{..}

computePtrs :: Int -> Vector CInt -> Vector CInt
computePtrs n indices = runST $ do
  counts <- MV.replicate n 0
  -- scan the indices once, counting the occurrences of each index
  V.forM_ indices $ \(fromIntegral -> ix) -> do
    count <- MV.unsafeRead counts ix
    MV.unsafeWrite counts ix $! count + 1
  -- compute the index pointers by prefix-summing the occurrence counts
  V.scanl (+) 0 <$> V.unsafeFreeze counts

decompress :: Vector CInt -> Vector CInt
decompress = \ptrs -> V.create $ do
  indices <- MV.new $ fromIntegral $ V.last ptrs
  U.forM_ (U.enumFromN 0 $ V.length ptrs - 1) $ \c -> do
    start <- fromIntegral <$> V.unsafeIndexM ptrs c
    end <- fromIntegral <$> V.unsafeIndexM ptrs (c + 1)
    MV.set (MV.slice start (end - start) indices) $ fromIntegral c
  return indices

transpose :: (Num a, Storable a) => Matrix a -> Matrix a
transpose mat@Matrix{..} = runST $ do
  let rowPointers = computePtrs nRows rowIndices
  -- re-initialize row counts from row pointers
  rowCount <- V.thaw $ V.slice 0 nRows rowPointers

  let nz = V.length values
  cols <- MV.new nz
  vals <- MV.new nz

  -- copy each column into place
  let insertIntoRow (fromIntegral -> !c) !r !x = do
        ix <- fromIntegral <$> preincrement rowCount (fromIntegral r)
        MV.unsafeWrite cols ix c
        MV.unsafeWrite vals ix x

  U.forM_ (U.enumFromN 0 nColumns) $ \c -> do
    let col = column mat c
    V.zipWithM_ (insertIntoRow c) (S.indices col) (S.values col)

  _values <- V.unsafeFreeze vals
  _colIndices <- V.unsafeFreeze cols

  return Matrix
    { nRows = nColumns
    , nColumns = nRows
    , columnPointers = rowPointers
    , rowIndices = _colIndices
    , values = _values
    }
