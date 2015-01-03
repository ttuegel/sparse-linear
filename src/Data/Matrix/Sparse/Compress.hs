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
import Control.Monad.ST (runST)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Algorithms.Search (binarySearchL)
import Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (MVector)
import qualified Data.Vector.Storable.Mutable as MV

import Data.Cs
import Data.Matrix.Sparse.Type

zipWithM3_
  :: (Monad m, Storable a, Storable b, Storable c)
  => (a -> b -> c -> m d) -> Vector a -> Vector b -> Vector c -> m ()
{-# INLINE zipWithM3_ #-}
zipWithM3_ f as bs cs = do
  let len = minimum [V.length as, V.length bs, V.length cs]
  V.forM_ (V.enumFromN 0 len) $ \ix -> do
    a <- V.unsafeIndexM as ix
    b <- V.unsafeIndexM bs ix
    c <- V.unsafeIndexM cs ix
    f a b c

shiftR :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> m ()
shiftR = \v ix -> do
  let len' = MV.length v - ix - 1
      src = MV.slice ix len' v
      dst = MV.slice (ix + 1) len' v
  MV.move dst src

shiftL :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> m ()
shiftL = \v ix -> do
  let len' = MV.length v - ix - 1
      dst = MV.slice ix len' v
      src = MV.slice (ix + 1) len' v
  MV.move dst src

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
      _ptrs = computePtrs nColumns cols

  rows' <- MV.replicate nz $ fromIntegral nRows
  vals' <- MV.new nz
  dels <- MV.replicate nColumns 0

  -- TODO: deduplication counts!
  let dedupInsert r (fromIntegral -> c) x = do
        start <- fromIntegral <$> V.unsafeIndexM _ptrs c
        end <- fromIntegral <$> V.unsafeIndexM _ptrs (c + 1)
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
            shiftR rs ix
            MV.unsafeWrite rs ix r
            shiftR xs ix
            MV.unsafeWrite xs ix x
  zipWithM3_ dedupInsert rows cols vals
  _ptrs <- V.zipWith (-) _ptrs . V.scanl (+) 0 <$> V.unsafeFreeze dels

  let columnPointers = _ptrs
      nz' = fromIntegral $ V.last columnPointers

  let dedupFilter !ix
        | ix < nz' = do
            r <- MV.unsafeRead rows' ix
            if fromIntegral r < nRows then dedupFilter (ix + 1) else do
              shiftL rows' ix
              shiftL vals' ix
              dedupFilter ix
        | otherwise = return ()
  dedupFilter 0

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
  V.forM_ (V.enumFromN 0 $ V.length ptrs - 1) $ \c -> do
    start <- fromIntegral <$> V.unsafeIndexM ptrs c
    end <- fromIntegral <$> V.unsafeIndexM ptrs (c + 1)
    MV.set (MV.slice start (end - start) indices) $ fromIntegral c
  return indices

transpose :: (Num a, Storable a) => Matrix a -> Matrix a
transpose Matrix{..} = runST $ do
  let rowPointers = computePtrs nRows rowIndices
  -- re-initialize row counts from row pointers
  rowCount <- V.thaw $ V.slice 0 nRows rowPointers

  let nz = V.length values
  cols <- MV.new nz
  vals <- MV.new nz

  let insertIntoRow r c x = do
        ix <- fromIntegral <$> preincrement rowCount (fromIntegral r)
        MV.unsafeWrite cols ix c
        MV.unsafeWrite vals ix x

  -- copy each column into place
  V.forM_ (V.enumFromN 0 nColumns) $ \c -> do
    start <- fromIntegral <$> V.unsafeIndexM columnPointers c
    end <- fromIntegral <$> V.unsafeIndexM columnPointers (c + 1)
    let rs = V.slice start (end - start) rowIndices
        xs = V.slice start (end - start) values
    V.zipWithM_ (\r x -> insertIntoRow r c x) rs xs

  _values <- V.unsafeFreeze vals
  _colIndices <- V.map fromIntegral <$> V.unsafeFreeze cols

  return Matrix
    { nRows = nColumns
    , nColumns = nRows
    , columnPointers = rowPointers
    , rowIndices = _colIndices
    , values = _values
    }

preincrement
  :: (Num a, PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> m a
{-# INLINE preincrement #-}
preincrement = \v ix -> do
  count <- MV.unsafeRead v ix
  MV.unsafeWrite v ix $! count + 1
  return count
