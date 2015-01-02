{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Matrix.Sparse.Compress ( compress, deduplicate ) where

import Control.Monad (liftM)
import Data.Ord (comparing)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Algorithms.Intro as Intro
import Data.Vector.Algorithms.Search (binarySearchL)
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (MVector)
import qualified Data.Vector.Storable.Mutable as MV

import Data.Cs
import Data.Pairs
import Data.Matrix.Sparse.Type
import Data.Triples

compress
  :: (CxSparse a, Num a, PrimMonad m, Storable a)
  => Int  -- ^ number of rows
  -> Int  -- ^ number of columns
  -> MVector (PrimState m) CInt  -- ^ row indices
  -> MVector (PrimState m) CInt  -- ^ column indices
  -> MVector (PrimState m) a  -- ^ values
  -> m (Matrix a)
compress nr nc _rows _cols _vals = do
  let comparingCol (c, _, _) (c', _, _) = compare c c'
  Intro.sortBy comparingCol $ MTriples _cols _rows _vals
  colPtrs <- computePtrs nc _cols
  deduplicate nr nc colPtrs _rows _vals

computePtrs
  :: PrimMonad m => Int -> MVector (PrimState m) CInt -> m (Vector CInt)
computePtrs nc cols =
  V.generateM (nc + 1) $ \c ->
    liftM fromIntegral $ binarySearchL cols (fromIntegral c)

deduplicate
  :: (CxSparse a, PrimMonad m, Storable a)
  => Int
  -> Int
  -> Vector CInt  -- ^ column pointers
  -> MVector (PrimState m) CInt  -- ^ row indices
  -> MVector (PrimState m) a  -- ^ values
  -> m (Matrix a)
deduplicate nRows nColumns _cols _rows _vals = do
  let starts = V.init $ V.map fromIntegral _cols
      ends = V.tail $ V.map fromIntegral _cols
      lens = V.zipWith (-) ends starts
      pairs = MPairs _rows _vals
      dels_go ix len = dedupCol $ GMV.slice ix len pairs
  dels <- liftM (V.postscanl (+) 0) $ V.zipWithM dels_go starts lens

  let columnPointers :: Vector CInt
      columnPointers = V.zipWith (-) _cols (V.cons 0 dels)

  _rows <- V.unsafeFreeze _rows
  _vals <- V.unsafeFreeze _vals
  case GV.filter ((>= 0) . fst) (Pairs _rows _vals) of
   Pairs rowIndices values -> return Matrix{..}

dedupCol
  :: (Num a, PrimMonad m, Storable a)
  => MPairs MVector (PrimState m) (CInt, a) -> m CInt
dedupCol pairs@(MPairs rows vals) = do
  Intro.sortBy (comparing fst) pairs
  dedupCol_go 0 1 0
  where
    len = MV.length rows
    dedupCol_go !ixW !ixR !nDel
      | ixR < len = do
          rW <- MV.unsafeRead rows ixW
          rR <- MV.unsafeRead rows ixR
          if rW /= rR
            then dedupCol_go ixR (ixR + 1) nDel
            else do
              xR <- MV.unsafeRead vals ixR
              xW <- MV.unsafeRead vals ixW
              MV.unsafeWrite vals ixW $! xW + xR
              MV.unsafeWrite rows ixR (-1)
              dedupCol_go ixW (ixR + 1) (nDel + 1)
      | otherwise = return nDel
