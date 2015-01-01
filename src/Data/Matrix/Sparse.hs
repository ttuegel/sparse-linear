{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Matrix.Sparse
    ( Matrix(..), withConstCs, fromCs, withConstTriples, cmap
    , toColumns, assertValid
    , module Data.Cs
    ) where

import Control.Applicative
import Control.Monad (unless)
import Data.MonoTraversable
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.C.Types
import Foreign.ForeignPtr.Safe (newForeignPtr)
import Foreign.Marshal.Alloc (free, finalizerFree)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack

import Data.Cs
import qualified Data.Vector.Sparse as SpV

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

fromCs :: CxSparse a => Ptr (Cs a) -> IO (Matrix a)
fromCs _ptr
  | _ptr == nullPtr = errorWithStackTrace "fromCs: null pointer"
  | otherwise = do
      _ptr <- do
        ptr' <- cs_compress _ptr
        if ptr' == nullPtr
           then return _ptr
          else do
            cs <- peek _ptr
            free (p cs) >> free (i cs) >> free (x cs) >> free _ptr
            return ptr'
      duplStatus <- cs_dupl _ptr
      unless (duplStatus > 0) $ errorWithStackTrace "fromCs: deduplication failed"
      Cs{..} <- peek _ptr
      let nRows = fromIntegral m
          nColumns = fromIntegral n
          nzmax_ = fromIntegral nzmax
      columnPointers <- mkVector p (nColumns + 1)
      rowIndices <- mkVector i nzmax_
      values <- mkVector x nzmax_
      free _ptr
      return Matrix{..}
  where
    mkVector ptr len =
      V.unsafeFromForeignPtr0 <$> newForeignPtr finalizerFree ptr <*> pure len

toColumns :: Storable a => Matrix a -> [SpV.Vector a]
toColumns = \Matrix{..} ->
  let starts = map fromIntegral $ V.toList $ V.init columnPointers
      ends = map fromIntegral $ V.toList $ V.tail columnPointers
      lens = zipWith (-) ends starts
      chop :: Storable b => Vector b -> [Vector b]
      chop v = zipWith (\n len -> V.slice n len v) starts lens
  in do
    (inds, vals) <- zip (chop rowIndices) (chop values)
    return SpV.Vector
      { SpV.dim = nRows
      , SpV.indices = inds
      , SpV.values = vals
      }

nondecreasing :: (Ord a, Storable a) => Vector a -> Bool
nondecreasing vec = V.and $ V.zipWith (<=) (V.init vec) (V.tail vec)

increasing :: (Ord a, Storable a) => Vector a -> Bool
increasing vec = V.and $ V.zipWith (<) (V.init vec) (V.tail vec)

assertValid :: Storable a => Matrix a -> Matrix a
assertValid mat@Matrix{..}
  | not (nondecreasing columnPointers) =
      errorWithStackTrace "column pointers are not nondecreasing"
  | V.length columnPointers /= nColumns + 1 =
      errorWithStackTrace "wrong number of column pointers"
  | V.length values /= (fromIntegral $ V.last columnPointers) =
      errorWithStackTrace "length values is wrong"
  | any (not . increasing . SpV.indices) (toColumns mat) =
      errorWithStackTrace "row indices are not increasing"
  | otherwise = mat
