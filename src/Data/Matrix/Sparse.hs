{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Matrix.Sparse
    ( Matrix(..), withConstCs, fromCs, withConstTriples, cmap
    , compress
    , toColumns, assertValid
    , module Data.Cs
    ) where

import Control.Applicative
import Control.Monad (liftM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.MonoTraversable
import Data.Ord (comparing)
import Data.Pairs
import Data.Triples
import qualified Data.Vector.Algorithms.Intro as Intro
import Data.Vector.Algorithms.Search (binarySearchL)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (MVector)
import qualified Data.Vector.Storable.Mutable as MV
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
      dels_go ix len = dedupCol $ MG.slice ix len pairs
  dels <- liftM (V.postscanl (+) 0) $ V.zipWithM dels_go starts lens

  let columnPointers :: Vector CInt
      columnPointers = V.zipWith (-) _cols (V.cons 0 dels)

  _rows <- V.unsafeFreeze _rows
  _vals <- V.unsafeFreeze _vals
  case G.filter ((>= 0) . fst) (Pairs _rows _vals) of
   Pairs rowIndices values -> return Matrix{..}

fromCs :: CxSparse a => Bool -> Ptr (Cs a) -> IO (Matrix a)
fromCs doDedup _ptr
  | _ptr == nullPtr = errorWithStackTrace "fromCs: null pointer"
  | otherwise = do
      Cs{..} <- peek _ptr
      let nr = fromIntegral m
          nc = fromIntegral n
          nzmax_ = fromIntegral nzmax
          mkVector ptr len =
            MV.unsafeFromForeignPtr0
            <$> newForeignPtr finalizerFree ptr
            <*> pure len
      _rows <- mkVector i nzmax_
      _vals <- mkVector x nzmax_
      mat <- if nz < 0
                then do
                  cols <- V.unsafeFromForeignPtr0
                          <$> newForeignPtr finalizerFree p
                          <*> pure (nc + 1)
                  if doDedup
                    then deduplicate nr nc cols _rows _vals
                    else do
                      _rows <- V.unsafeFreeze _rows
                      _vals <- V.unsafeFreeze _vals
                      return Matrix
                        { nRows = nr
                        , nColumns = nc
                        , columnPointers = cols
                        , rowIndices = _rows
                        , values = _vals
                        }
             else do
                  cols <- mkVector p nzmax_
                  compress nr nc _rows cols _vals
      free _ptr
      return mat

toColumns :: Storable a => Matrix a -> [SpV.Vector a]
toColumns = \Matrix{..} ->
  let starts = map fromIntegral $ V.toList $ V.init columnPointers
      ends = map fromIntegral $ V.toList $ V.tail columnPointers
      lens = zipWith (-) ends starts
      chop :: Storable b => Vector b -> [Vector b]
      chop v = zipWith chop_go starts lens
        where
          chop_go n len
            | len > 0 = V.slice n len v
            | otherwise = V.empty
  in do
    (inds, vals) <- zip (chop rowIndices) (chop values)
    return SpV.Vector
      { SpV.dim = nRows
      , SpV.indices = inds
      , SpV.values = vals
      }

nondecreasing :: (Ord a, Storable a) => Vector a -> Bool
nondecreasing vec
  | V.null vec = True
  | otherwise = V.and $ V.zipWith (<=) (V.init vec) (V.tail vec)

increasing :: (Ord a, Storable a) => Vector a -> Bool
increasing vec
  | V.null vec = True
  | otherwise = V.and $ V.zipWith (<) (V.init vec) (V.tail vec)

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
