{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}

module Data.Vector.Sparse
       ( Vector(..), cmap
       , fromPairs, (|>)
       , unsafeLinInto
       ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import Data.MonoTraversable
import Data.Ord (comparing)
import qualified Data.Vector.Algorithms.Intro as Intro
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MV

data Vector a = Vector
  { dim :: !Int
  , entries :: !(V.Vector (Int, a))
  }
  deriving (Eq, Show)

fromPairs :: Unbox a => Int -> V.Vector (Int, a) -> Vector a
{-# INLINE fromPairs #-}
fromPairs = \dim _pairs -> runST $ do
  _pairs <- V.thaw _pairs
  Intro.sortBy (comparing fst) _pairs
  entries <- V.unsafeFreeze _pairs
  return Vector{..}

(|>) :: Unbox a => Int -> [(Int, a)] -> Vector a
{-# INLINE (|>) #-}
(|>) = \dim pairs -> fromPairs dim $ V.fromList pairs

type instance Element (Vector a) = a

instance Unbox a => MonoFunctor (Vector a) where
  {-# INLINE omap #-}
  omap = \f v ->
    let (indices, values) = V.unzip $ entries v
    in v { entries = V.zip indices $ omap f values }

instance Unbox a => MonoFoldable (Vector a) where
  {-# INLINE ofoldMap #-}
  {-# INLINE ofoldr #-}
  {-# INLINE ofoldl' #-}
  {-# INLINE ofoldr1Ex #-}
  {-# INLINE ofoldl1Ex' #-}
  ofoldMap = \f Vector{..} -> ofoldMap f $ snd $ V.unzip entries
  ofoldr = \f r Vector{..} -> ofoldr f r $ snd $ V.unzip entries
  ofoldl' = \f r Vector{..} -> ofoldl' f r $ snd $ V.unzip entries
  ofoldr1Ex = \f Vector{..} -> ofoldr1Ex f $ snd $ V.unzip entries
  ofoldl1Ex' = \f Vector{..} -> ofoldl1Ex' f $ snd $ V.unzip entries

cmap :: (Unbox a, Unbox b) => (a -> b) -> Vector a -> Vector b
{-# INLINE cmap #-}
cmap = \f v ->
  let (indices, values) = V.unzip $ entries v
  in v { entries = V.zip indices $ V.map f values }

-- | Add two sparse vectors into a mutable unboxed sparse vector. The
-- destination vector length must be greater than or equal to the sum of the
-- lengths of the input vectors, but this condition is not checked
-- (hence "unsafe"). Duplicate entries are summed and the number of unique
-- entries is returned.
unsafeLinInto
  :: (Num a, PrimMonad m, Unbox a)
  => a -> Vector a -> a -> Vector a -> Int -> MVector (PrimState m) (Int, a)
  -> m Int
{-# INLINE unsafeLinInto #-}
unsafeLinInto = \a as b bs off cs -> do
  let (ixsA, xsA) = V.unzip $ entries as
      (ixsB, xsB) = V.unzip $ entries bs
      (ixsC, xsC) = MV.unzip cs
      lenA = V.length ixsA
      lenB = V.length ixsB

      go i j k =
        if i < lenA
          then if j < lenB
            then do
              rA <- V.unsafeIndexM ixsA i
              rB <- V.unsafeIndexM ixsB j
              case compare rA rB of
                LT -> do
                  MV.unsafeWrite ixsC k rA
                  xA <- V.unsafeIndexM xsA i
                  MV.unsafeWrite xsC k $! a * xA
                  go (i + 1) j (k + 1)
                EQ -> do
                  MV.unsafeWrite ixsC k rA
                  xA <- V.unsafeIndexM xsA i
                  xB <- V.unsafeIndexM xsB j
                  MV.unsafeWrite xsC k $! a * xA + b * xB
                  go (i + 1) (j + 1) (k + 1)
                GT -> do
                  MV.unsafeWrite ixsC k rB
                  xB <- V.unsafeIndexM xsB j
                  MV.unsafeWrite xsC k $! b * xB
                  go i (j + 1) (k + 1)
            else do
              -- no more elements to consider in bs, copy remaining as
              let len' = lenA - i
              V.copy (MV.slice k len' ixsC) (V.slice i len' ixsA)
              V.forM_ (V.enumFromN 0 len') $ \ii -> do
                x <- V.unsafeIndexM xsA (i + ii)
                MV.unsafeWrite xsC (k + ii) $! a * x
              return $! k + len'
          else if j < lenB
            then do
              -- no more elements to consider in as, copy remaining bs
              let len' = lenB - j
              V.copy (MV.slice k len' ixsC) (V.slice j len' ixsB)
              V.forM_ (V.enumFromN 0 len') $ \jj -> do
                x <- V.unsafeIndexM xsB (j + jj)
                MV.unsafeWrite xsC (k + jj) $! b * x
              return $! k + len'
            else return k
  off' <- go 0 0 off
  return $! off' - off
