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
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as UM

data Vector a = Vector
  { dim :: !Int
  , entries :: !(U.Vector (Int, a))
  }
  deriving (Eq, Show)

fromPairs :: Unbox a => Int -> U.Vector (Int, a) -> Vector a
{-# INLINE fromPairs #-}
fromPairs = \dim _pairs -> runST $ do
  _pairs <- U.thaw _pairs
  Intro.sortBy (comparing fst) _pairs
  entries <- U.unsafeFreeze _pairs
  return Vector{..}

(|>) :: Unbox a => Int -> [(Int, a)] -> Vector a
{-# INLINE (|>) #-}
(|>) = \dim pairs -> fromPairs dim $ U.fromList pairs

type instance Element (Vector a) = a

instance Unbox a => MonoFunctor (Vector a) where
  {-# INLINE omap #-}
  omap = \f v ->
    let (indices, values) = U.unzip $ entries v
    in v { entries = U.zip indices $ omap f values }

instance Unbox a => MonoFoldable (Vector a) where
  {-# INLINE ofoldMap #-}
  {-# INLINE ofoldr #-}
  {-# INLINE ofoldl' #-}
  {-# INLINE ofoldr1Ex #-}
  {-# INLINE ofoldl1Ex' #-}
  ofoldMap = \f Vector{..} -> ofoldMap f $ snd $ U.unzip entries
  ofoldr = \f r Vector{..} -> ofoldr f r $ snd $ U.unzip entries
  ofoldl' = \f r Vector{..} -> ofoldl' f r $ snd $ U.unzip entries
  ofoldr1Ex = \f Vector{..} -> ofoldr1Ex f $ snd $ U.unzip entries
  ofoldl1Ex' = \f Vector{..} -> ofoldl1Ex' f $ snd $ U.unzip entries

cmap :: (Unbox a, Unbox b) => (a -> b) -> Vector a -> Vector b
{-# INLINE cmap #-}
cmap = \f v ->
  let (indices, values) = U.unzip $ entries v
  in v { entries = U.zip indices $ U.map f values }

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
  let (ixsA, xsA) = U.unzip $ entries as
      (ixsB, xsB) = U.unzip $ entries bs
      (ixsC, xsC) = UM.unzip cs
      lenA = U.length ixsA
      lenB = U.length ixsB

      go i j k =
        if i < lenA
          then if j < lenB
            then do
              rA <- U.unsafeIndexM ixsA i
              rB <- U.unsafeIndexM ixsB j
              case compare rA rB of
                LT -> do
                  UM.unsafeWrite ixsC k rA
                  xA <- U.unsafeIndexM xsA i
                  UM.unsafeWrite xsC k $! a * xA
                  go (i + 1) j (k + 1)
                EQ -> do
                  UM.unsafeWrite ixsC k rA
                  xA <- U.unsafeIndexM xsA i
                  xB <- U.unsafeIndexM xsB j
                  UM.unsafeWrite xsC k $! a * xA + b * xB
                  go (i + 1) (j + 1) (k + 1)
                GT -> do
                  UM.unsafeWrite ixsC k rB
                  xB <- U.unsafeIndexM xsB j
                  UM.unsafeWrite xsC k $! b * xB
                  go i (j + 1) (k + 1)
            else do
              -- no more elements to consider in bs, copy remaining as
              let len' = lenA - i
              U.copy (UM.slice k len' ixsC) (U.slice i len' ixsA)
              U.forM_ (U.enumFromN 0 len') $ \ii -> do
                x <- U.unsafeIndexM xsA (i + ii)
                UM.unsafeWrite xsC (k + ii) $! a * x
              return $! k + len'
          else if j < lenB
            then do
              -- no more elements to consider in as, copy remaining bs
              let len' = lenB - j
              U.copy (UM.slice k len' ixsC) (U.slice j len' ixsB)
              U.forM_ (U.enumFromN 0 len') $ \jj -> do
                x <- U.unsafeIndexM xsB (j + jj)
                UM.unsafeWrite xsC (k + jj) $! b * x
              return $! k + len'
            else return k
  off' <- go 0 0 off
  return $! off' - off
