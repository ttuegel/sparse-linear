{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}

module Data.Vector.Sparse
       ( Vector(..), cmap
       , fromPairs, (|>)
       ) where

import Control.Monad.ST (runST)
import Data.MonoTraversable
import Data.Ord (comparing)
import qualified Data.Vector.Algorithms.Intro as Intro
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as V

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
