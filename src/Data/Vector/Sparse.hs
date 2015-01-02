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
import Data.Vector.Storable (Storable)
import qualified Data.Vector.Storable as V
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as U
import Foreign.C.Types (CInt(..))

data Vector a = Vector
  { dim :: !Int
  , indices :: !(V.Vector CInt)
  , values :: !(V.Vector a)
  }
  deriving (Eq, Show)

fromPairs :: (Storable a, Unbox a) => Int -> U.Vector (Int, a) -> Vector a
{-# INLINE fromPairs #-}
fromPairs = \dim _pairs -> runST $ do
  _pairs <- U.thaw _pairs
  Intro.sortBy (comparing fst) _pairs
  _pairs <- U.unsafeFreeze _pairs
  let (V.map fromIntegral . V.convert -> indices, V.convert -> values) =
        U.unzip _pairs
  return Vector{..}

(|>) :: (Storable a, Unbox a) => Int -> [(Int, a)] -> Vector a
{-# INLINE (|>) #-}
(|>) = \dim pairs -> fromPairs dim $ U.fromList pairs

type instance Element (Vector a) = a

instance Storable a => MonoFunctor (Vector a) where
  omap = \f v -> v { values = omap f $ values v }
  {-# INLINE omap #-}

instance Storable a => MonoFoldable (Vector a) where
  ofoldMap = \f Vector{..} -> ofoldMap f values
  {-# INLINE ofoldMap #-}
  ofoldr = \f r Vector{..} -> ofoldr f r values
  {-# INLINE ofoldr #-}
  ofoldl' = \f r Vector{..} -> ofoldl' f r values
  {-# INLINE ofoldl' #-}
  ofoldr1Ex = \f Vector{..} -> ofoldr1Ex f values
  {-# INLINE ofoldr1Ex #-}
  ofoldl1Ex' = \f Vector{..} -> ofoldl1Ex' f values
  {-# INLINE ofoldl1Ex' #-}

cmap :: (Storable a, Storable b) => (a -> b) -> Vector a -> Vector b
{-# INLINE cmap #-}
cmap = \f v -> v { values = V.map f $ values v }
