{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}

module Data.Vector.Sparse
       ( Vector(..)
       , fromPairs, (|>)
       ) where

import Control.Monad.ST (runST)
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
