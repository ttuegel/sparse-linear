{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Matrix.Sparse.Type
       ( Matrix(..)
       , cmap
       ) where

import Data.MonoTraversable (Element, MonoFoldable(..), MonoFunctor(..))
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Storable, Vector)

import Data.Cs

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
