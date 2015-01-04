{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Matrix.Sparse.Type
       ( Matrix(..), nonZero
       , fromColumns, toColumns
       , cmap
       ) where

import Control.Applicative
import Data.MonoTraversable (Element, MonoFoldable(..), MonoFunctor(..))
import qualified Data.Vector as Box
import qualified Data.Vector.Generic as V
import Data.Vector.Storable (Storable, Vector)
import GHC.Stack (errorWithStackTrace)

import Data.Cs
import qualified Data.Vector.Sparse as S

-- | Matrix in compressed sparse column (CSC) format.
data Matrix a = Matrix
    { nRows :: !Int
    , nColumns :: !Int
    , columnPointers :: !(Vector CInt)
    , rowIndices :: !(Vector CInt)
    , values :: !(Vector a)
    }
  deriving (Eq, Read, Show)

nonZero :: Storable a => Matrix a -> Int
{-# INLINE nonZero #-}
nonZero = \m -> V.length $ values m

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

toColumns :: Storable a => Matrix a -> Box.Vector (S.Vector a)
{-# INLINE toColumns #-}
toColumns = \Matrix{..} -> do
  c <- V.enumFromN 0 nColumns
  start <- fromIntegral <$> V.unsafeIndexM columnPointers c
  end <- fromIntegral <$> V.unsafeIndexM columnPointers (c + 1)
  let len = end - start
  return S.Vector
    { S.dim = nRows
    , S.indices = V.slice start len rowIndices
    , S.values = V.slice start len values
    }

fromColumns :: Storable a => Box.Vector (S.Vector a) -> Matrix a
{-# INLINE fromColumns #-}
fromColumns columns
  | V.null columns = errorWithStackTrace "fromColumns: empty list"
  | V.any ((/= nr) . S.dim) columns =
      errorWithStackTrace "fromColumns: row dimensions do not match"
  | otherwise =
      Matrix
      { nRows = nr
      , nColumns = V.length columns
      , columnPointers =
          V.scanl' (+) 0 $ V.convert
          $ V.map (fromIntegral . V.length . S.values) columns
      , rowIndices = V.concat $ V.toList $ V.map S.indices columns
      , values = V.concat $ V.toList $ V.map S.values columns
      }
  where
    nr = S.dim $ V.head columns
