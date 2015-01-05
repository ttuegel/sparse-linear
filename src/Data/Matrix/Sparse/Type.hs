{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Matrix.Sparse.Type
       ( Orient(..), orient, Trans, Indices(..)
       , Matrix(..), nonZero
       , fromSlices, slices, slice
       , cmap, CInt
       ) where

import Data.Proxy
import Data.MonoTraversable (Element, MonoFoldable(..), MonoFunctor(..))
import qualified Data.Vector as Box
import qualified Data.Vector.Generic as V
import Data.Vector.Storable (Storable, Vector)
import Foreign.C.Types (CInt)
import GHC.Stack (errorWithStackTrace)

import qualified Data.Vector.Sparse as S

data Orient = Row | Col

type family Trans (or :: Orient) where
  Trans Row = Col
  Trans Col = Row

class Indices (or :: Orient) where
  -- | Given an orientation and a pair of values associated with the row and
  -- column indices, respectively, return the value associated with the major
  -- indices.
  ixsM :: Proxy or -> a -> a -> a
  -- | Given an orientation and a pair of values associated with the row and
  -- column indices, respectively, return the value associated with the minor
  -- indices.
  ixsN :: Proxy or -> a -> a -> a
  -- | Given an orientation and a pair of values associated with the major and
  -- minor indices, respectively, return the value associated with the row
  -- indices.
  ixsR :: Proxy or -> a -> a -> a
  -- | Given an orientation and a pair of values associated with the major and
  -- minor indices, respectively, return the value associated with the column
  -- indices.
  ixsC :: Proxy or -> a -> a -> a

instance Indices Row where
  {-# INLINE ixsM #-}
  {-# INLINE ixsN #-}
  {-# INLINE ixsR #-}
  {-# INLINE ixsC #-}
  ixsM Proxy = \ixs _ -> ixs
  ixsN Proxy = \_ ixs -> ixs
  ixsR Proxy = \ixs _ -> ixs
  ixsC Proxy = \_ ixs -> ixs

instance Indices Col where
  {-# INLINE ixsM #-}
  {-# INLINE ixsN #-}
  {-# INLINE ixsR #-}
  {-# INLINE ixsC #-}
  ixsM Proxy = \_ ixs -> ixs
  ixsN Proxy = \ixs _ -> ixs
  ixsR Proxy = \_ ixs -> ixs
  ixsC Proxy = \ixs _ -> ixs

-- | Matrix in compressed sparse column (CSC) format.
data Matrix (or :: Orient) a = Matrix
    { dimM :: !Int -- ^ major/outer dimension (number of slices)
    , dimN :: !Int -- ^ minor/inner dimension (dimension of each slice)
    , pointers :: !(Vector CInt)
                  -- ^ starting index of each slice + length of values
    , indices :: !(Vector CInt) -- ^ minor/inner index of each entry
    , values :: !(Vector a)
    }
  deriving (Eq, Read, Show)

orient :: Matrix or a -> Proxy or
{-# INLINE orient #-}
orient = \_ -> Proxy

nonZero :: Storable a => Matrix or a -> Int
{-# INLINE nonZero #-}
nonZero = \Matrix{..} -> fromIntegral $ V.last pointers

type instance Element (Matrix or a) = a

instance Storable a => MonoFunctor (Matrix or a) where
  omap = \f mat -> mat { values = omap f $ values mat }
  {-# INLINE omap #-}

instance Storable a => MonoFoldable (Matrix or a) where
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

cmap :: (Storable a, Storable b) => (a -> b) -> Matrix or a -> Matrix or b
{-# INLINE cmap #-}
cmap = \f m -> m { values = V.map f $ values m }

slices :: Storable a => Matrix or a -> Box.Vector (S.Vector a)
{-# INLINE slices #-}
slices = \mat@Matrix{..} -> V.map (slice mat) $ V.enumFromN 0 dimM

fromSlices :: Storable a => Box.Vector (S.Vector a) -> Matrix or a
{-# INLINE fromSlices #-}
fromSlices slices_
  | V.null slices_ = errorWithStackTrace "fromSlices: empty list"
  | V.any ((/= n) . S.dim) slices_ =
      errorWithStackTrace "fromSlices: inner dimensions do not match"
  | otherwise =
      Matrix
      { dimM = V.length slices_
      , dimN = n
      , pointers =
          V.scanl' (+) 0 $ V.convert
          $ V.map (fromIntegral . V.length . S.values) slices_
      , indices = V.concat $ V.toList $ V.map S.indices slices_
      , values = V.concat $ V.toList $ V.map S.values slices_
      }
  where
    n = S.dim $ V.head slices_

slice :: Storable a => Matrix or a -> Int -> S.Vector a
{-# INLINE slice #-}
slice = \Matrix{..} c ->
  let start = fromIntegral $ pointers V.! c
      end = fromIntegral $ pointers V.! (c + 1)
      len = end - start
  in S.Vector
  { S.dim = dimN
  , S.indices = V.slice start len indices
  , S.values = V.slice start len values
  }
