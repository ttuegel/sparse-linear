{-# LANGUAGE RecordWildCards #-}

module Data.Matrix
       ( Matrix(..), fromVector, toColumns ) where

import Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as V
import GHC.Stack (errorWithStackTrace)

-- | Dense matrix in column-major format
data Matrix a = Matrix
    { nRows :: !Int
    , nColumns :: !Int
    , values :: !(Vector a)
    }
  deriving (Eq, Read, Show)

fromVector :: Storable a => Int -> Int -> Vector a -> Matrix a
{-# INLINE fromVector #-}
fromVector nr nc v
  | len /= nr * nc = errorWithStackTrace $
      "vector length " ++ show len ++ " does not match matrix size "
      ++ show nr ++ " by " ++ show nc
  | otherwise = Matrix { nRows = nr, nColumns = nc, values = v }
  where
    len = V.length v

toColumns :: Storable a => Matrix a -> [Vector a]
{-# INLINE toColumns #-}
toColumns Matrix{..} =
  map (\c -> V.slice (nRows * c) nRows values) [0..(nColumns - 1)]
