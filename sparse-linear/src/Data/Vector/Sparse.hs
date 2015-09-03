{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}

module Data.Vector.Sparse
       ( Vector(..), null, nonZero, cmap
       , fromPairs, (|>), unsafeFromPairs
       , lin, glin
       , iforM_
       ) where

import Data.List ( mapAccumL )
import Data.Maybe ( fromJust, isJust )
import Data.Monoid
import Data.MonoTraversable
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Prelude hiding ( length, null )

import qualified Data.Vector.Sparse.ScatterGather as SG

data Vector v a
  = Vector { length :: !Int
           , indices :: U.Vector Int
           , values :: v a
           }
  deriving (Eq, Show)

null :: Vector v a -> Bool
{-# INLINE null #-}
null v = nonZero v == 0

nonZero :: Vector v a -> Int
{-# INLINE nonZero #-}
nonZero (Vector _ indices _) = U.length indices

unsafeFromPairs :: (G.Vector u Int, G.Vector v a)
                => Int -> u Int -> v a -> Vector v a
{-# INLINE unsafeFromPairs #-}
unsafeFromPairs len idx val = Vector len (G.convert idx) val

fromPairs :: (G.Vector u Int, G.Vector v a, Num a)
          => Int -> u Int -> v a -> Vector v a
{-# INLINE fromPairs #-}
fromPairs len idx val
  | nIdx /= nVal = oops (show nIdx ++ " indices and " ++ show nVal ++ "values")
  | isJust outOfBounds = oops ("index out of bounds at: "
                               ++ show (fromJust outOfBounds))
  | otherwise = SG.run len $ do
      SG.reset 0
      SG.unsafeScatterIndices idx
      SG.unsafeScatterValues idx val (+)
      SG.gather
  where
    oops msg = error ("fromPairs: " ++ msg)
    nIdx = G.length idx
    nVal = G.length val

    outOfBounds = G.findIndex (>= len) idx

(|>) :: (G.Vector v a, Num a) => Int -> [(Int, a)] -> Vector v a
{-# INLINE (|>) #-}
(|>) dim pairs = fromPairs dim indices values
  where
    (U.fromList -> indices, G.fromList -> values) = unzip pairs

type instance Element (Vector v a) = a

instance G.Vector v a => MonoFunctor (Vector v a) where
  {-# INLINE omap #-}
  omap f (Vector len indices values) = Vector len indices (G.map f values)

instance (G.Vector v a) => MonoFoldable (Vector v a) where
  {-# INLINE ofoldMap #-}
  {-# INLINE ofoldr #-}
  {-# INLINE ofoldl' #-}
  {-# INLINE ofoldr1Ex #-}
  {-# INLINE ofoldl1Ex' #-}
  ofoldMap f (Vector _ _ values) = G.foldr (\a -> mappend (f a)) mempty values
  ofoldr f r (Vector _ _ values) = G.foldr f r values
  ofoldl' f r (Vector _ _ values) = G.foldl' f r values
  ofoldr1Ex f (Vector _ _ values) = G.foldr1 f values
  ofoldl1Ex' f (Vector _ _ values) = G.foldl1' f values

cmap :: (G.Vector v a, G.Vector v b) => (a -> b) -> Vector v a -> Vector v b
{-# INLINE cmap #-}
cmap f (Vector len indices values) = Vector len indices (G.map f values)

glin :: (G.Vector u a, G.Vector v b, G.Vector w c)
     => c
     -> (c -> a -> c) -> Vector u a
     -> (c -> b -> c) -> Vector v b
     -> Vector w c
{-# INLINE glin #-}
glin c0 adda as addb bs = SG.run len $ do
  SG.reset c0
  SG.unsafeScatter as adda
  SG.unsafeScatter bs addb
  SG.gather
  where
    lenA = length as
    lenB = length bs
    len | lenA == lenB = lenA
        | otherwise = oops "vector lengths differ"
    oops msg = error ("Data.Vector.Sparse.glin: " ++ msg)

lin :: (G.Vector u a, G.Vector v a, G.Vector w a, Num a)
    => a -> Vector u a -> a -> Vector v a -> Vector w a
{-# INLINE lin #-}
lin a0 as b0 bs = glin 0 (\c a -> a0 * a + c) as (\c b -> b0 * b + c) bs

instance (G.Vector v a, Num a) => Num (Vector v a) where
  {-# INLINE (+) #-}
  {-# INLINE (*) #-}
  {-# INLINE (-) #-}
  {-# INLINE negate #-}
  {-# INLINE abs #-}
  {-# INLINE signum #-}
  (+) as bs = glin 0 (+) as (+) bs
  (*) as bs = glin 0 (+) as (*) bs
  (-) as bs = glin 0 (+) as (-) bs
  negate = omap negate
  abs = omap abs
  signum = omap signum
  fromInteger = error "Data.Vector.Sparse.fromInteger: not implemented"

instance G.Vector v a => Monoid (Vector v a) where
  {-# INLINE mempty #-}
  mempty = Vector 0 U.empty G.empty

  {-# INLINE mappend #-}
  mappend a b = mconcat [a, b]

  {-# INLINE mconcat #-}
  mconcat xs
    = Vector { length = sum (length <$> xs)
             , indices = U.concat (snd (mapAccumL offsetIndices 0 xs))
             , values = G.concat (values <$> xs)
             }
    where
      offsetIndices off x = (off + length x, U.map (+ off) (indices x))

iforM_ :: (G.Vector v a, Monad m) => Vector v a -> (Int -> a -> m ()) -> m ()
{-# INLINE iforM_ #-}
iforM_ as f = iforM__go 0 where
  nz = nonZero as
  ixs = indices as
  vals = values as
  iforM__go !i
    | i >= nz = return ()
    | otherwise = do
        !ix <- U.unsafeIndexM ixs i
        !a <- G.unsafeIndexM vals i
        f ix a
        iforM__go (i + 1)
