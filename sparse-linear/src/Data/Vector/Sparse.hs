{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=200 #-}

module Data.Vector.Sparse
       ( Vector(..), nonZero, length, cmap
       , fromPairs, (|>), unsafeFromPairs
       , lin, glin
       ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import Data.Maybe (fromJust, isJust)
import Data.MonoTraversable
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Prelude hiding (length)

data Vector v a = Vector !Int (U.Vector Int) (v a)
  deriving (Eq, Show)

nonZero :: Vector v a -> Int
{-# INLINE nonZero #-}
nonZero (Vector _ indices _) = U.length indices

unsafeFromPairs :: (G.Vector u Int, G.Vector v a)
                => Int -> u Int -> v a -> Vector v a
{-# INLINE unsafeFromPairs #-}
unsafeFromPairs len idx val = Vector len (G.convert idx) val

fromPairs :: (G.Vector u Int, G.Vector v a, GM.MVector (G.Mutable v) a, Num a)
          => Int -> u Int -> v a -> Vector v a
{-# INLINE fromPairs #-}
fromPairs len idx val
  | nIdx /= nVal = oops (show nIdx ++ " indices and " ++ show nVal ++ "values")
  | isJust outOfBounds = oops ("index out of bounds at: "
                               ++ show (fromJust outOfBounds))
  | otherwise = runST $ do
      pat <- UM.replicate len False
      scatterIndices pat idx
      indices <- gatherIndices pat

      scat <- GM.replicate len 0
      scatterValues (+) scat idx val
      values <- gatherValues pat scat (U.length indices)

      return (Vector len indices values)
  where
    oops msg = error ("fromPairs: " ++ msg)
    nIdx = G.length idx
    nVal = G.length val

    outOfBounds = G.findIndex (>= len) idx

scatterIndices :: (G.Vector v Int, PrimMonad m)
               => UM.MVector (PrimState m) Bool
               -> v Int
               -> m ()
{-# INLINE scatterIndices #-}
scatterIndices pat idx = G.mapM_ (\i -> UM.unsafeWrite pat i True) idx

scatterValues :: (GM.MVector u a, G.Vector v Int, G.Vector w b, PrimMonad m)
              => (a -> b -> a)
              -> u (PrimState m) a
              -> v Int
              -> w b
              -> m ()
{-# INLINE scatterValues #-}
scatterValues f dst idx val = do
  G.imapM_ scatterValue idx
  where
    scatterValue iVal iDst = do
      !x <- G.unsafeIndexM val iVal
      !y <- GM.unsafeRead dst iDst
      GM.unsafeWrite dst iDst (f y x)

gatherIndices :: PrimMonad m
              => UM.MVector (PrimState m) Bool
              -> m (U.Vector Int)
{-# LANGUAGE gatherIndices #-}
gatherIndices pat = do
  let countIndices !count iPat = do
        occupied <- UM.unsafeRead pat iPat
        return (if occupied then count + 1 else count)
  count <- U.foldM' countIndices 0 (U.enumFromN 0 (UM.length pat))

  idx <- UM.new count
  let writeIndex !iIdx !iPat = do
        occupied <- UM.unsafeRead pat iPat
        if not occupied
          then return iIdx
          else do
            UM.unsafeWrite idx iIdx iPat
            return (iIdx + 1)
  _ <- U.foldM' writeIndex 0 (U.enumFromN 0 (UM.length pat))
  U.unsafeFreeze idx

gatherValues :: (G.Vector v a, PrimMonad m)
             => UM.MVector (PrimState m) Bool
             -> G.Mutable v (PrimState m) a
             -> Int
             -> m (v a)
{-# INLINE gatherValues #-}
gatherValues pat scat count = do
  val <- GM.new count
  let writeValue !iVal !iPat = do
        occupied <- UM.unsafeRead pat iPat
        if not occupied
          then return iVal
          else do
            !x <- GM.unsafeRead scat iPat
            GM.unsafeWrite val iVal x
            return (iVal + 1)
      indices_pat = U.enumFromN 0 (UM.length pat)
  _ <- U.foldM' writeValue 0 indices_pat
  G.unsafeFreeze val

(|>) :: (G.Vector v a, Num a) => Int -> [(Int, a)] -> Vector v a
{-# INLINE (|>) #-}
(|>) dim pairs = fromPairs dim indices values
  where
    (U.fromList -> indices, G.fromList -> values) = unzip pairs

type instance Element (Vector v a) = a

instance G.Vector v a => MonoFunctor (Vector v a) where
  {-# INLINE omap #-}
  omap f (Vector len indices values)
    = Vector len indices (G.map f values)

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
glin
  c0
  adda (Vector lenA indicesA valuesA)
  addb (Vector lenB indicesB valuesB)
  = runST $ do
    pat <- UM.replicate len False
    scatterIndices pat indicesA
    scatterIndices pat indicesB
    indices <- gatherIndices pat

    scat <- GM.replicate len c0
    scatterValues adda scat indicesA valuesA
    scatterValues addb scat indicesB valuesB
    values <- gatherValues pat scat (U.length indices)

    return (Vector len indices values)
  where
    len | lenA == lenB = lenA
        | otherwise = oops "vector lengths differ"
    oops msg = error ("Data.Vector.Sparse.glin: " ++ msg)

lin :: (G.Vector u a, G.Vector v a, G.Vector w a, Num a)
    => a -> Vector u a -> a -> Vector v a -> Vector w a
{-# INLINE lin #-}
lin a0 as b0 bs
  = glin 0 (\c a -> a0 * a + c) as (\c b -> b0 * b + c) bs

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

length :: Vector v a -> Int
{-# INLINE length #-}
length (Vector len _ _) = len
