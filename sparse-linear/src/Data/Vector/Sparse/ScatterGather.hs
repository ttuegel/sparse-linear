{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Data.Vector.Sparse.ScatterGather
       ( SG, run, reset
       , scatter, unsafeScatter, unsafeScatterIndices, unsafeScatterValues
       , gather
       ) where

import Control.Monad.ST ( ST, runST )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Reader ( ReaderT(..), ask )
import Data.Vector.Generic ( Mutable, Vector )
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import {-# SOURCE #-} qualified Data.Vector.Sparse as S

-- * The 'SG' Monad

data StateSG v s a = StateSG !(U.MVector s Bool) !(Mutable v s a)

newtype SG v s a b = SG (ReaderT (StateSG v s a) (ST s) b)
  deriving (Applicative, Functor, Monad)

-- | Run a computation in 'SG', the scatter-gather monad. The state is not
-- automatically initialized, so 'reset' must be called before any
-- scatter-gather operations.
run :: Vector v a => Int -> (forall s. SG v s a b) -> b
{-# INLINE run #-}
run len sg = runST $ do
  pattern <- UM.new len
  values <- GM.new len
  let SG rdr = sg
  runReaderT rdr (StateSG pattern values)

-- | Initialize or re-initialize the 'SG' monad state. Use this before any
-- 'scatter' operations or after a 'gather'.
reset :: Vector v a => a -> SG v s a ()
{-# INLINE reset #-}
reset a0 = SG $ do
  StateSG pattern values <- ask
  lift $ do
    UM.set pattern False
    GM.set values a0

-- * Scatter operations

scatter
  :: (Vector u a, Vector v b)
  => S.Vector v b -> (a -> b -> a) -> SG u s a ()
{-# INLINE scatter #-}
scatter v@(S.Vector len _ _) add = do
  StateSG pattern _ <- SG ask
  if len == UM.length pattern
    then unsafeScatter v add
    else oops "vector length does not match workspace length"
  where
    oops msg = error ("scatter: " ++ msg)

unsafeScatter
  :: (Vector u a, Vector v b)
  => S.Vector v b -> (a -> b -> a) -> SG u s a ()
{-# INLINE unsafeScatter #-}
unsafeScatter (S.Vector _ indices values) add = do
  unsafeScatterIndices indices
  unsafeScatterValues indices values add

unsafeScatterIndices :: Vector u Int => u Int -> SG v s a ()
{-# INLINE unsafeScatterIndices #-}
unsafeScatterIndices indices = SG $ do
  StateSG pattern _ <- ask
  lift $ G.mapM_ (\i -> UM.unsafeWrite pattern i True) indices

unsafeScatterValues
  :: (Vector u a, Vector v b, Vector w Int)
  => w Int -> v b -> (a -> b -> a) -> SG u s a ()
{-# INLINE unsafeScatterValues #-}
unsafeScatterValues indices values add = SG $ do
  StateSG _ scattered <- ask
  let scatterValue !iV !iS = do
        !x <- G.unsafeIndexM values iV
        !y <- GM.unsafeRead scattered iS
        GM.unsafeWrite scattered iS (add y x)
  lift $ G.imapM_ scatterValue indices

-- * Gather operations

gather :: Vector v a => SG v s a (S.Vector v a)
{-# INLINE gather #-}
gather = do
  pop <- count
  indices <- gatherIndices pop
  values <- gatherValues pop
  StateSG pattern _ <- SG ask
  let len = UM.length pattern
  return (S.Vector len indices values)

count :: SG v s a Int
{-# INLINE count #-}
count = SG $ do
  StateSG pattern _ <- ask
  let countTrue !n !i = do
        occupied <- UM.unsafeRead pattern i
        return (if occupied then n + 1 else n)
  lift $ U.foldM' countTrue 0 (U.enumFromN 0 (UM.length pattern))

gatherIndices :: Int -> SG v s a (U.Vector Int)
{-# INLINE gatherIndices #-}
gatherIndices pop = SG $ do
  StateSG pattern _ <- ask
  lift $ do
    indices <- UM.new pop
    let gatherIndex !iIdx !iPat = do
          occupied <- UM.unsafeRead pattern iPat
          if not occupied
            then return iIdx
            else do
              UM.unsafeWrite indices iIdx iPat
              return (iIdx + 1)
    _ <- U.foldM' gatherIndex 0 (U.enumFromN 0 (UM.length pattern))
    U.unsafeFreeze indices

gatherValues :: Vector v a => Int -> SG v s a (v a)
{-# INLINE gatherValues #-}
gatherValues pop = SG $ do
  StateSG pattern scattered <- ask
  lift $ do
    values <- GM.new pop
    let gatherValue !iVal !iPat = do
          occupied <- UM.unsafeRead pattern iPat
          if not occupied
            then return iVal
            else do
              !x <- GM.unsafeRead scattered iPat
              GM.unsafeWrite values iVal x
              return (iVal + 1)
    _ <- U.foldM' gatherValue 0 (U.enumFromN 0 (UM.length pattern))
    G.unsafeFreeze values
