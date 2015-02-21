{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vector.Util
       ( zipWithM3_
       , shiftR
       , preincrement
       , forSlicesM2_
       , increasing, nondecreasing
       ) where

import Control.Monad (liftM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as G
import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U

zipWithM3_
  :: (Monad m, Vector u a, Vector v b, Vector w c)
  => (a -> b -> c -> m d) -> u a -> v b -> w c -> m ()
{-# INLINE zipWithM3_ #-}
zipWithM3_ f as bs cs = do
  let len = minimum [G.length as, G.length bs, G.length cs]
  U.forM_ (U.enumFromN 0 len) $ \ix -> do
    a <- G.unsafeIndexM as ix
    b <- G.unsafeIndexM bs ix
    c <- G.unsafeIndexM cs ix
    f a b c

-- | Shift the right part (relative to the given index) of a mutable vector by
-- the given offset. Positive (negative) offsets shift to the right (left).
shiftR
  :: (PrimMonad m, MVector v a)
  => v (PrimState m) a
  -> Int  -- ^ index
  -> Int  -- ^ offset
  -> m ()
{-# INLINE shiftR #-}
shiftR = \v ix off -> do
  let len' = GM.length v - ix - abs off
      src
        | off >= 0 = GM.unsafeSlice ix len' v
        | otherwise = GM.unsafeSlice (ix + abs off) len' v
      dst
        | off >= 0 = GM.unsafeSlice (ix + abs off) len' v
        | otherwise = GM.unsafeSlice ix len' v
  GM.move dst src

preincrement
  :: (Num a, PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> m a
{-# INLINE preincrement #-}
preincrement = \v ix -> do
  count <- GM.unsafeRead v ix
  GM.unsafeWrite v ix $! count + 1
  return count

forSlicesM2_
  :: (Integral i, Monad m, Vector u i, Vector v a, Vector w b)
  => u i -> v a -> w b -> (Int -> v a -> w b -> m c) -> m ()
{-# INLINE forSlicesM2_ #-}
forSlicesM2_ = \ptrs as bs f -> do
  U.forM_ (U.enumFromN 0 $ G.length ptrs - 1) $ \c -> do
    start <- liftM fromIntegral $ G.unsafeIndexM ptrs c
    end <- liftM fromIntegral $ G.unsafeIndexM ptrs (c + 1)
    let as' = G.unsafeSlice start (end - start) as
        bs' = G.unsafeSlice start (end - start) bs
    f c as' bs'

nondecreasing :: (Ord a, Vector v Bool, Vector v a) => v a -> Bool
nondecreasing vec
  | G.null vec = True
  | otherwise = G.and $ G.zipWith (<=) (G.init vec) (G.tail vec)

increasing :: (Ord a, Vector v Bool, Vector v a) => v a -> Bool
increasing vec
  | G.null vec = True
  | otherwise = G.and $ G.zipWith (<) (G.init vec) (G.tail vec)
