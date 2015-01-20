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
import qualified Data.Vector.Generic as V
import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed as U

zipWithM3_
  :: (Monad m, Vector u a, Vector v b, Vector w c)
  => (a -> b -> c -> m d) -> u a -> v b -> w c -> m ()
{-# INLINE zipWithM3_ #-}
zipWithM3_ f as bs cs = do
  let len = minimum [V.length as, V.length bs, V.length cs]
  U.forM_ (U.enumFromN 0 len) $ \ix -> do
    a <- V.unsafeIndexM as ix
    b <- V.unsafeIndexM bs ix
    c <- V.unsafeIndexM cs ix
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
  let len' = MV.length v - ix - abs off
      src
        | off >= 0 = MV.unsafeSlice ix len' v
        | otherwise = MV.unsafeSlice (ix + abs off) len' v
      dst
        | off >= 0 = MV.unsafeSlice (ix + abs off) len' v
        | otherwise = MV.unsafeSlice ix len' v
  MV.move dst src

preincrement
  :: (Num a, PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> m a
{-# INLINE preincrement #-}
preincrement = \v ix -> do
  count <- MV.unsafeRead v ix
  MV.unsafeWrite v ix $! count + 1
  return count

forSlicesM2_
  :: (Integral i, Monad m, Vector u i, Vector v a, Vector w b)
  => u i -> v a -> w b -> (Int -> v a -> w b -> m c) -> m ()
{-# INLINE forSlicesM2_ #-}
forSlicesM2_ = \ptrs as bs f -> do
  U.forM_ (U.enumFromN 0 $ V.length ptrs - 1) $ \c -> do
    start <- liftM fromIntegral $ V.unsafeIndexM ptrs c
    end <- liftM fromIntegral $ V.unsafeIndexM ptrs (c + 1)
    let as' = V.unsafeSlice start (end - start) as
        bs' = V.unsafeSlice start (end - start) bs
    f c as' bs'

nondecreasing :: (Ord a, Vector v Bool, Vector v a) => v a -> Bool
nondecreasing vec
  | V.null vec = True
  | otherwise = V.and $ V.zipWith (<=) (V.init vec) (V.tail vec)

increasing :: (Ord a, Vector v Bool, Vector v a) => v a -> Bool
increasing vec
  | V.null vec = True
  | otherwise = V.and $ V.zipWith (<) (V.init vec) (V.tail vec)
