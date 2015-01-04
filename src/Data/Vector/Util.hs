{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vector.Util
       ( zipWithM3_
       , shiftR
       ) where

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
        | off >= 0 = MV.slice ix len' v
        | otherwise = MV.slice (ix + abs off) len' v
      dst
        | off >= 0 = MV.slice (ix + abs off) len' v
        | otherwise = MV.slice ix len' v
  MV.move dst src
