{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Pairs where

import Control.Monad (liftM2)
import Data.Vector.Generic (Mutable, Vector)
import qualified Data.Vector.Generic as G
import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as MG

data MPairs :: (* -> * -> *) -> * -> * -> * where
  MPairs :: !(v s a) -> !(v s b) -> MPairs v s (a, b)

data Pairs :: (* -> *) -> * -> * where
  Pairs :: !(v a) -> !(v b) -> Pairs v (a, b)

type instance Mutable (Pairs v) = MPairs (Mutable v)

instance (MVector v a, MVector v b) => MVector (MPairs v) (a, b) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  basicLength = \(MPairs _ xs) -> MG.basicLength xs
  basicUnsafeSlice = \off len (MPairs rs xs) ->
    MPairs
      (MG.basicUnsafeSlice off len rs)
      (MG.basicUnsafeSlice off len xs)
  basicOverlaps = \(MPairs ra xa) (MPairs rb xb) ->
    MG.basicOverlaps ra rb || MG.basicOverlaps xa xb
  basicUnsafeNew = \len ->
    liftM2 MPairs
      (MG.basicUnsafeNew len)
      (MG.basicUnsafeNew len)
  basicUnsafeRead = \(MPairs rs xs) ix -> do
    liftM2 (,)
      (MG.basicUnsafeRead rs ix)
      (MG.basicUnsafeRead xs ix)
  basicUnsafeWrite = \(MPairs rs xs) ix (r, x) -> do
    MG.basicUnsafeWrite rs ix r
    MG.basicUnsafeWrite xs ix x

instance (Vector v a, Vector v b) => Vector (Pairs v) (a, b) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeFreeze = \(MPairs rs xs) ->
    liftM2 Pairs
      (G.basicUnsafeFreeze rs)
      (G.basicUnsafeFreeze xs)
  basicUnsafeThaw = \(Pairs rs xs) ->
    liftM2 MPairs
      (G.basicUnsafeThaw rs)
      (G.basicUnsafeThaw xs)
  basicLength = \(Pairs _ xs) -> G.basicLength xs
  basicUnsafeSlice = \off len (Pairs rs xs) ->
    Pairs
      (G.basicUnsafeSlice off len rs)
      (G.basicUnsafeSlice off len xs)
  basicUnsafeIndexM = \(Pairs rs xs) ix ->
    liftM2 (,) (G.basicUnsafeIndexM rs ix) (G.basicUnsafeIndexM xs ix)
