{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Triples where

import Control.Monad (liftM3)
import qualified Data.Vector.Generic.Mutable as MG
import Data.Vector.Storable (Storable)
import Data.Vector.Storable.Mutable (MVector)

data MTriples :: * -> * -> * where
  MTriples :: !(MVector s a) -> !(MVector s b) -> !(MVector s c) -> MTriples s (a, b, c)

instance (Storable a, Storable b, Storable c) => MG.MVector MTriples (a, b, c) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  basicLength = \(MTriples as _ _) -> MG.basicLength as
  basicUnsafeSlice = \off len (MTriples as bs cs) ->
    MTriples
      (MG.basicUnsafeSlice off len as)
      (MG.basicUnsafeSlice off len bs)
      (MG.basicUnsafeSlice off len cs)
  basicOverlaps = \(MTriples as bs cs) (MTriples as' bs' cs') ->
    MG.basicOverlaps as as'
    || MG.basicOverlaps bs bs'
    || MG.basicOverlaps cs cs'
  basicUnsafeNew = \len ->
    liftM3 MTriples
      (MG.basicUnsafeNew len)
      (MG.basicUnsafeNew len)
      (MG.basicUnsafeNew len)
  basicUnsafeRead = \(MTriples as bs cs) ix -> do
    liftM3 (,,)
      (MG.basicUnsafeRead as ix)
      (MG.basicUnsafeRead bs ix)
      (MG.basicUnsafeRead cs ix)
  basicUnsafeWrite = \(MTriples as bs cs) ix (a, b, c) -> do
    MG.basicUnsafeWrite as ix a
    MG.basicUnsafeWrite bs ix b
    MG.basicUnsafeWrite cs ix c
