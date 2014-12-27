{-# LANGUAGE FlexibleInstances #-}

module Data.Vector.Mutability where

import Control.Monad.ST (RealWorld)
import Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (MVector)
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.Ptr

class WithMut v where
  withMut :: Storable a => v a -> (Ptr a -> IO ()) -> IO (v a)

class WithImm v where
  withImm :: Storable a => v a -> (Ptr a -> IO ()) -> IO ()

instance WithMut Vector where
  withMut _v f = do
    _v <- V.thaw _v
    MV.unsafeWith _v f
    V.unsafeFreeze _v
  {-# INLINE withMut #-}

instance WithImm Vector where
  withImm = V.unsafeWith
  {-# INLINE withImm #-}

instance WithMut (MVector RealWorld) where
  withMut v f = do
    MV.unsafeWith v f
    return v
  {-# INLINE withMut #-}

instance WithImm (MVector RealWorld) where
  withImm = MV.unsafeWith
  {-# INLINE withImm #-}
