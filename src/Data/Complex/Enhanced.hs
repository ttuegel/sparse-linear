{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Complex.Enhanced
       ( RealOf, ComplexOf, IsReal(..), IsImag(..)
       , Complex(..), magnitude
       ) where

import Data.Complex
import Foreign.Storable.Complex ()

type family RealOf a where
  RealOf (Complex a) = a
  RealOf a = a

type family ComplexOf a where
  ComplexOf (Complex a) = Complex a
  ComplexOf a = a

class IsReal a where
  real :: Num (RealOf a) => RealOf a -> a
  conj :: a -> a

class IsImag a where
  imag :: Num (RealOf a) => RealOf a -> a

instance IsReal Double where
  {-# INLINE real #-}
  {-# INLINE conj #-}
  real = id
  conj = id

instance IsReal (Complex Double) where
  {-# INLINE real #-}
  {-# INLINE conj #-}
  real = (:+ 0)
  conj = conjugate

instance IsImag (Complex Double) where
  {-# INLINE imag #-}
  imag = (0 :+)
