{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.LinearAlgebra.Feast.Internal
       ( FeastRci
       , Feast(..)
       , feastinit
       ) where

import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (concat, mapM)

import Data.Complex.Enhanced
import qualified Data.Packed.Matrix as Dense
import Numeric.LinearAlgebra.Umfpack

type FeastRci a
  =  Ptr CInt  -- ^ ijob
  -> Ptr CInt  -- ^ N
  -> Ptr a  -- ^ Ze
  -> Ptr a  -- ^ work
  -> Ptr (ComplexOf a)  -- ^ workc
  -> Ptr a  -- ^ Aq
  -> Ptr a  -- ^ Sq
  -> Ptr CInt  -- ^ feastparam
  -> Ptr (RealOf a)  -- ^ epsout
  -> Ptr CInt  -- ^ loop
  -> Ptr (RealOf a)  -- ^ Emin
  -> Ptr (RealOf a)  -- ^ Emax
  -> Ptr CInt  -- ^ M0
  -> Ptr (RealOf a)  -- ^ lambda
  -> Ptr a  -- ^ q
  -> Ptr CInt  -- ^ mode
  -> Ptr (RealOf a)  -- ^ res
  -> Ptr CInt  -- ^ info
  -> IO ()

type FeastConstraint a =
  ( Dense.Element a
  , Eq a
  , IsReal a
  , Num a
  , Num (ComplexOf a)
  , Num (RealOf a)
  , Storable a
  , Storable (ComplexOf a)
  , Storable (RealOf a)
  , Umfpack a
  )

class FeastConstraint a => Feast a where
  feast_rci :: FeastRci a

foreign import ccall "zfeast_hrci_" zfeast_hrci :: FeastRci (Complex Double)

instance Feast (Complex Double) where
  {-# INLINE feast_rci #-}
  feast_rci = zfeast_hrci

foreign import ccall "dfeast_srci_" dfeast_srci :: FeastRci Double

instance Feast Double where
  {-# INLINE feast_rci #-}
  feast_rci = dfeast_srci

foreign import ccall "feastinit_" feastinit_ :: Ptr CInt -> IO ()

feastinit :: IO (IOVector CInt)
feastinit = do
  fpm <- MV.replicate 64 0
  MV.unsafeWith fpm feastinit_
  return fpm
