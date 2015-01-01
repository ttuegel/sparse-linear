{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Numeric.LinearAlgebra.Umfpack.Internal
    ( Control, Info, Numeric, Symbolic
    , Umfpack(..)
    ) where

import Foreign.Storable

import Data.Complex.Enhanced
import Data.Cs

type Control = Ptr Double
type Info = Ptr Double
newtype Numeric a = Numeric (Ptr ()) deriving (Storable)
newtype Symbolic a = Symbolic (Ptr ()) deriving (Storable)

type UmfpackSymbolic a
    =  Ptr (Cs a)  -- ^ matrix
    -> Ptr (Symbolic a)
    -> Control
    -> Info
    -> IO CInt  -- ^ status code

type UmfpackNumeric a
    =  Ptr (Cs a)  -- ^ matrix
    -> Symbolic a
    -> Ptr (Numeric a)
    -> Control
    -> Info
    -> IO CInt  -- ^ status code

type UmfpackSolve a
    =  Ptr (Cs a)  -- ^ matrix
    -> Ptr a  -- ^ solution vector
    -> Ptr a  -- ^ rhs vector (real parts)
    -> Numeric a
    -> Control
    -> Info
    -> IO CInt  -- ^ status code

type UmfpackFreeSymbolic a = Ptr (Symbolic a) -> IO ()

type UmfpackFreeNumeric a = Ptr (Numeric a) -> IO ()

type UmfpackReport = Control -> CInt -> IO ()

class Umfpack a where
    umfpack_symbolic :: UmfpackSymbolic a
    umfpack_numeric :: UmfpackNumeric a
    umfpack_solve :: UmfpackSolve a
    umfpack_free_symbolic :: UmfpackFreeSymbolic a
    umfpack_free_numeric :: UmfpackFreeNumeric a
    umfpack_report_status :: Ptr (Cs a) -> UmfpackReport

foreign import ccall "umfpack_cs.h umfpack_cs_ci_symbolic"
  umfpack_ci_symbolic :: UmfpackSymbolic (Complex Double)
foreign import ccall "umfpack_cs.h umfpack_cs_ci_numeric"
  umfpack_ci_numeric :: UmfpackNumeric (Complex Double)
foreign import ccall "umfpack_cs.h umfpack_cs_ci_solve"
  umfpack_ci_solve :: UmfpackSolve (Complex Double)
foreign import ccall "umfpack.h umfpack_zi_free_symbolic"
  umfpack_ci_free_symbolic :: UmfpackFreeSymbolic (Complex Double)
foreign import ccall "umfpack.h umfpack_zi_free_numeric"
  umfpack_ci_free_numeric :: UmfpackFreeNumeric (Complex Double)
foreign import ccall "umfpack.h umfpack_zi_report_status"
  umfpack_ci_report_status :: UmfpackReport

instance Umfpack (Complex Double) where
    {-# INLINE umfpack_symbolic #-}
    {-# INLINE umfpack_numeric #-}
    {-# INLINE umfpack_solve #-}
    {-# INLINE umfpack_free_symbolic #-}
    {-# INLINE umfpack_free_numeric #-}
    {-# INLINE umfpack_report_status #-}
    umfpack_symbolic = umfpack_ci_symbolic
    umfpack_numeric = umfpack_ci_numeric
    umfpack_solve = umfpack_ci_solve
    umfpack_free_symbolic = umfpack_ci_free_symbolic
    umfpack_free_numeric = umfpack_ci_free_numeric
    umfpack_report_status _ = umfpack_ci_report_status

foreign import ccall "umfpack_cs.h umfpack_cs_di_symbolic"
  umfpack_di_symbolic :: UmfpackSymbolic Double
foreign import ccall "umfpack_cs.h umfpack_cs_di_numeric"
  umfpack_di_numeric :: UmfpackNumeric Double
foreign import ccall "umfpack_cs.h umfpack_cs_di_solve"
  umfpack_di_solve :: UmfpackSolve Double
foreign import ccall "umfpack.h umfpack_di_free_symbolic"
  umfpack_di_free_symbolic :: UmfpackFreeSymbolic Double
foreign import ccall "umfpack.h umfpack_di_free_numeric"
  umfpack_di_free_numeric :: UmfpackFreeNumeric Double
foreign import ccall "umfpack.h umfpack_di_report_status"
  umfpack_di_report_status :: UmfpackReport

instance Umfpack Double where
    {-# INLINE umfpack_symbolic #-}
    {-# INLINE umfpack_numeric #-}
    {-# INLINE umfpack_solve #-}
    {-# INLINE umfpack_free_symbolic #-}
    {-# INLINE umfpack_free_numeric #-}
    {-# INLINE umfpack_report_status #-}
    umfpack_symbolic = umfpack_di_symbolic
    umfpack_numeric = umfpack_di_numeric
    umfpack_solve = umfpack_di_solve
    umfpack_free_symbolic = umfpack_di_free_symbolic
    umfpack_free_numeric = umfpack_di_free_numeric
    umfpack_report_status _ = umfpack_di_report_status
