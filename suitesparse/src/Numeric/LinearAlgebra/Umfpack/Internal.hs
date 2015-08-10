{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Numeric.LinearAlgebra.Umfpack.Internal
    ( Control, Info, Numeric, Symbolic
    , Umfpack(..)
    ) where

import Data.Vector.Unboxed (Unbox)
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (FinalizerPtr)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable

import Data.Complex.Enhanced
import Data.Matrix.Sparse (Matrix)

type Control = Ptr Double
type Info = Ptr Double
newtype Numeric a = Numeric (Ptr ()) deriving (Storable)
newtype Symbolic a = Symbolic (Ptr ()) deriving (Storable)

type UmfpackSymbolic a
  =  CInt
  -> CInt
  -> Ptr CInt
  -> Ptr CInt
  -> Ptr a
  -> Ptr (Symbolic a)
  -> Control
  -> Info
  -> IO CInt  -- ^ status code

type UmfpackNumeric a
  =  Ptr CInt
  -> Ptr CInt
  -> Ptr a
  -> Symbolic a
  -> Ptr (Numeric a)
  -> Control
  -> Info
  -> IO CInt  -- ^ status code

type UmfpackSolve a
  =  CInt
  -> Ptr CInt
  -> Ptr CInt
  -> Ptr a
  -> Ptr a -- ^ solution vector
  -> Ptr a -- ^ rhs vector
  -> Numeric a
  -> Control
  -> Info
  -> IO CInt  -- ^ status code

type UmfpackReport = Control -> CInt -> IO ()

class (Num a, Storable a, Unbox a) => Umfpack a where
    umfpack_symbolic :: UmfpackSymbolic a
    umfpack_numeric :: UmfpackNumeric a
    umfpack_solve :: UmfpackSolve a
    umfpack_free_symbolic :: FinalizerPtr (Symbolic a)
    umfpack_free_numeric :: FinalizerPtr (Numeric a)
    umfpack_report_status :: Matrix a -> UmfpackReport

foreign import ccall "umfpack.h umfpack_zi_symbolic"
  umfpack_zi_symbolic
    :: CInt
    -> CInt
    -> Ptr CInt
    -> Ptr CInt
    -> Ptr Double
    -> Ptr Double
    -> Ptr (Symbolic (Complex Double))
    -> Control
    -> Info
    -> IO CInt  -- ^ status code

foreign import ccall "umfpack.h umfpack_zi_numeric"
  umfpack_zi_numeric
    :: Ptr CInt
    -> Ptr CInt
    -> Ptr Double
    -> Ptr Double
    -> Symbolic (Complex Double)
    -> Ptr (Numeric (Complex Double))
    -> Control
    -> Info
    -> IO CInt  -- ^ status code

foreign import ccall "umfpack.h umfpack_zi_solve"
  umfpack_zi_solve
    :: CInt
    -> Ptr CInt
    -> Ptr CInt
    -> Ptr Double
    -> Ptr Double
    -> Ptr Double -- ^ solution vector
    -> Ptr Double -- ^ solution vector
    -> Ptr Double -- ^ rhs vector
    -> Ptr Double -- ^ rhs vector
    -> Numeric (Complex Double)
    -> Control
    -> Info
    -> IO CInt  -- ^ status code

foreign import ccall "umfpack.h &umfpack_zi_free_symbolic"
  umfpack_zi_free_symbolic :: FinalizerPtr (Symbolic (Complex Double))
foreign import ccall "umfpack.h &umfpack_zi_free_numeric"
  umfpack_zi_free_numeric :: FinalizerPtr (Numeric (Complex Double))
foreign import ccall "umfpack.h umfpack_zi_report_status"
  umfpack_zi_report_status :: UmfpackReport

instance Umfpack (Complex Double) where
    {-# INLINE umfpack_symbolic #-}
    {-# INLINE umfpack_numeric #-}
    {-# INLINE umfpack_solve #-}
    {-# INLINE umfpack_free_symbolic #-}
    {-# INLINE umfpack_free_numeric #-}
    {-# INLINE umfpack_report_status #-}
    umfpack_symbolic = \m n p i x ->
      umfpack_zi_symbolic m n p i (castPtr x) nullPtr
    umfpack_numeric = \p i x ->
      umfpack_zi_numeric p i (castPtr x) nullPtr
    umfpack_solve = \mode p i x y b ->
      umfpack_zi_solve mode p i
        (castPtr x) nullPtr
        (castPtr y) nullPtr
        (castPtr b) nullPtr
    umfpack_free_symbolic = umfpack_zi_free_symbolic
    umfpack_free_numeric = umfpack_zi_free_numeric
    umfpack_report_status _ = umfpack_zi_report_status

foreign import ccall "umfpack.h umfpack_di_symbolic"
  umfpack_di_symbolic :: UmfpackSymbolic Double
foreign import ccall "umfpack.h umfpack_di_numeric"
  umfpack_di_numeric :: UmfpackNumeric Double
foreign import ccall "umfpack.h umfpack_di_solve"
  umfpack_di_solve :: UmfpackSolve Double
foreign import ccall "umfpack.h &umfpack_di_free_symbolic"
  umfpack_di_free_symbolic :: FinalizerPtr (Symbolic Double)
foreign import ccall "umfpack.h &umfpack_di_free_numeric"
  umfpack_di_free_numeric :: FinalizerPtr (Numeric Double)
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
