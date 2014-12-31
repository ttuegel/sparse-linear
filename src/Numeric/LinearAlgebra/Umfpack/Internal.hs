{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Numeric.LinearAlgebra.Umfpack.Internal
    ( Control, Info, Numeric, Symbolic
    , Umfpack(..)
    , wrap_umfpack
    ) where

import Foreign.Storable
import GHC.Stack (currentCallStack, errorWithStackTrace)

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
    -> IO Int  -- ^ status code

type UmfpackNumeric a
    =  Ptr (Cs a)  -- ^ matrix
    -> Symbolic a
    -> Ptr (Numeric a)
    -> Control
    -> Info
    -> IO Int  -- ^ status code

type UmfpackSolve a
    =  Ptr (Cs a)  -- ^ matrix
    -> Ptr a  -- ^ solution vector
    -> Ptr a  -- ^ rhs vector (real parts)
    -> Numeric a
    -> Control
    -> Info
    -> IO Int  -- ^ status code

type UmfpackFreeSymbolic a = Ptr (Symbolic a) -> IO ()

type UmfpackFreeNumeric a = Ptr (Numeric a) -> IO ()

class Umfpack a where
    umfpack_symbolic :: UmfpackSymbolic a
    umfpack_numeric :: UmfpackNumeric a
    umfpack_solve :: UmfpackSolve a
    umfpack_free_symbolic :: UmfpackFreeSymbolic a
    umfpack_free_numeric :: UmfpackFreeNumeric a

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

instance Umfpack (Complex Double) where
    {-# INLINE umfpack_symbolic #-}
    {-# INLINE umfpack_numeric #-}
    {-# INLINE umfpack_solve #-}
    {-# INLINE umfpack_free_symbolic #-}
    {-# INLINE umfpack_free_numeric #-}
    umfpack_symbolic = umfpack_ci_symbolic
    umfpack_numeric = umfpack_ci_numeric
    umfpack_solve = umfpack_ci_solve
    umfpack_free_symbolic = umfpack_ci_free_symbolic
    umfpack_free_numeric = umfpack_ci_free_numeric

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

instance Umfpack Double where
    {-# INLINE umfpack_symbolic #-}
    {-# INLINE umfpack_numeric #-}
    {-# INLINE umfpack_solve #-}
    {-# INLINE umfpack_free_symbolic #-}
    {-# INLINE umfpack_free_numeric #-}
    umfpack_symbolic = umfpack_di_symbolic
    umfpack_numeric = umfpack_di_numeric
    umfpack_solve = umfpack_di_solve
    umfpack_free_symbolic = umfpack_di_free_symbolic
    umfpack_free_numeric = umfpack_di_free_numeric

wrap_umfpack :: IO Int -> IO ()
wrap_umfpack act = do
    status <- act
    case status of
      0 -> return ()
      1 -> warnWithStackTrace "umfpack: singular matrix"
      2 -> warnWithStackTrace "umfpack: determinant underflow"
      3 -> warnWithStackTrace "umfpack: determinant overflow"
      (-1) -> errorWithStackTrace "umfpack: out of memory"
      (-3) -> errorWithStackTrace "umfpack: invalid numeric object"
      (-4) -> errorWithStackTrace "umfpack: invalid symbolic object"
      (-5) -> errorWithStackTrace "umfpack: argument missing"
      (-6) -> errorWithStackTrace "umfpack: n not positive"
      (-8) -> errorWithStackTrace "umfpack: invalid matrix"
      (-11) -> errorWithStackTrace "umfpack: different pattern"
      (-13) -> errorWithStackTrace "umfpack: invalid system"
      (-15) -> errorWithStackTrace "umfpack: invalid permutation"
      (-17) -> errorWithStackTrace "umfpack: I/O error"
      (-18) -> errorWithStackTrace "umfpack: ordering failed"
      (-911) -> errorWithStackTrace "umfpack: internal error"
      code -> errorWithStackTrace $ "umfpack: unknown error " ++ show code

warnWithStackTrace :: String -> IO ()
warnWithStackTrace str = do
    ccs <- currentCallStack
    putStrLn $ str ++ "\n" ++ renderStack ccs
  where
    renderStack :: [String] -> String
    renderStack strs = "Stack trace:" ++ concatMap ("\n  "++) (reverse strs)
