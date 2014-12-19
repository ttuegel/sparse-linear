{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Numeric.LinearAlgebra.Umfpack.Internal
    ( Control, Info, Numeric, Symbolic
    , Umfpack(..)
    , wrap_umfpack
    ) where

import Data.Complex (Complex)
import Foreign.Ptr (Ptr)
import Foreign.Storable
import GHC.Stack (currentCallStack, errorWithStackTrace)

import Numeric.LinearAlgebra.Matrix.Sparse.Internal

type Control = Ptr Double
type Info = Ptr Double
newtype Numeric a = Numeric { derefNum :: Ptr () } deriving (Storable)
newtype Symbolic a = Symbolic { derefSym :: Ptr () } deriving (Storable)

type Umfpack_symbolic a
    =  Ptr (Cs a)  -- ^ matrix
    -> Ptr (Symbolic a)
    -> Control
    -> Info
    -> IO Int  -- ^ status code

type Umfpack_numeric a
    =  Ptr (Cs a)  -- ^ matrix
    -> Symbolic a
    -> Ptr (Numeric a)
    -> Control
    -> Info
    -> IO Int  -- ^ status code

type Umfpack_solve a
    =  Ptr (Cs a)  -- ^ matrix
    -> Ptr a  -- ^ solution vector
    -> Ptr a  -- ^ rhs vector (real parts)
    -> Numeric a
    -> Control
    -> Info
    -> IO Int  -- ^ status code

class Umfpack a where
    umfpack_symbolic :: Umfpack_symbolic a
    umfpack_numeric :: Umfpack_numeric a
    umfpack_solve :: Umfpack_solve a
    umfpack_free_symbolic :: Symbolic a -> IO ()
    umfpack_free_numeric :: Numeric a -> IO ()

foreign import ccall "umfpack_cs_zi_symbolic" umfpack_zi_symbolic
  :: Umfpack_symbolic (Complex Double)
foreign import ccall "umfpack_cs_zi_numeric" umfpack_zi_numeric
  :: Umfpack_numeric (Complex Double)
foreign import ccall "umfpack_cs_zi_solve" umfpack_zi_solve
  :: Umfpack_solve (Complex Double)
foreign import ccall "umfpack.h umfpack_zi_free_symbolic" umfpack_zi_free_symbolic
  :: Ptr () -> IO ()
foreign import ccall "umfpack.h umfpack_zi_free_numeric" umfpack_zi_free_numeric
  :: Ptr () -> IO ()

instance Umfpack (Complex Double) where
    {-# INLINE umfpack_symbolic #-}
    {-# INLINE umfpack_numeric #-}
    {-# INLINE umfpack_solve #-}
    {-# INLINE umfpack_free_symbolic #-}
    {-# INLINE umfpack_free_numeric #-}
    umfpack_symbolic = umfpack_zi_symbolic
    umfpack_numeric = umfpack_zi_numeric
    umfpack_solve = umfpack_zi_solve
    umfpack_free_symbolic = umfpack_zi_free_symbolic . derefSym
    umfpack_free_numeric = umfpack_zi_free_numeric . derefNum

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
      x -> errorWithStackTrace $ "umfpack: unknown error " ++ show x

warnWithStackTrace :: String -> IO ()
warnWithStackTrace x = do
    ccs <- currentCallStack
    putStrLn $ x ++ "\n" ++ renderStack ccs
  where
    renderStack :: [String] -> String
    renderStack strs = "Stack trace:" ++ concatMap ("\n  "++) (reverse strs)
