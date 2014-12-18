{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}

module Numeric.LinearAlgebra.Umfpack.Internal where

import Data.Complex (Complex)
import Foreign.Ptr (Ptr)
import GHC.Stack (currentCallStack, errorWithStackTrace)

type Symbolic a = Ptr ()
type Control = Ptr Double
type Info = Ptr Double
foreign import ccall "umfpack.h umfpack_di_symbolic" c_symbolic_real
    :: Int  -- ^ matrix rows
    -> Int  -- ^ matrix columns
    -> Ptr Int  -- ^ index of initial entry in each column
    -> Ptr Int  -- ^ row indices of matrix entries
    -> Ptr Double  -- ^ matrix entries
    -> Ptr (Symbolic Double)
    -> Control
    -> Info
    -> IO Int  -- ^ status code

foreign import ccall "umfpack.h umfpack_zi_symbolic" c_symbolic_complex
    :: Int  -- ^ matrix rows
    -> Int  -- ^ matrix columns
    -> Ptr Int  -- ^ index of initial entry in each column
    -> Ptr Int  -- ^ row indices of matrix entries
    -> Ptr Double  -- ^ matrix entries (real parts)
    -> Ptr Double  -- ^ matrix entries (complex parts)
    -> Ptr (Symbolic (Complex Double))
    -> Control
    -> Info
    -> IO Int  -- ^ status code

type Numeric a = Ptr ()
foreign import ccall "umfpack.h umfpack_di_numeric" c_numeric_real
    :: Ptr Int  -- ^ index of initial entry in each column
    -> Ptr Int  -- ^ row indices of matrix entries
    -> Ptr Double  -- ^ matrix entries
    -> Symbolic Double
    -> Ptr (Numeric Double)
    -> Control
    -> Info
    -> IO Int  -- ^ status code

foreign import ccall "umfpack.h umfpack_zi_numeric" c_numeric_complex
    :: Ptr Int  -- ^ index of initial entry in each column
    -> Ptr Int  -- ^ row indices of matrix entries
    -> Ptr Double  -- ^ matrix entries (real parts)
    -> Ptr Double  -- ^ matrix entries (complex parts)
    -> Symbolic (Complex Double)
    -> Ptr (Numeric (Complex Double))
    -> Control
    -> Info
    -> IO Int  -- ^ status code

foreign import ccall "umfpack.h umfpack_di_solve" c_solve_real
    :: Int  -- ^ system type
    -> Ptr Int  -- ^ index of initial entry in each column
    -> Ptr Int  -- ^ row indices of matrix entries
    -> Ptr Double  -- ^ matrix entries
    -> Ptr Double  -- ^ solution vector
    -> Ptr Double  -- ^ rhs vector
    -> Numeric Double
    -> Control
    -> Info
    -> IO Int  -- ^ status code

foreign import ccall "umfpack.h umfpack_zi_solve" c_solve_complex
    :: Int  -- ^ system type
    -> Ptr Int  -- ^ index of initial entry in each column
    -> Ptr Int  -- ^ row indices of matrix entries
    -> Ptr Double  -- ^ matrix entries (real parts)
    -> Ptr Double  -- ^ matrix entries (imaginary parts)
    -> Ptr Double  -- ^ solution vector (real parts)
    -> Ptr Double  -- ^ solution vector (imaginary parts)
    -> Ptr Double  -- ^ rhs vector (real parts)
    -> Ptr Double  -- ^ rhs vector (imaginary parts)
    -> Numeric (Complex Double)
    -> Control
    -> Info
    -> IO Int  -- ^ status code

foreign import ccall "umfpack.h umfpack_di_free_symbolic" c_free_symbolic_real
    :: Symbolic Double -> IO ()

foreign import ccall "umfpack.h umfpack_zi_free_symbolic" c_free_symbolic_complex
    :: Symbolic (Complex Double) -> IO ()

foreign import ccall "umfpack.h umfpack_di_free_numeric" c_free_numeric_real
    :: Numeric Double -> IO ()

foreign import ccall "umfpack.h umfpack_zi_free_numeric" c_free_numeric_complex
    :: Numeric (Complex Double) -> IO ()

wrap_umfpack :: IO (Int, a) -> IO a
wrap_umfpack act = do
    (status, result) <- act
    case fromIntegral status of
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
    return result

warnWithStackTrace :: String -> IO ()
warnWithStackTrace x = do
    ccs <- currentCallStack
    putStrLn $ x ++ "\n" ++ renderStack ccs
  where
    renderStack :: [String] -> String
    renderStack strs = "Stack trace:" ++ concatMap ("\n  "++) (reverse strs)
