{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Numeric.LinearAlgebra.Sparse.Internal where

import Data.Complex
import Foreign.Ptr
import Foreign.Storable

import Numeric.LinearAlgebra.Matrix.Sparse.Internal

type Cs_gaxpy a = Ptr (Cs a) -> Ptr a -> Ptr a -> IO Int
type Cs_compress a = Ptr (Cs a) -> IO (Ptr (Cs a))
type Cs_transpose a = Ptr (Cs a) -> Int -> IO (Ptr (Cs a))
type Cs_multiply a = Ptr (Cs a) -> Ptr (Cs a) -> IO (Ptr (Cs a))
type Cs_add a = Ptr (Cs a) -> Ptr (Cs a) -> Ptr a -> Ptr a -> IO (Ptr (Cs a))
type Cs_diag a = Ptr (Cs a) -> IO (Ptr a)

class (Storable a, Storable (Cs a)) => CxSparse a where
    cs_gaxpy :: Cs_gaxpy a
    cs_compress :: Cs_compress a
    cs_transpose :: Cs_transpose a
    cs_multiply :: Cs_multiply a
    cs_add :: Cs_add a
    cs_kron :: Cs_multiply a
    cs_diag :: Cs_diag a

foreign import ccall "cs.h cs_ci_gaxpy" cs_ci_gaxpy :: Cs_gaxpy (Complex Double)
foreign import ccall "cs.h cs_ci_compress" cs_ci_compress :: Cs_compress (Complex Double)
foreign import ccall "cs.h cs_ci_transpose" cs_ci_transpose :: Cs_transpose (Complex Double)
foreign import ccall "cs.h cs_ci_multiply" cs_ci_multiply :: Cs_multiply (Complex Double)
foreign import ccall "cs_ci_add_ptrs" cs_ci_add :: Cs_add (Complex Double)
foreign import ccall "cs_ci_kron" cs_ci_kron :: Cs_multiply (Complex Double)
foreign import ccall "cs_ci_diag" cs_ci_diag :: Cs_diag (Complex Double)

instance CxSparse (Complex Double) where
    {-# INLINE cs_gaxpy #-}
    {-# INLINE cs_compress #-}
    {-# INLINE cs_transpose #-}
    {-# INLINE cs_multiply #-}
    {-# INLINE cs_add #-}
    {-# INLINE cs_kron #-}
    {-# INLINE cs_diag #-}
    cs_gaxpy = cs_ci_gaxpy
    cs_compress = cs_ci_compress
    cs_transpose = cs_ci_transpose
    cs_multiply = cs_ci_multiply
    cs_add = cs_ci_add
    cs_kron = cs_ci_kron
    cs_diag = cs_ci_diag

