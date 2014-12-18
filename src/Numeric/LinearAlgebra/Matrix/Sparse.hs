{-# LANGUAGE ViewPatterns #-}

module Numeric.LinearAlgebra.Matrix.Sparse where

import Control.Monad (void)
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as Unbox
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.Marshal.Utils (with)
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

import Numeric.LinearAlgebra.Matrix.Sparse.Internal

mul :: CxSparse a => Matrix a -> Matrix a -> Matrix a
mul a b = unsafePerformIO $
    unsafeWithMatrix a $ \csa ->
    unsafeWithMatrix b $ \csb ->
        cs_multiply csa csb >>= peek >>= fromCs
{-# INLINE mul #-}

compress
  :: (CxSparse a, Unbox a)
  => Int -> Int -> Unbox.Vector (Int, Int, a) -> Matrix a
compress nr nc (Unbox.unzip3 -> (rs, cs, xs)) = unsafePerformIO $
    unsafeWithTriples nr nc (V.convert rs) (V.convert cs) (V.convert xs)
      $ \cs -> cs_compress cs >>= peek >>= fromCs
{-# INLINE compress #-}

transpose :: CxSparse a => Matrix a -> Matrix a
transpose a = unsafePerformIO $
    unsafeWithMatrix a $ \cs ->
        cs_transpose cs (V.length $ vals a) >>= peek >>= fromCs
{-# INLINE transpose #-}

lin :: CxSparse a => a -> Matrix a -> a -> Matrix a -> Matrix a
lin alpha a beta b = unsafePerformIO $
    unsafeWithMatrix a $ \csa ->
    unsafeWithMatrix b $ \csb ->
    with alpha $ \al ->
    with beta $ \bt ->
        cs_add csa csb al bt >>= peek >>= fromCs
{-# INLINE lin #-}

add :: (CxSparse a, Num a) => Matrix a -> Matrix a -> Matrix a
add a b = lin 1 a 1 b
{-# INLINE add #-}

gaxpy :: CxSparse a => Matrix a -> Vector a -> Vector a -> Vector a
gaxpy a x y = unsafePerformIO $
    unsafeWithMatrix a $ \csa ->
    V.unsafeWith x $ \px -> do
        y_ <- V.thaw y
        MV.unsafeWith y_ $ \py -> void $ cs_gaxpy csa px py
        V.unsafeFreeze y_
{-# INLINE gaxpy #-}

mulV :: (CxSparse a, Num a) => Matrix a -> Vector a -> Vector a
mulV a x = unsafePerformIO $
    unsafeWithMatrix a $ \csa ->
    V.unsafeWith x $ \px -> do
        y <- MV.replicate (V.length x) 0
        MV.unsafeWith y $ \py -> void $ cs_gaxpy csa px py
        V.unsafeFreeze y
{-# INLINE mulV #-}
