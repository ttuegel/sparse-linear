{-# LANGUAGE RecordWildCards #-}

module Numeric.LinearAlgebra.Umfpack where

import Data.Traversable
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

import Numeric.LinearAlgebra.Sparse
import Numeric.LinearAlgebra.Matrix.Sparse.Internal
import Numeric.LinearAlgebra.Umfpack.Internal

linearSolve
  :: (CxSparse a, Num a, Umfpack a)
  => Matrix a -> [Vector a] -> [Vector a]
linearSolve mat@Matrix{..} bs =
    unsafePerformIO $
    unsafeWithMatrix mat $ \cs -> do
        sym <- alloca $ \psym -> do
            wrap_umfpack $ umfpack_symbolic cs psym nullPtr nullPtr
            peek psym
        num <- alloca $ \pnum -> do
            wrap_umfpack $ umfpack_numeric cs sym pnum nullPtr nullPtr
            peek pnum
        umfpack_free_symbolic sym
        xs <- forM bs $ \b -> V.unsafeWith b $ \pb -> do
            x <- MV.replicate ncols 0
            MV.unsafeWith x $ \px ->
                wrap_umfpack $ umfpack_solve cs px pb num nullPtr nullPtr
            V.freeze x
        umfpack_free_numeric num
        return xs

(<\>) :: (CxSparse a, Num a, Umfpack a) => Matrix a -> Vector a -> Vector a
(<\>) mat b = head $ linearSolve mat [b]
