{-# LANGUAGE RecordWildCards #-}

module Numeric.LinearAlgebra.Umfpack
    ( Umfpack()
    , linearSolve, (<\>)
    ) where

import Data.Traversable
import Data.Vector.Mutability
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

import Data.Matrix.Sparse
import Numeric.LinearAlgebra.Sparse
import Numeric.LinearAlgebra.Umfpack.Internal

linearSolve
  :: (CxSparse a, Num a, Umfpack a, WithImm v)
  => Matrix a -> [v a] -> [Vector a]
linearSolve mat@Matrix{..} bs =
    unsafePerformIO $
    unsafeWithMatrix mat $ \cs -> do
        psym <- malloc
        wrap_umfpack $ umfpack_symbolic cs psym nullPtr nullPtr
        pnum <- malloc
        sym <- peek psym
        wrap_umfpack $ umfpack_numeric cs sym pnum nullPtr nullPtr
        umfpack_free_symbolic psym
        num <- peek pnum
        xs <- forM bs $ \b -> do
          x <- MV.replicate nColumns 0
          _ <- withImm b $ \pb -> MV.unsafeWith x $ \px ->
            wrap_umfpack $ umfpack_solve cs px pb num nullPtr nullPtr
          V.freeze x
        umfpack_free_numeric pnum
        return xs

(<\>) :: (CxSparse a, Num a, Umfpack a) => Matrix a -> Vector a -> Vector a
(<\>) mat b = head $ linearSolve mat [b]
