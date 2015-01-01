{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.LinearAlgebra.Umfpack
    ( Umfpack(), linearSolve, linearSolve_, (<\>)
    ) where

import Control.Monad (when)
import Data.Traversable
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack (errorWithStackTrace)
import Prelude hiding (mapM)
import System.IO.Unsafe (unsafePerformIO)

import Data.Matrix.Sparse
import Numeric.LinearAlgebra.Umfpack.Internal

linearSolve_
  :: (CxSparse a, Num a, Umfpack a)
  => Matrix a -> [IOVector a] -> IO [IOVector a]
linearSolve_ mat@Matrix{..} bs =
  withConstCs mat $ \cs -> do
    psym <- malloc
    _stat <- umfpack_symbolic cs psym nullPtr nullPtr
    umfpack_report_status cs nullPtr _stat
    when (_stat < 0) $ errorWithStackTrace "linearSolve_: umfpack_symbolic failed"

    pnum <- malloc
    sym <- peek psym
    _stat <- umfpack_numeric cs sym pnum nullPtr nullPtr
    umfpack_report_status cs nullPtr _stat
    umfpack_free_symbolic psym
    when (_stat < 0) $ errorWithStackTrace "linearSolve_: umfpack_numeric failed"

    num <- peek pnum
    xs <- forM bs $ \_b -> do
      _x <- MV.replicate nColumns 0
      _ <- MV.unsafeWith _b $ \_b -> MV.unsafeWith _x $ \_x -> do
        _stat <- umfpack_solve cs _x _b num nullPtr nullPtr
        umfpack_report_status cs nullPtr _stat
        when (_stat < 0) $ errorWithStackTrace "linearSolve_: umfpack_solve failed"
      return _x
    umfpack_free_numeric pnum
    return xs
{-# INLINE linearSolve_ #-}

linearSolve
  :: (CxSparse a, Num a, Umfpack a)
  => Matrix a -> [Vector a] -> [Vector a]
linearSolve = linearSolve_go where
  {-# NOINLINE linearSolve_go #-}
  linearSolve_go mat@Matrix{..} _bs =
    unsafePerformIO $ do
      _bs <- mapM V.unsafeThaw _bs
      _xs <- linearSolve_ mat _bs
      mapM V.unsafeFreeze _xs
{-# INLINE linearSolve #-}

(<\>) :: (CxSparse a, Num a, Umfpack a) => Matrix a -> Vector a -> Vector a
(<\>) mat b = head $ linearSolve mat [b]
{-# INLINE (<\>) #-}
