{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.LinearAlgebra.Umfpack
    ( Umfpack(), linearSolve, linearSolve_, (<\>)
    ) where

import Control.Applicative
import Control.Monad (when)
import Data.Traversable
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack (errorWithStackTrace)
import Prelude hiding (mapM)
import System.IO.Unsafe (unsafePerformIO)

import Data.Complex.Enhanced
import Data.Matrix.Sparse
import Data.Matrix.Sparse.Foreign
import Numeric.LinearAlgebra.Umfpack.Internal

linearSolve_
  :: Umfpack a
  => Matrix Col a -> [IOVector a] -> IO [IOVector a]
{-# SPECIALIZE linearSolve_ :: Matrix Col Double -> [IOVector Double] -> IO [IOVector Double] #-}
{-# SPECIALIZE linearSolve_ :: Matrix Col (Complex Double) -> [IOVector (Complex Double)] -> IO [IOVector (Complex Double)] #-}
linearSolve_ mat@Matrix{..} bs =
  withConstMatrix mat $ \m n ptrs ixs xs -> do
    psym <- malloc
    _stat <- umfpack_symbolic m n ptrs ixs xs psym nullPtr nullPtr
    umfpack_report_status mat nullPtr _stat
    when (_stat < 0) $ errorWithStackTrace "linearSolve_: umfpack_symbolic failed"

    pnum <- malloc
    sym <- peek psym
    _stat <- umfpack_numeric ptrs ixs xs sym pnum nullPtr nullPtr
    umfpack_report_status mat nullPtr _stat
    umfpack_free_symbolic psym
    when (_stat < 0) $ errorWithStackTrace "linearSolve_: umfpack_numeric failed"

    num <- peek pnum
    solns <- forM bs $ \_b -> do
      _soln <- MV.replicate majDim 0
      _ <- MV.unsafeWith _b $ \_b -> MV.unsafeWith _soln $ \_soln -> do
        _stat <- umfpack_solve 0 ptrs ixs xs _soln _b num nullPtr nullPtr
        umfpack_report_status mat nullPtr _stat
        when (_stat < 0) $ errorWithStackTrace "linearSolve_: umfpack_solve failed"
      return _soln
    umfpack_free_numeric pnum
    return solns

linearSolve
  :: (Vector v a, Umfpack a)
  => Matrix Col a -> [v a] -> [v a]
{-# INLINE linearSolve #-}
linearSolve = linearSolve_go where
  {-# NOINLINE linearSolve_go #-}
  linearSolve_go mat@Matrix{..} _bs =
    unsafePerformIO $ do
      _xs <- mapM (V.unsafeThaw . V.convert) _bs >>= linearSolve_ mat
      map V.convert <$> mapM V.unsafeFreeze _xs

(<\>) :: (Vector v a, Umfpack a) => Matrix Col a -> v a -> v a
{-# INLINE (<\>) #-}
(<\>) mat b = head $ linearSolve mat [b]
