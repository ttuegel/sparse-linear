{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.LinearAlgebra.Umfpack
    ( -- * Simple interface
      Umfpack()
    , linearSolve, (<\>)
      -- * Advanced interface
    , Analysis, analyze
    , Factors, factor
    , UmfpackMode(..), linearSolve_
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

import Data.Matrix.Sparse
import Data.Matrix.Sparse.Foreign
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Numeric.LinearAlgebra.Umfpack.Internal

-- ------------------------------------------------------------------------
-- Simple interface
-- ------------------------------------------------------------------------

linearSolve :: (Vector v a, Umfpack a) => Matrix a -> [v a] -> [v a]
{-# INLINE linearSolve #-}
linearSolve mat@Matrix{..} bs =
  unsafePerformIO $ do
    let fact = factor mat (analyze mat)
    xs <- forM bs $ \_b -> do
      _b <- V.unsafeThaw (V.convert _b)
      linearSolve_ fact UmfpackNormal mat _b
    map V.convert <$> mapM V.unsafeFreeze xs

(<\>) :: (Vector v a, Umfpack a) => Matrix a -> v a -> v a
{-# INLINE (<\>) #-}
(<\>) mat b = head $ linearSolve mat [b]

-- ------------------------------------------------------------------------
-- Advanced interface
-- ------------------------------------------------------------------------

newtype Analysis a = Analysis { fsym :: ForeignPtr (Symbolic a) }

newtype Factors a = Factors { fnum :: ForeignPtr (Numeric a) }

analyze :: Umfpack a => Matrix a -> Analysis a
{-# INLINE analyze #-}
analyze mat = unsafePerformIO $ withConstMatrix mat $ \m n p i x -> do
  sym <- malloc
  _stat <- umfpack_symbolic m n p i x sym nullPtr nullPtr
  fsym <- newForeignPtr umfpack_free_symbolic sym
  umfpack_report_status mat nullPtr _stat
  when (_stat < 0) $ errorWithStackTrace "analyze: umfpack_symbolic failed"

  return Analysis{..}

factor :: Umfpack a => Matrix a -> Analysis a -> Factors a
{-# INLINE factor #-}
factor mat Analysis{..} =
  unsafePerformIO $ withConstMatrix mat $ \_ _ p i x -> do
    num <- malloc
    _stat <- withForeignPtr fsym $ \_sym -> do
      _sym <- peek _sym
      umfpack_numeric p i x _sym num nullPtr nullPtr
    fnum <- newForeignPtr umfpack_free_numeric num
    umfpack_report_status mat nullPtr _stat
    when (_stat < 0) $ errorWithStackTrace "factor: umfpack_numeric failed"

    return Factors{..}

data UmfpackMode = UmfpackNormal | UmfpackTrans

linearSolve_
  :: Umfpack a
  => Factors a -> UmfpackMode -> Matrix a -> IOVector a -> IO (IOVector a)
{-# INLINE linearSolve_ #-}
linearSolve_ fact mode mat@Matrix{..} _b =
  withConstMatrix mat $ \_ _ p i x -> do
    _soln <- MV.replicate ncols 0
    _ <- MV.unsafeWith _b $ \_b -> MV.unsafeWith _soln $ \_soln -> do
      let m = case mode of
                UmfpackNormal -> 0
                UmfpackTrans -> 1
      _stat <- withNum fact $ \num ->
        umfpack_solve m p i x _soln _b num nullPtr nullPtr
      umfpack_report_status mat nullPtr _stat
      when (_stat < 0) $ errorWithStackTrace "linearSolve_: umfpack_solve failed"
    return _soln

-- ------------------------------------------------------------------------
-- Utilities
-- ------------------------------------------------------------------------

withNum :: Factors a -> (Numeric a -> IO b) -> IO b
{-# INLINE withNum #-}
withNum Factors{..} f = withForeignPtr fnum $ \p -> peek p >>= f
