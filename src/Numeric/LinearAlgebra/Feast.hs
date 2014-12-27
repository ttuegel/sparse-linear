{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Numeric.LinearAlgebra.Feast
    ( EigH, geigH, eigH
    ) where

import Control.Applicative
import Control.Monad (when)
import Data.Foldable
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.Types
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack
import System.GlobalLock (lock)
import System.IO.Unsafe

import qualified Data.Matrix as Dense
import Data.Matrix.Sparse as Sparse
import Numeric.LinearAlgebra.Sparse
import Numeric.LinearAlgebra.Umfpack

type family RealOf a where
  RealOf Double = Double
  RealOf (Complex a) = a

type family ComplexOf a where
  ComplexOf Double = Complex Double
  ComplexOf (Complex a) = (Complex a)

type Feast_rci a
    =  Ptr CInt  -- ^ ijob
    -> Ptr CInt  -- ^ N
    -> Ptr a  -- ^ Ze
    -> Ptr a  -- ^ work
    -> Ptr (ComplexOf a)  -- ^ workc
    -> Ptr a  -- ^ Aq
    -> Ptr a  -- ^ Sq
    -> Ptr CInt  -- ^ feastparam
    -> Ptr (RealOf a)  -- ^ epsout
    -> Ptr CInt  -- ^ loop
    -> Ptr (RealOf a)  -- ^ Emin
    -> Ptr (RealOf a)  -- ^ Emax
    -> Ptr CInt  -- ^ M0
    -> Ptr (RealOf a)  -- ^ lambda
    -> Ptr a  -- ^ q
    -> Ptr CInt  -- ^ mode
    -> Ptr (RealOf a)  -- ^ res
    -> Ptr CInt  -- ^ info
    -> IO ()

class Feast a where
    feast_rci :: Feast_rci a

foreign import ccall "feastinit_" feastinit :: Ptr CInt -> IO ()

foreign import ccall "zfeast_hrci_" zfeast_hrci :: Feast_rci (Complex Double)

instance Feast (Complex Double) where
    feast_rci = zfeast_hrci
    {-# INLINE feast_rci #-}

type EigH a =
    ( CxSparse (Complex a)
    , Feast (Complex a)
    , Num a
    , RealFloat a
    , Storable a
    , Umfpack (Complex a)
    )
geigH
  :: (EigH a)
  => Int -> (a, a)
  -> Sparse.Matrix (Complex a) -> Sparse.Matrix (Complex a)
  -> (Vector a, Dense.Matrix (Complex a))
geigH !m0 (!_emin, !_emax) !matA !matB
  | not (hermitian matA) = errorWithStackTrace "matrix A must be hermitian"
  | not (hermitian matB) = errorWithStackTrace "matrix B must be hermitian"
  | nRows matA /= nColumns matA = errorWithStackTrace "matrix A must be square"
  | nRows matB /= nColumns matB = errorWithStackTrace "matrix B must be square"
  | nRows matA /= nRows matB =
      errorWithStackTrace "matrices A and B must be the same size"
  | otherwise =
      unsafePerformIO $ lock $
        -- initialize scalars
        with (-1) $ \_ijob ->
        withArray (replicate 64 0) $ \fpm ->
        with (fromIntegral n) $ \n_ ->
        with 0 $ \_ze ->
        with 0 $ \epsout ->
        with 0 $ \loop ->
        with _emin $ \_emin -> with _emax $ \_emax ->
        with (fromIntegral m0) $ \m0_ ->
        with 0 $ \mode ->
        with 0 $ \info -> do

          -- initialize vectors
          _eigenvalues <- MV.replicate m0 0
          _eigenvectors <- MV.replicate (m0 * n) 0
          _work1 <- MV.replicate (m0 * n) 0
          _work2 <- MV.replicate (m0 * n) 0
          _aq <- MV.replicate (m0 * m0) 0
          _bq <- MV.replicate (m0 * m0) 0
          _res <- MV.replicate m0 0

          -- initialize
          feastinit fpm

          let feast_go =
                MV.unsafeWith _work1 $ \_work1 ->
                MV.unsafeWith _work2 $ \_work2 ->
                MV.unsafeWith _aq $ \_aq ->
                MV.unsafeWith _bq $ \_bq ->
                MV.unsafeWith _eigenvalues $ \_eigenvalues ->
                MV.unsafeWith _eigenvectors $ \_eigenvectors ->
                MV.unsafeWith _res $ \_res ->
                  feast_rci
                    _ijob
                    n_
                    _ze
                    _work1
                    _work2
                    _aq
                    _bq
                    fpm
                    epsout
                    loop
                    _emin
                    _emax
                    m0_
                    _eigenvalues
                    _eigenvectors
                    mode
                    _res
                    info
          let geigSH_go = do
                feast_go
                _ijob <- peek _ijob
                when (_ijob /= 0) $ do
                  case _ijob of
                   10 -> return ()
                   11 -> do
                     _ze <- peek _ze
                     solveLinear $ lin (-1) matA _ze matB
                   20 -> return ()
                   21 -> do
                     _ze <- peek _ze
                     solveLinear $ lin (-1) matA' (conjugate _ze) matB'
                   30 -> multiplyWork matA
                   40 -> multiplyWork matB
                   _ -> return ()
                  geigSH_go
              workVectors = flip map [0..(m0 - 1)] $ \c -> MV.slice (c * n) n _work2
              solveLinear mat = do
                solns <- linearSolve mat <$> mapM V.freeze workVectors
                forM_ (zip workVectors solns) $ uncurry V.copy
              multiplyWork mat = do
                i <- (+ (-1)) . fromIntegral <$> peekElemOff fpm 23
                j <- (+ i) . fromIntegral <$> peekElemOff fpm 24
                let multiplyWork_loop c
                      | c < j = do
                          let dst = MV.slice (c * n) n _work1
                              x = MV.slice (c * n) n _eigenvectors
                          MV.set dst 0
                          _ <- gaxpy_ mat x dst
                          multiplyWork_loop $ c + 1
                      | otherwise = return ()
                multiplyWork_loop i
          geigSH_go

          _eigenvalues <- V.unsafeFreeze _eigenvalues
          _eigenvectors <- V.unsafeFreeze _eigenvectors

          let eigenvectorMat = Dense.Matrix
                  { Dense.nRows = m0
                  , Dense.nColumns = m0
                  , Dense.values = _eigenvectors
                  }

          return (_eigenvalues, eigenvectorMat)

  where
    n = nColumns matA
    matA' = ctrans matA
    matB' = ctrans matB
{-# SPECIALIZE
    geigH
      :: Int -> (Double, Double)
      -> Sparse.Matrix (Complex Double)
      -> Sparse.Matrix (Complex Double)
      -> (Vector Double, Dense.Matrix (Complex Double))
    #-}

eigH
  :: (EigH a)
  => Int
  -> (a, a)
  -> Sparse.Matrix (Complex a)
  -> (Vector a, Dense.Matrix (Complex a))
eigH = \m0 bounds matA -> geigH m0 bounds matA $ ident $ nColumns matA
{-# INLINE eigH #-}
