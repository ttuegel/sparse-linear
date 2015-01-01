{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Numeric.LinearAlgebra.Feast
    ( EigSH, geigSH, eigSH
    ) where

import Control.Applicative
import Control.Monad (when)
import Data.Foldable
import Data.Traversable
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
import qualified Data.Matrix.Sparse as Sparse
import Numeric.LinearAlgebra.Sparse
import Numeric.LinearAlgebra.Umfpack

type FeastRci a
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
    feast_rci :: FeastRci a

foreign import ccall "feastinit_" feastinit :: Ptr CInt -> IO ()

foreign import ccall "zfeast_hrci_" zfeast_hrci :: FeastRci (Complex Double)

foreign import ccall "dfeast_srci_" dfeast_srci :: FeastRci Double

instance Feast (Complex Double) where
    feast_rci = zfeast_hrci
    {-# INLINE feast_rci #-}

instance Feast Double where
    feast_rci = dfeast_srci
    {-# INLINE feast_rci #-}

type EigSH a =
    ( CxSparse a
    , Eq a
    , Feast a
    , IsReal a
    , Num (RealOf a)
    , Num a
    , Show a
    , Storable (RealOf a)
    , Umfpack a
    )
geigSH
  :: (EigSH a)
  => Int -> (RealOf a, RealOf a)
  -> Sparse.Matrix a -> Sparse.Matrix a
  -> (Vector (RealOf a), Dense.Matrix a)
geigSH !m0 (!_emin, !_emax) !matA !matB
  | not (assertEq matA matA') = errorWithStackTrace "matrix A must be hermitian"
  | not (assertEq matB matB') = errorWithStackTrace "matrix B must be hermitian"
  | nRows matA /= nRows matB =
      errorWithStackTrace "matrices A and B must be the same size"
  | otherwise = geigH_go
  where
    n = nColumns matA
    matA' = ctrans matA
    matB' = ctrans matB

    {-# NOINLINE geigH_go #-}
    geigH_go =
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

          _work1 <- forM [0..(m0 - 1)] $ \c ->
            return $ MV.slice (c * n) n _work1

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
                     solveLinear $ lin (-1) matA' (conj _ze) matB'
                   30 -> multiplyWork matA
                   40 -> multiplyWork matB
                   _ -> return ()
                  geigSH_go
              solveLinear mat = do
                solns <- linearSolve_ mat _work1
                forM_ (zip _work1 solns) $ uncurry MV.copy
              multiplyWork mat = do
                i <- (+ (-1)) . fromIntegral <$> peekElemOff fpm 23
                j <- (+ i) . fromIntegral <$> peekElemOff fpm 24
                _eigenvectors <- forM [i..(j - 1)] $ \c ->
                  return $ MV.slice (c * n) n _eigenvectors
                _work1 <- return $ take (j - i) $ drop (i - 1) _work1
                forM_ (zip _work1 _eigenvectors) $ \(dst, x) -> do
                  MV.set dst 0
                  gaxpy_ mat x dst
          geigSH_go

          _eigenvalues <- V.unsafeFreeze _eigenvalues
          _eigenvectors <- V.unsafeFreeze _eigenvectors

          return (_eigenvalues, Dense.fromVector m0 m0 _eigenvectors)
{-# INLINE geigSH #-}

eigSH
  :: (EigSH a)
  => Int
  -> (RealOf a, RealOf a)
  -> Sparse.Matrix a
  -> (Vector (RealOf a), Dense.Matrix a)
eigSH = \m0 bounds matA -> geigSH m0 bounds matA $ ident $ nColumns matA
{-# INLINE eigSH #-}
