{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
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
import Control.Concurrent.ParallelIO.Local (withPool, parallel)
import Control.Monad (when)
import Data.Foldable
import Data.IORef
import Data.Traversable
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.Types
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable
import GHC.Conc.Sync (getNumCapabilities)
import GHC.Stack (errorWithStackTrace)
import Prelude hiding (mapM)
import System.GlobalLock (lock)
import System.IO.Unsafe

import Data.Complex.Enhanced
import qualified Data.Packed.Matrix as Dense
import qualified Data.Matrix.Sparse as Sparse
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
    ( Eq a
    , Feast a
    , IsReal a
    , Num (RealOf a)
    , Show a
    , Storable (RealOf a)
    , Umfpack a
    , Dense.Element a
    )
geigSH
  :: (EigSH a)
  => Int -> (RealOf a, RealOf a)
  -> Sparse.Matrix Sparse.Col a -> Sparse.Matrix Sparse.Col a
  -> (Vector (RealOf a), Dense.Matrix a)
geigSH !m0 (!_emin, !_emax) !matA !matB
  | matA /= matA' = errorWithStackTrace "matrix A must be hermitian"
  | matB /= matB' = errorWithStackTrace "matrix B must be hermitian"
  | Sparse.odim matA /= Sparse.odim matB =
      errorWithStackTrace "matrices A and B must be the same size"
  | otherwise = unsafePerformIO $ lock $ do
      nCap <- getNumCapabilities
      withPool nCap geigSH_go
  where
    n = Sparse.odim matA
    matA' = Sparse.reorient $ Sparse.ctrans matA
    matB' = Sparse.reorient $ Sparse.ctrans matB

    {-# NOINLINE geigSH_go #-}
    geigSH_go pool =
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
        _work2 <- forM [0..(m0 - 1)] $ \c ->
          return $ MV.slice (c * n) n _work2
        _eigenvectors <- forM [0..(m0 - 1)] $ \c ->
          return $ MV.slice (c * n) n _eigenvectors

        _fact <- newIORef undefined
        _mat <- newIORef undefined

        let go = do
              feast_go
              _ijob <- peek _ijob
              when (_ijob /= 0) $ do
                case _ijob of
                 10 -> do
                   _ze <- peek _ze
                   let mat = Sparse.lin (-1) matA _ze matB
                   writeIORef _mat $! mat
                   writeIORef _fact $! factor mat
                 11 -> solveLinear UmfpackNormal
                 20 -> return ()
                 21 -> solveLinear UmfpackTrans
                 30 -> multiplyWork matA
                 40 -> multiplyWork matB
                 _ -> return ()
                go

            parMapM f xs = parallel pool $ map f xs

            solveLinear m = do
              !fact <- readIORef _fact
              !mat <- readIORef _mat
              solns <- map head
                       <$> parMapM (linearSolve_ fact m mat)
                       (map (: []) _work2)
              forM_ (zip _work2 solns) $ uncurry MV.copy

            multiplyWork mat = do
              i <- (+ (-1)) . fromIntegral <$> peekElemOff fpm 23
              j <- fromIntegral <$> peekElemOff fpm 24
              forM_ (take j $ drop i $ zip _work1 _eigenvectors) $ \(dst, x) -> do
                  MV.set dst 0
                  Sparse.gaxpy_ mat x dst
        go

        i <- peek info
        case i of
          (-3) -> errorWithStackTrace
            "geigSH: internal error in reduced eigenvalue solver"
          (-2) -> errorWithStackTrace
            "geigSH: internal error in inner system solver"
          (-1) -> errorWithStackTrace
            "geigSH: internal error in memory allocation"
          0 -> return ()
          1 -> putStrLn "geigSH: no eigenvalues in search interval"
          2 -> putStrLn "geigSH: no convergence"
          3 -> putStrLn "geigSH: subspace too small"
          4 -> putStrLn "geigSH: only subspace returned"
          _ -> errorWithStackTrace
               $ "geigSH: error info = " ++ show i

        m <- fromIntegral <$> peek mode
        _eigenvalues <- V.unsafeFreeze $ MV.slice 0 m _eigenvalues
        _eigenvectors <- mapM V.unsafeFreeze $ take m _eigenvectors

        return (_eigenvalues, Dense.fromColumns _eigenvectors)
{-# INLINE geigSH #-}

eigSH
  :: (EigSH a)
  => Int
  -> (RealOf a, RealOf a)
  -> Sparse.Matrix Sparse.Col a
  -> (Vector (RealOf a), Dense.Matrix a)
eigSH = \m0 bounds matA ->
  geigSH m0 bounds matA $ Sparse.ident $ Sparse.odim matA
{-# INLINE eigSH #-}
