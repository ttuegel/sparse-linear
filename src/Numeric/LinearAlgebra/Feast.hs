{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Numeric.LinearAlgebra.Feast
    ( Feast, geigSH, eigSH
    ) where

import Control.Applicative
import Control.Concurrent.ParallelIO.Local (withPool, parallel_)
import Control.Monad (when)
import Data.IORef
import Data.Maybe (isJust)
import Data.Traversable
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.Marshal.Utils (with)
import Foreign.Storable
import GHC.Conc.Sync (getNumCapabilities)
import GHC.Stack (errorWithStackTrace)
import Prelude hiding (concat, error, mapM)
import System.GlobalLock (lock)
import System.IO.Unsafe

import Data.Complex.Enhanced
import qualified Data.Packed.Matrix as Dense
import qualified Numeric.LinearAlgebra.Devel as Dense
import Data.Matrix.Sparse
import Numeric.LinearAlgebra.Feast.Internal
import Numeric.LinearAlgebra.Umfpack

error :: String -> a
error = errorWithStackTrace

geigSH_
  :: (Feast a)
  => Int -> (RealOf a, RealOf a)
  -> Maybe (Dense.Matrix a) -- ^ subspace guess
  -> Matrix Col a -> Matrix Col a
  -> (Int, Vector (RealOf a), Dense.Matrix a)
{-# INLINE geigSH_ #-}
geigSH_ !m0 (!_emin, !_emax) !guess !matA !matB = geigSH_go where
  n = odim matA
  m0' = maybe m0 Dense.cols guess
  n' = maybe n Dense.rows guess

  {-# NOINLINE geigSH_go #-}
  geigSH_go
    | not (hermitian matA) = error "geigSH: matrix A not hermitian"
    | not (hermitian matB) = error "geigSH: matrix B not hermitian"
    | odim matA /= odim matB = error "geigSH: matrix sizes do not match"
    | m0 /= m0' = error "geigSH: subspace guess has wrong column dimension"
    | n /= n' = error "geigSH: subspace guess has wrong row dimension"
    | otherwise = unsafePerformIO $ lock $ do
        ncap <- getNumCapabilities
        withPool ncap $ \pool ->

          -- initialize scalars
          with (-1) $ \_ijob ->
          with (fromIntegral n) $ \n_ ->
          with 0 $ \_ze ->
          with 0 $ \epsout ->
          with 0 $ \loop ->
          with _emin $ \_emin ->
          with _emax $ \_emax ->
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

            -- initialize subspace guess
            case guess of
              Nothing -> return ()
              Just mat ->
                V.copy
                  _eigenvectors
                  (Dense.unsafeMatrixToVector (Dense.fmat mat))

            -- initialize
            fpm <- feastinit

            when (isJust guess) (MV.unsafeWrite fpm 4 1)

            let feast_go =
                  MV.unsafeWith fpm $ \_fpm ->
                  MV.unsafeWith _work1 $ \_work1 ->
                  MV.unsafeWith _work2 $ \_work2 ->
                  MV.unsafeWith _aq $ \_aq ->
                  MV.unsafeWith _bq $ \_bq ->
                  MV.unsafeWith _eigenvalues $ \_eigenvalues ->
                  MV.unsafeWith _eigenvectors $ \_eigenvectors ->
                  MV.unsafeWith _res $ \_res ->
                    feast_rci
                      _ijob n_ _ze _work1 _work2 _aq _bq
                      _fpm epsout loop _emin _emax
                      m0_ _eigenvalues _eigenvectors
                      mode _res info

            let sliceMat mat =
                  map
                    (\c -> MV.unsafeSlice (c * n) n mat)
                    [0..(m0 - 1)]
            _work1 <- return (sliceMat _work1)
            _work2 <- return (sliceMat _work2)
            _eigenvectors <- return (sliceMat _eigenvectors)

            _fact <- newIORef undefined
            _mat <- newIORef undefined

            let go = do
                  feast_go
                  _ijob <- peek _ijob
                  when (_ijob /= 0) $ do
                    case _ijob of
                     10 -> do
                       _ze <- peek _ze
                       let mat = lin (-1) matA _ze matB
                       writeIORef _mat $! mat
                       writeIORef _fact $! factor mat
                     11 -> solveLinear UmfpackNormal
                     20 -> return ()
                     21 -> solveLinear UmfpackTrans
                     30 -> multiplyWork matA
                     40 -> multiplyWork matB
                     _ -> return ()
                    go

                parMapM_ :: (a -> IO b) -> [a] -> IO ()
                parMapM_ f xs = parallel_ pool $ map f xs

                solveLinear m = do
                  !fact <- readIORef _fact
                  !mat <- readIORef _mat
                  parMapM_
                    (\work -> do
                         [soln] <- linearSolve_ fact m mat [work]
                         MV.copy work soln)
                    _work2

                multiplyWork mat = do
                  ndrop <- (+ (-1)) . fromIntegral <$> MV.unsafeRead fpm 23
                  ntake <- fromIntegral <$> MV.unsafeRead fpm 24
                  parMapM_
                    (\(!dst, !x) -> MV.set dst 0 >> gaxpy_ mat x dst)
                    (take ntake (drop ndrop (zip _work1 _eigenvectors)))
            go

            i <- peek info
            case i of
              (-3) ->
                error "geigSH: internal error in reduced eigenvalue solver"
              (-2) -> error "geigSH: internal error in inner system solver"
              (-1) -> error "geigSH: internal error in memory allocation"
              0 -> return ()
              1 -> putStrLn "geigSH: no eigenvalues in search interval"
              2 -> putStrLn "geigSH: no convergence"
              3 -> putStrLn "geigSH: subspace too small"
              4 -> putStrLn "geigSH: only subspace returned"
              _ -> error ("geigSH: unknown error, info = " ++ show i)

            (,,)
              <$> (fromIntegral <$> peek mode)
              <*> V.unsafeFreeze _eigenvalues
              <*> (Dense.fromColumns <$> mapM V.unsafeFreeze _eigenvectors)

geigSH
  :: (Feast a)
  => Int
  -> (RealOf a, RealOf a)
  -> Matrix Col a
  -> Matrix Col a
  -> (Vector (RealOf a), Dense.Matrix a)
{-# INLINE geigSH #-}
geigSH = \m0 bounds matA matB ->
  let (m, evals, evecs) = geigSH_ m0 bounds Nothing matA matB
  in (V.take m evals, Dense.takeColumns m evecs)


eigSH
  :: (Feast a)
  => Int
  -> (RealOf a, RealOf a)
  -> Matrix Col a
  -> (Vector (RealOf a), Dense.Matrix a)
{-# INLINE eigSH #-}
eigSH = \m0 bounds matA -> geigSH m0 bounds matA (ident (odim matA))
