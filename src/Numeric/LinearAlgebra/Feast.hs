{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Numeric.LinearAlgebra.Feast
    ( Feast, geigSH_, geigSH, eigSH
    ) where

import Control.Applicative
import Control.Concurrent.QSem
import Control.Concurrent.QSemN
import Control.Exception (bracket_)
import Control.Monad (when)
import Control.Monad.State.Strict (MonadState(..), evalStateT)
import Control.Monad.IO.Class
import Data.Maybe (isJust)
import Data.Traversable
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.Types (CInt)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import Foreign.Storable
import GHC.Conc (forkOn, getNumCapabilities)
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

parMapM_ :: MonadIO m => (a -> IO ()) -> [a] -> m ()
parMapM_ f xs = liftIO $ do
  ncap <- getNumCapabilities
  qstart <- newQSem ncap
  qend <- newQSemN 0
  let parMapM__go n ys =
        case ys of
          [] -> return n
          (y : ys') -> forkOn n (parMapM__worker y) >> parMapM__go (n + 1) ys'
      parMapM__worker a =
        bracket_
          (waitQSem qstart)
          (signalQSem qstart >> signalQSemN qend 1)
          (f a)
  len <- parMapM__go 0 xs
  -- wait for all workers to finish
  waitQSemN qend len

doM :: Monad m => m a -> (a -> Bool) -> m ()
doM body check = doM_go where
  doM_go = do
    a <- body
    when (check a) doM_go

geigSH_
  :: (Feast a)
  => Int -> (RealOf a, RealOf a)
  -> Maybe (Dense.Matrix a) -- ^ subspace guess
  -> Matrix Col a -> Matrix Col a
  -> (Int, Vector (RealOf a), Dense.Matrix a)
{-# NOINLINE geigSH_ #-}
geigSH_ !m0 (!_emin, !_emax) !guess !matA !matB = geigSH_go where
  n = odim matA
  m0' = maybe m0 Dense.cols guess
  n' = maybe n Dense.rows guess

  geigSH_go
    | not (hermitian matA) = error "geigSH: matrix A not hermitian"
    | not (hermitian matB) = error "geigSH: matrix B not hermitian"
    | odim matA /= odim matB = error "geigSH: matrix sizes do not match"
    | m0 /= m0' = error "geigSH: subspace guess has wrong column dimension"
    | n /= n' = error "geigSH: subspace guess has wrong row dimension"
    | otherwise = unsafePerformIO $ lock $

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
                MV.unsafeWith _res $ \_res -> do
                  feast_rci
                    _ijob n_ _ze _work1 _work2 _aq _bq
                    _fpm epsout loop _emin _emax
                    m0_ _eigenvalues _eigenvectors
                    mode _res info
                  peek _ijob

          let sliceMat mat =
                map
                  (\c -> MV.unsafeSlice (c * n) n mat)
                  [0..(m0 - 1)]
          _work1 <- return (sliceMat _work1)
          _work2 <- return (sliceMat _work2)
          _eigenvectors <- return (sliceMat _eigenvectors)

          let solveLinear m = do
                Just (!mat, !fact) <- get
                parMapM_
                  (\work -> linearSolve_ fact m mat work >>= MV.copy work)
                  _work2

              multiplyWork mat = liftIO $ do
                ndrop <- (+ (-1)) . fromIntegral <$> MV.unsafeRead fpm 23
                ntake <- fromIntegral <$> MV.unsafeRead fpm 24
                parMapM_
                  (\(!dst, !x) -> MV.set dst 0 >> gaxpy_ mat x dst)
                  (take ntake (drop ndrop (zip _work1 _eigenvectors)))

              factorMatrix = do
                _ze <- liftIO (peek _ze)
                let !mat = lin (-1) matA _ze matB
                    !fact = factor mat
                put (Just (mat, fact))

          evalStateT
            (doM
              (do job <- liftIO feast_go
                  case job of
                    10 -> factorMatrix
                    11 -> solveLinear UmfpackNormal
                    20 -> return ()
                    21 -> solveLinear UmfpackTrans
                    30 -> multiplyWork matA
                    40 -> multiplyWork matB
                    _ -> return ()
                  return job)
              (/= 0))
            Nothing

          geigSH__decodeInfo info

          (,,)
            <$> (fromIntegral <$> peek mode)
            <*> V.unsafeFreeze _eigenvalues
            <*> (Dense.fromColumns <$> mapM V.unsafeFreeze _eigenvectors)

geigSH__decodeInfo :: Ptr CInt -> IO ()
geigSH__decodeInfo info = do
  peek info >>= \case
    (-3) -> error "geigSH: internal error in reduced eigenvalue solver"
    (-2) -> error "geigSH: internal error in inner system solver"
    (-1) -> error "geigSH: internal error in memory allocation"
    0 -> return ()
    1 -> putStrLn "geigSH: no eigenvalues in search interval"
    2 -> putStrLn "geigSH: no convergence"
    3 -> putStrLn "geigSH: subspace too small"
    4 -> putStrLn "geigSH: only subspace returned"
    i -> error ("geigSH: unknown error, info = " ++ show i)

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
