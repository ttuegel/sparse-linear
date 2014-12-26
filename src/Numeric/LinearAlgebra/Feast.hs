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
import Data.Foldable.For
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
geigH !m0 (!emin, !emax) !matA !matB
  | not (hermitian matA) = errorWithStackTrace "matrix A must be hermitian"
  | not (hermitian matB) = errorWithStackTrace "matrix B must be hermitian"
  | nRows matA /= nColumns matA = errorWithStackTrace "matrix A must be square"
  | nRows matB /= nColumns matB = errorWithStackTrace "matrix B must be square"
  | nRows matA /= nRows matB =
      errorWithStackTrace "matrices A and B must be the same size"
  | otherwise = unsafePerformIO $ lock $
      -- initialize scalars
      with (-1) $ \ijob_ ->
      withArray (replicate 64 0) $ \fpm_ ->
      with (fromIntegral n) $ \n_ ->
      with 0 $ \ze_ ->
      with 0 $ \epsout_ ->
      with 0 $ \loop_ ->
      with emin $ \emin_ -> with emax $ \emax_ ->
      with (fromIntegral m0) $ \m0_ ->
      with 0 $ \mode_ ->
      with 0 $ \info_ -> do

          -- initialize vectors
          eigenvalues_ <- MV.replicate m0 0
          eigenvectors_ <- MV.replicate (m0 * n) 0
          work1 <- MV.replicate (m0 * n) 0
          work2 <- MV.replicate (m0 * n) 0
          aq <- MV.replicate (m0 * m0) 0
          bq <- MV.replicate (m0 * m0) 0
          res <- MV.replicate m0 0

          let vecs1 = toList $ F
                  work1
                  (\v -> MV.length v > 0)
                  (\v -> let (this, rest) = MV.splitAt n v in (# rest, this #))

              vecs2 = toList $ F
                  work2
                  (\v -> MV.length v > 0)
                  (\v -> let (this, rest) = MV.splitAt n v in (# rest, this #))

          -- initialize
          feastinit fpm_
          poke fpm_ 1

          let feast_go =
                MV.unsafeWith work1 $ \work1_ ->
                MV.unsafeWith work2 $ \work2_ ->
                MV.unsafeWith aq $ \aq_ ->
                MV.unsafeWith bq $ \bq_ ->
                MV.unsafeWith eigenvalues_ $ \eigenvalues__ ->
                MV.unsafeWith eigenvectors_ $ \eigenvectors__ ->
                MV.unsafeWith res $ \res_ ->
                  feast_rci
                    ijob_
                    n_
                    ze_
                    work1_
                    work2_
                    aq_
                    bq_
                    fpm_
                    epsout_
                    loop_
                    emin_
                    emax_
                    m0_
                    eigenvalues__
                    eigenvectors__
                    mode_
                    res_
                    info_
          let geigSH_go = do
                putStrLn "geigSH_go"
                feast_go
                ijob <- peek ijob_
                when (ijob /= 0) $ do
                  case ijob of
                   10 -> return ()
                   11 -> return ()
                   20 -> return ()
                   21 -> return ()
                   30 -> return ()
                   40 -> return ()
                   _ -> do
                     info <- peek info_
                     errorWithStackTrace
                       $ "unknown ijob " ++ show ijob ++ " info " ++ show info
          geigSH_go

          eigenvalues <- V.unsafeFreeze eigenvalues_
          eigenvectors <- V.unsafeFreeze eigenvectors_

          let eigenvectorMat = Dense.Matrix
                  { Dense.nRows = m0
                  , Dense.nColumns = m0
                  , Dense.values = eigenvectors
                  }

          return (eigenvalues, eigenvectorMat)

  where
    n = nColumns matA
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
