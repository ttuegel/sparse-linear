{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Numeric.LinearAlgebra.Sparse
    ( CxSparse()
    , Matrix(), cmap, orient, dimM, dimN
    , Orient(..), Trans, Indices(..), reorient
    , mul
    , compress, fromTriples, (><)
    , transpose, ctrans, hermitian, assertEq
    , lin
    , add
    , gaxpy, gaxpy_, mulV
    , hcat, vcat, fromBlocks, fromBlocksDiag
    , kronecker
    , takeDiag, diag, ident
    , zeros
    , module Data.Complex.Enhanced
    ) where

import Control.Applicative
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import Data.Foldable
import qualified Data.List as List
import Data.Maybe
import Data.MonoTraversable
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (MVector)
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector.Unboxed as U
import Foreign.Storable
import GHC.Stack (errorWithStackTrace)
import Prelude hiding (any, foldl1)
import System.IO.Unsafe (unsafePerformIO)

import Data.Complex.Enhanced
import Data.Matrix.Sparse
import qualified Data.Vector.Sparse as S

instance CxSparse a => Num (Matrix Col a) where
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE negate #-}
  {-# INLINE abs #-}
  {-# INLINE signum #-}
  (+) = add
  (-) = \a b -> lin 1 a (-1) b
  (*) = mul
  negate = omap negate
  abs = omap abs
  signum = omap signum
  fromInteger = errorWithStackTrace "fromInteger: not implemented"

mul :: CxSparse a => Matrix Col a -> Matrix Col a -> Matrix Col a
mul = mul_go where
  {-# NOINLINE mul_go #-}
  mul_go _a _b =
    unsafePerformIO $
    withConstCs _a $ \_a ->
    withConstCs _b $ \_b ->
      cs_multiply _a _b >>= fromCs False

fromTriples
  :: (Indices or, Num a, Storable a)
  => Int -> Int -> [(Int, Int, a)] -> Matrix or a
{-# INLINE fromTriples #-}
fromTriples = \nr nc (unzip3 -> (rs, cs, xs)) ->
  let rs' = V.fromList $ map fromIntegral rs
      cs' = V.fromList $ map fromIntegral cs
      xs' = V.fromList xs
  in compress nr nc rs' cs' xs'

(><)
  :: (Indices or, Num a, Storable a)
  => Int -> Int -> [(Int, Int, a)] -> Matrix or a
{-# INLINE (><) #-}
(><) = fromTriples

ctrans :: (IsReal a, Num a, Storable a) => Matrix or a -> Matrix (Trans or) a
{-# INLINE ctrans #-}
ctrans = omap conj . transpose

hermitian :: (Eq a, IsReal a, Num a, Storable a) => Matrix or a -> Bool
{-# INLINE hermitian #-}
hermitian m = reorient (ctrans m) == m

assertEq :: (Eq a, Show a, Storable a) => Matrix or a -> Matrix or a -> Bool
assertEq a b
  | dimN a /= dimN b = errorWithStackTrace "assertEq: inner dimensions differ"
  | dimM a /= dimM b = errorWithStackTrace "assertEq: outer dimensions differ"
  | pointers a /= pointers b =
      errorWithStackTrace "assertEq: pointers differ"
  | indices a /= indices b =
      errorWithStackTrace "assertEq: indices differ"
  | values a /= values b =
      errorWithStackTrace "assertEq: values differ"
  | otherwise = True

lin :: (Num a, Storable a) => a -> Matrix or a -> a -> Matrix or a -> Matrix or a
{-# SPECIALIZE
    lin
      :: Double -> Matrix Row Double
      -> Double -> Matrix Row Double
      -> Matrix Row Double
  #-}
{-# SPECIALIZE
    lin
      :: Double -> Matrix Col Double
      -> Double -> Matrix Col Double
      -> Matrix Col Double
  #-}
{-# SPECIALIZE
    lin
      :: (Complex Double) -> Matrix Row (Complex Double)
      -> (Complex Double) -> Matrix Row (Complex Double)
      -> Matrix Row (Complex Double)
  #-}
{-# SPECIALIZE
    lin
      :: (Complex Double) -> Matrix Col (Complex Double)
      -> (Complex Double) -> Matrix Col (Complex Double)
      -> Matrix Col (Complex Double)
  #-}
lin a matA b matB
  | dimN matA /= dimN matB =
      errorWithStackTrace "lin: inner dimensions differ"
  | dimM matA /= dimM matB =
      errorWithStackTrace "lin: outer dimensions differ"
  | otherwise = runST $ do
      let dm = dimM matA
          dn = dimN matA
      ptrs <- MV.new (dm + 1)
      MV.unsafeWrite ptrs 0 0

      let nz = nonZero matA + nonZero matB
      ixs <- MV.new nz
      xs <- MV.new nz

      U.forM_ (U.enumFromN 0 dm) $ \ixM -> do
        let sliceA = slice matA ixM
            sliceB = slice matB ixM
            lenA = V.length $ S.values sliceA
            lenB = V.length $ S.values sliceB
            ixsA = S.indices sliceA
            ixsB = S.indices sliceB
            xsA = S.values sliceA
            xsB = S.values sliceB

            dedupCopy2 !ixA !ixB !ix =
              if ixA < lenA
                then if ixB < lenB
                  then do
                    rA <- V.unsafeIndexM ixsA ixA
                    rB <- V.unsafeIndexM ixsB ixB
                    case compare rA rB of
                     LT -> do
                       MV.unsafeWrite ixs ix rA
                       x <- V.unsafeIndexM xsA ixA
                       MV.unsafeWrite xs ix $! a * x
                       dedupCopy2 (ixA + 1) ixB (ix + 1)
                     EQ -> do
                       MV.unsafeWrite ixs ix rA
                       xA <- V.unsafeIndexM xsA ixA
                       xB <- V.unsafeIndexM xsB ixB
                       MV.unsafeWrite xs ix $! a * xA + b * xB
                       dedupCopy2 (ixA + 1) (ixB + 1) (ix + 1)
                     GT -> do
                       MV.unsafeWrite ixs ix rB
                       x <- V.unsafeIndexM xsB ixB
                       MV.unsafeWrite xs ix $! b * x
                       dedupCopy2 ixA (ixB + 1) (ix + 1)
                else do
                  let len' = lenA - ixA
                  V.copy (MV.slice ix len' ixs) (V.slice ixA len' ixsA)
                  V.copy (MV.slice ix len' xs) (V.slice ixA len' xsA)
                  return $! ix + len'
              else do
                let len' = lenB - ixB
                V.copy (MV.slice ix len' ixs) (V.slice ixB len' ixsB)
                V.copy (MV.slice ix len' xs) (V.slice ixB len' xsB)
                return $! ix + len'

        off <- fromIntegral <$> MV.unsafeRead ptrs ixM
        off' <- fromIntegral <$> dedupCopy2 0 0 off
        MV.unsafeWrite ptrs (ixM + 1) off'

      pointers <- V.unsafeFreeze ptrs
      let nz' = fromIntegral $ V.last pointers
          dimM = dm
          dimN = dn
      indices <- V.unsafeFreeze $ MV.slice 0 nz' ixs
      values <- V.unsafeFreeze $ MV.slice 0 nz' xs

      return Matrix {..}

add :: (Num a, Storable a) => Matrix or a -> Matrix or a -> Matrix or a
{-# INLINE add #-}
add a b = lin 1 a 1 b

gaxpy_
  :: (Indices or, Num a, PrimMonad m, Storable a)
  => Matrix or a -> MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
{-# INLINE gaxpy_ #-}
gaxpy_ mat@Matrix{..} xs ys =
  V.forM_ (V.enumFromN 0 dimM) $ \m -> do
    S.iforM_ (slice mat m) $ \(fromIntegral -> n) a -> do
      let r = ixsR (orient mat) m n
          c = ixsC (orient mat) m n
      x <- MV.unsafeRead xs c
      y <- MV.unsafeRead ys r
      MV.unsafeWrite ys r $! y + a * x

gaxpy
  :: (Indices or, Num a, Storable a)
  => Matrix or a -> Vector a -> Vector a -> Vector a
{-# INLINE gaxpy #-}
gaxpy = \a _x _y -> runST $ do
  _y <- V.thaw _y
  _x <- V.unsafeThaw _x
  gaxpy_ a _x _y
  V.unsafeFreeze _y

mulV :: (Indices or, Num a, Storable a) => Matrix or a -> Vector a -> Vector a
{-# INLINE mulV #-}
mulV = \a _x -> runST $ do
  _x <- V.unsafeThaw _x
  y <- MV.replicate (MV.length _x) 0
  gaxpy_ a _x y
  V.unsafeFreeze y

mcat :: Storable a => Matrix or a -> Matrix or a -> Matrix or a
{-# INLINE mcat #-}
mcat a b
  | dimN a /= dimN b = errorWithStackTrace "inner dimension mismatch"
  | otherwise = Matrix
      { dimM = dm
      , dimN = dimN a
      , pointers = V.init (pointers a) V.++ (V.map (+ nza) $ pointers b)
      , indices = indices a V.++ indices b
      , values = values a V.++ values b
      }
  where
    dm = dimM a + dimM b
    nza = fromIntegral $ nonZero a

hcat :: Storable a => Matrix Col a -> Matrix Col a -> Matrix Col a
{-# INLINE hcat #-}
hcat = mcat

vcat :: Storable a => Matrix Row a -> Matrix Row a -> Matrix Row a
{-# INLINE vcat #-}
vcat = mcat

fromBlocks :: (Num a, Storable a) => [[Maybe (Matrix Col a)]] -> Matrix Row a
{-# SPECIALIZE
    fromBlocks
      :: [[Maybe (Matrix Col Double)]] -> Matrix Row Double
  #-}
{-# SPECIALIZE
    fromBlocks
      :: [[Maybe (Matrix Col (Complex Double))]] -> Matrix Row (Complex Double)
  #-}
fromBlocks = foldl1 vcat . map (reorient . foldl1 hcat) . adjustDims
  where
    adjustDims rows = do
      (r, row) <- zip [0..] rows
      return $ do
        (c, mat) <- zip [0..] row
        return $ case mat of
          Nothing -> zeros (heights V.! r) (widths V.! c)
          Just x -> x
      where
        cols = List.transpose rows
        incompatible = any (\xs -> let x = head xs in any (/= x) xs)
        underspecified = any null
        heightSpecs = map (map dimN . catMaybes) rows
        widthSpecs = map (map dimM . catMaybes) cols
        heights
          | underspecified heightSpecs =
              errorWithStackTrace "fixDimsByRow: underspecified heights"
          | incompatible heightSpecs =
              errorWithStackTrace "fixDimsByRow: incompatible heights"
          | otherwise = V.fromList $ map head heightSpecs
        widths
          | underspecified widthSpecs =
              errorWithStackTrace "fixDimsByRow: underspecified widths"
          | incompatible widthSpecs =
              errorWithStackTrace "fixDimsByRow: incompatible widths"
          | otherwise = V.fromList $ map head widthSpecs

fromBlocksDiag
  :: (Num a, Storable a) => [[Maybe (Matrix Col a)]] -> Matrix Row a
{-# INLINE fromBlocksDiag #-}
fromBlocksDiag = fromBlocks . zipWith rejoin [0..] . List.transpose where
  rejoin = \n as -> let (rs, ls) = splitAt (length as - n) as in ls ++ rs

kronecker :: (Num a, Storable a) => Matrix or a -> Matrix or a -> Matrix or a
{-# SPECIALIZE
    kronecker
      :: Matrix Row Double -> Matrix Row Double -> Matrix Row Double
  #-}
{-# SPECIALIZE
    kronecker
      :: Matrix Col Double -> Matrix Col Double -> Matrix Col Double
  #-}
{-# SPECIALIZE
    kronecker
      :: Matrix Row (Complex Double) -> Matrix Row (Complex Double)
      -> Matrix Row (Complex Double)
  #-}
{-# SPECIALIZE
    kronecker
      :: Matrix Col (Complex Double) -> Matrix Col (Complex Double)
      -> Matrix Col (Complex Double)
  #-}
kronecker matA matB = runST $ do
  let dn = dimN matA * dimN matB
      dm = dimM matA * dimM matB
      nz = nonZero matA * nonZero matB

      lengthsA = V.zipWith (-) (V.tail $ pointers matA) (pointers matA)
      lengthsB = V.zipWith (-) (V.tail $ pointers matB) (pointers matB)

  let ptrs = V.scanl' (+) 0
             $ V.concat
             $ map (\nzA -> V.map (* nzA) lengthsB)
             $ V.toList lengthsA

  _ixs <- MV.new nz
  _xs <- MV.new nz

  V.forM_ (V.enumFromN 0 $ dimM matA) $ \mA -> do
    V.forM_ (V.enumFromN 0 $ dimM matB) $ \mB -> do

      let sliceA = slice matA mA
          lenA = V.length $ S.values sliceA
          sliceB = slice matB mB
          lenB = V.length $ S.values sliceB
          m = mA * dimM matB + mB

      let copyIxs !ixA !off
            | ixA < lenA = do
                nA <- V.unsafeIndexM (S.indices sliceA) ixA
                let nOff = nA * fromIntegral (dimN matB)
                V.copy (MV.slice off lenB _ixs)
                  $ V.map (+ nOff) $ S.indices sliceB
                copyIxs (ixA + 1) (off + lenB)
            | otherwise = return ()

      V.unsafeIndexM ptrs m >>= copyIxs 0 . fromIntegral

  V.forM_ (V.enumFromN 0 $ dimM matA) $ \mA -> do
    V.forM_ (V.enumFromN 0 $ dimM matB) $ \mB -> do

      let sliceA = slice matA mA
          lenA = V.length $ S.values sliceA
          sliceB = slice matB mB
          lenB = V.length $ S.values sliceB
          m = mA * dimM matB + mB

      let copyXs !ixA !off
            | ixA < lenA = do
                a <- V.unsafeIndexM (S.values sliceA) ixA
                V.copy (MV.slice off lenB _xs)
                  $ V.map (* a) $ S.values sliceB
                copyXs (ixA + 1) (off + lenB)
            | otherwise = return ()

      V.unsafeIndexM ptrs m >>= copyXs 0 . fromIntegral

  _ixs <- V.unsafeFreeze _ixs
  _xs <- V.unsafeFreeze _xs
  return Matrix
    { dimM = dm
    , dimN = dn
    , pointers = ptrs
    , indices = _ixs
    , values = _xs
    }

takeDiag :: (Num a, Storable a) => Matrix or a -> Vector a
{-# INLINE takeDiag #-}
takeDiag = \mat@Matrix{..} ->
  flip V.map (V.enumFromN 0 $ min dimM dimN) $ \m ->
    let sl = slice mat m
    in case V.elemIndex (fromIntegral m) (S.indices sl) of
      Nothing -> 0
      Just ix -> S.values sl V.! ix

diag :: Storable a => Vector a -> Matrix or a
{-# INLINE diag #-}
diag values = Matrix{..}
  where
    dimM = V.length values
    dimN = dimM
    pointers = V.iterateN (dimM + 1) (+1) 0
    indices = V.iterateN dimM (+1) 0

ident :: (Num a, Storable a) => Int -> Matrix or a
{-# INLINE ident #-}
ident n = diag $ V.replicate n 1

zeros :: (Indices or, Storable a) => Int -> Int -> Matrix or a
{-# INLINE zeros #-}
zeros nRows nColumns = mat
  where
    pointers = V.replicate (dimM + 1) 0
    indices = V.empty
    values = V.empty
    dimM = ixsM (orient mat) nRows nColumns
    dimN = ixsN (orient mat) nRows nColumns
    mat = Matrix {..}
