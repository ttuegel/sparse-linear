{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Numeric.LinearAlgebra.Sparse
    ( CxSparse()
    , Matrix(), nRows, nColumns, cmap
    , mul
    , compress, fromTriples, (><)
    , transpose, ctrans, hermitian, assertEq
    , toColumns, toRows
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
import Control.Monad.ST (runST)
import Data.Foldable
import qualified Data.List as List
import Data.Maybe
import Data.MonoTraversable
import qualified Data.Vector as Box
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector.Unboxed as U
import Foreign.Storable
import GHC.Stack (errorWithStackTrace)
import Prelude hiding (any, foldl1)
import System.IO.Unsafe (unsafePerformIO)

import Data.Complex.Enhanced
import Data.Matrix.Sparse
import qualified Data.Vector.Sparse as S

instance CxSparse a => Num (Matrix a) where
  {-# SPECIALIZE instance Num (Matrix Double) #-}
  {-# SPECIALIZE instance Num (Matrix (Complex Double)) #-}
  (+) = add
  (-) = \a b -> lin 1 a (-1) b
  (*) = mul
  negate = omap negate
  abs = omap abs
  signum = omap signum
  fromInteger = errorWithStackTrace "fromInteger: not implemented"

mul :: CxSparse a => Matrix a -> Matrix a -> Matrix a
mul = mul_go where
  {-# NOINLINE mul_go #-}
  mul_go _a _b =
    unsafePerformIO $
    withConstCs _a $ \_a ->
    withConstCs _b $ \_b ->
      cs_multiply _a _b >>= fromCs False

fromTriples :: (Num a, Storable a) => Int -> Int -> [(Int, Int, a)] -> Matrix a
{-# INLINE fromTriples #-}
fromTriples = \nr nc (unzip3 -> (rs, cs, xs)) ->
  let rs' = V.fromList $ map fromIntegral rs
      cs' = V.fromList $ map fromIntegral cs
      xs' = V.fromList xs
  in compress nr nc rs' cs' xs'

(><) :: (Num a, Storable a) => Int -> Int -> [(Int, Int, a)] -> Matrix a
{-# INLINE (><) #-}
(><) = fromTriples

toRows :: (Num a, Storable a) => Matrix a -> Box.Vector (S.Vector a)
{-# INLINE toRows #-}
toRows = toColumns . transpose

ctrans :: (IsReal a, Num a, Storable a) => Matrix a -> Matrix a
{-# INLINE ctrans #-}
ctrans = omap conj . transpose

hermitian :: (Eq a, IsReal a, Num a, Storable a) => Matrix a -> Bool
{-# INLINE hermitian #-}
hermitian m = ctrans m == m

assertEq :: (Eq a, Show a, Storable a) => Matrix a -> Matrix a -> Bool
assertEq a b
  | nRows a /= nRows b = errorWithStackTrace "assertEq: nRows differ"
  | nColumns a /= nColumns b = errorWithStackTrace "assertEq: nColumns differ"
  | columnPointers a /= columnPointers b =
      errorWithStackTrace "assertEq: columnPointers differ"
  | rowIndices a /= rowIndices b =
      errorWithStackTrace "assertEq: rowIndices differ"
  | values a /= values b =
      errorWithStackTrace "assertEq: values differ"
  | otherwise = True

lin :: (Num a, Storable a) => a -> Matrix a -> a -> Matrix a -> Matrix a
{-# INLINE lin #-}
lin a matA b matB
  | nRows matA /= nRows matB =
      errorWithStackTrace "lin: row dimensions differ"
  | nColumns matA /= nColumns matB =
      errorWithStackTrace "lin: column dimensions differ"
  | otherwise = runST $ do
      let nc = nColumns matA
          nr = nRows matA
      ptrs <- MV.new (nc + 1)
      MV.unsafeWrite ptrs 0 0

      let nz = nonZero matA + nonZero matB
      rows <- MV.new nz
      vals <- MV.new nz

      U.forM_ (U.enumFromN 0 nc) $ \c -> do
        let colA = column matA c
            colB = column matB c
            lenA = V.length $ S.values colA
            lenB = V.length $ S.values colB
            rowsA = S.indices colA
            rowsB = S.indices colB
            valsA = S.values colA
            valsB = S.values colB

            dedupCopy2 !ixA !ixB !ix =
              if ixA < lenA
                then if ixB < lenB
                  then do
                    rA <- V.unsafeIndexM rowsA ixA
                    rB <- V.unsafeIndexM rowsB ixB
                    case compare rA rB of
                     LT -> do
                       MV.unsafeWrite rows ix rA
                       x <- V.unsafeIndexM valsA ixA
                       MV.unsafeWrite vals ix $! a * x
                       dedupCopy2 (ixA + 1) ixB (ix + 1)
                     EQ -> do
                       MV.unsafeWrite rows ix rA
                       xA <- V.unsafeIndexM valsA ixA
                       xB <- V.unsafeIndexM valsB ixB
                       MV.unsafeWrite vals ix $! a * xA + b * xB
                       dedupCopy2 (ixA + 1) (ixB + 1) (ix + 1)
                     GT -> do
                       MV.unsafeWrite rows ix rB
                       x <- V.unsafeIndexM valsB ixB
                       MV.unsafeWrite vals ix $! b * x
                       dedupCopy2 ixA (ixB + 1) (ix + 1)
                else do
                  let len' = lenA - ixA
                  V.copy (MV.slice ix len' rows) (V.slice ixA len' rowsA)
                  V.copy (MV.slice ix len' vals) (V.slice ixA len' valsA)
                  return $! ix + len'
              else do
                let len' = lenB - ixB
                V.copy (MV.slice ix len' rows) (V.slice ixB len' rowsB)
                V.copy (MV.slice ix len' vals) (V.slice ixB len' valsB)
                return $! ix + len'

        off <- fromIntegral <$> MV.unsafeRead ptrs c
        off' <- fromIntegral <$> dedupCopy2 0 0 off
        MV.unsafeWrite ptrs (c + 1) off'

      columnPointers <- V.unsafeFreeze ptrs
      let nz' = fromIntegral $ V.last columnPointers
          nColumns = nc
          nRows = nr
      rowIndices <- V.unsafeFreeze $ MV.slice 0 nz' rows
      values <- V.unsafeFreeze $ MV.slice 0 nz' vals

      return Matrix {..}

add :: (Num a, Storable a) => Matrix a -> Matrix a -> Matrix a
{-# INLINE add #-}
add a b = lin 1 a 1 b

gaxpy_ :: (Num a, Storable a) => Matrix a -> IOVector a -> IOVector a -> IO ()
{-# INLINE gaxpy_ #-}
gaxpy_ mat@Matrix{..} xs ys =
  V.forM_ (V.enumFromN 0 nColumns) $ \c -> do
    x <- MV.unsafeRead xs c
    S.iforM_ (column mat c) $ \(fromIntegral -> r) a -> do
      y <- MV.unsafeRead ys r
      MV.unsafeWrite ys r $! y + a * x

gaxpy :: (Num a, Storable a) => Matrix a -> Vector a -> Vector a -> Vector a
{-# INLINE gaxpy #-}
gaxpy = gaxpy_go where
  {-# NOINLINE gaxpy_go #-}
  gaxpy_go a _x _y =
    unsafePerformIO $ do
      _y <- V.thaw _y
      _x <- V.unsafeThaw _x
      gaxpy_ a _x _y
      V.unsafeFreeze _y

mulV :: (Num a, Storable a) => Matrix a -> Vector a -> Vector a
{-# INLINE mulV #-}
mulV = mulV_go where
  {-# NOINLINE mulV_go #-}
  mulV_go a _x =
    unsafePerformIO $ do
      _x <- V.unsafeThaw _x
      y <- MV.replicate (MV.length _x) 0
      gaxpy_ a _x y
      V.unsafeFreeze y

hcat :: Storable a => Matrix a -> Matrix a -> Matrix a
hcat a b
  | nRows a /= nRows b = errorWithStackTrace "row dimension mismatch"
  | otherwise = Matrix
      { nRows = nRows a
      , nColumns = nc
      , columnPointers =
        V.init (columnPointers a) V.++ (V.map (+ nza) $ columnPointers b)
      , rowIndices = rowIndices a V.++ rowIndices b
      , values = values a V.++ values b
      }
  where
    nc = nColumns a + nColumns b
    nza = V.last $ columnPointers a

vcat :: (Num a, Storable a) => Matrix a -> Matrix a -> Matrix a
vcat a b
  | nColumns a /= nColumns b = errorWithStackTrace "column dimension mismatch"
  | otherwise = transpose $ hcat (transpose a) (transpose b)

fromBlocks :: (Num a, Storable a) => [[Maybe (Matrix a)]] -> Matrix a
fromBlocks = foldl1 vcat . map (foldl1 hcat) . adjustDims
  where
    adjustDims :: Storable a => [[Maybe (Matrix a)]] -> [[Matrix a]]
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
        heightSpecs = map (map nRows . catMaybes) rows
        widthSpecs = map (map nColumns . catMaybes) cols
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

fromBlocksDiag :: (Num a, Storable a) => [[Maybe (Matrix a)]] -> Matrix a
fromBlocksDiag = fromBlocks . zipWith rejoin [0..] . List.transpose where
  rejoin = \n as -> let (rs, ls) = splitAt (length as - n) as in ls ++ rs

kronecker :: (Num a, Storable a) => Matrix a -> Matrix a -> Matrix a
{-# INLINE kronecker #-}
kronecker matA matB = runST $ do
  let nr = nRows matA * nRows matB
      nc = nColumns matA * nColumns matB
      nz = nonZero matA * nonZero matB

  _ptrs <- MV.new (nc + 1)
  MV.unsafeWrite _ptrs 0 0

  _vals <- MV.new nz
  _ixs <- MV.new nz

  V.forM_ (V.enumFromN 0 $ nColumns matA) $ \cA -> do
    V.forM_ (V.enumFromN 0 $ nColumns matB) $ \cB -> do

      let colA = column matA cA
          colB = column matB cB
          len = V.length $ S.values colB
          c = cA * nColumns matB + cB

      MV.unsafeRead _ptrs c >>= MV.unsafeWrite _ptrs (c + 1)

      S.iforM_ colA $ \rA a -> do
        let rOff = rA * fromIntegral (nRows matB)
            ixs = V.map (+ rOff) $ S.indices colB
            vals = V.map (* a) $ S.values colB

        off <- fromIntegral <$> MV.unsafeRead _ptrs (c + 1)

        V.copy (MV.slice off len _ixs) ixs
        V.copy (MV.slice off len _vals) vals

        MV.unsafeWrite _ptrs (c + 1) $! fromIntegral $ off + len

  _ptrs <- V.unsafeFreeze _ptrs
  _vals <- V.unsafeFreeze _vals
  _ixs <- V.unsafeFreeze _ixs
  return Matrix
    { nRows = nr
    , nColumns = nc
    , columnPointers = _ptrs
    , rowIndices = _ixs
    , values = _vals
    }

takeDiag :: (Num a, Storable a) => Matrix a -> Vector a
{-# INLINE takeDiag #-}
takeDiag = \mat@Matrix{..} ->
  flip V.map (V.enumFromN 0 $ min nRows nColumns) $ \c ->
    let col = column mat c
    in case V.elemIndex (fromIntegral c) (S.indices col) of
      Nothing -> 0
      Just ix -> S.values col V.! ix

diag :: Storable a => Vector a -> Matrix a
diag values = Matrix{..}
  where
    nColumns = V.length values
    nRows = nColumns
    columnPointers = V.iterateN (nRows + 1) (+1) 0
    rowIndices = V.iterateN nColumns (+1) 0

ident :: (Num a, Storable a) => Int -> Matrix a
ident n = diag $ V.replicate n 1

zeros :: Storable a => Int -> Int -> Matrix a
zeros nRows nColumns = Matrix{..}
  where
    columnPointers = V.replicate (nColumns + 1) 0
    rowIndices = V.empty
    values = V.empty
