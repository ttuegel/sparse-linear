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
    , hjoin, vjoin, hcat, vcat, fromBlocks, fromBlocksDiag
    , kronecker
    , takeDiag, diag, ident
    , zeros
    , module Data.Complex.Enhanced
    ) where

import Control.Applicative
import Control.Monad (void, zipWithM_)
import Control.Monad.ST (runST)
import Data.Foldable
import qualified Data.List as List
import Data.Maybe
import Data.MonoTraversable
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.ForeignPtr.Safe (newForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree)
import Foreign.Marshal.Utils (with)
import Foreign.Storable
import GHC.Stack
import Prelude hiding (any, foldl1, foldr1)
import System.IO.Unsafe (unsafePerformIO)

import Debug.Trace (traceShow)

import Data.Complex.Enhanced
import Data.Matrix.Sparse
import qualified Data.Vector.Sparse as SpV

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

fromTriples :: CxSparse a => Int -> Int -> [(Int, Int, a)] -> Matrix a
fromTriples = fromTriples_go where
  {-# NOINLINE fromTriples_go #-}
  fromTriples_go nr nc (unzip3 -> (_rs, _cs, _xs)) =
    unsafePerformIO $ do
      _rs <- return $ V.fromList $ map fromIntegral _rs
      _cs <- return $ V.fromList $ map fromIntegral _cs
      _xs <- return $ V.fromList _xs
      return $ compress nr nc _rs _cs _xs

(><) :: CxSparse a => Int -> Int -> [(Int, Int, a)] -> Matrix a
(><) = fromTriples

toRows :: CxSparse a => Matrix a -> [SpV.Vector a]
toRows = toColumns . transpose

ctrans :: (Num a, IsReal a, Storable a) => Matrix a -> Matrix a
ctrans = omap conj . transpose

hermitian :: (Eq a, IsReal a, CxSparse a) => Matrix a -> Bool
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
      traceShow (values a)
      $ traceShow (values b)
      $ errorWithStackTrace "assertEq: values differ"
  | otherwise = True

lin :: CxSparse a => a -> Matrix a -> a -> Matrix a -> Matrix a
lin = lin_go where
  {-# NOINLINE lin_go #-}
  lin_go _alpha _a _beta _b =
    unsafePerformIO $
    withConstCs _a $ \_a ->
    withConstCs _b $ \_b ->
    with _alpha $ \_alpha ->
    with _beta $ \_beta ->
      cs_add _a _b _alpha _beta >>= fromCs True

add :: CxSparse a => Matrix a -> Matrix a -> Matrix a
add a b = lin 1 a 1 b

gaxpy_ :: CxSparse a => Matrix a -> IOVector a -> IOVector a -> IO ()
gaxpy_ _a _x _y =
  withConstCs _a $ \_a ->
  MV.unsafeWith _x $ \_x ->
  MV.unsafeWith _y $ \_y ->
    void $ cs_gaxpy _a _x _y

gaxpy :: CxSparse a => Matrix a -> Vector a -> Vector a -> Vector a
gaxpy = gaxpy_go where
  {-# NOINLINE gaxpy_go #-}
  gaxpy_go a _x _y =
    unsafePerformIO $ do
      _y <- V.thaw _y
      _x <- V.unsafeThaw _x
      gaxpy_ a _x _y
      V.unsafeFreeze _y

mulV :: CxSparse a => Matrix a -> Vector a -> Vector a
mulV = mulV_go where
  {-# NOINLINE mulV_go #-}
  mulV_go a _x =
    unsafePerformIO $ do
      _x <- V.unsafeThaw _x
      y <- MV.replicate (MV.length _x) 0
      gaxpy_ a _x y
      V.unsafeFreeze y

hjoin :: Storable a => Matrix a -> Matrix a -> Matrix a
hjoin a b
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

vjoin :: CxSparse a => Matrix a -> Matrix a -> Matrix a
vjoin a b
  | nColumns a /= nColumns b = errorWithStackTrace "column dimension mismatch"
  | otherwise = transpose $ hjoin (transpose a) (transpose b)

hcat :: Storable a => [Matrix a] -> Matrix a
hcat matrices
  | null matrices = errorWithStackTrace "hcat: empty list"
  | any ((/= nr) . nRows) matrices =
      errorWithStackTrace "hcat: row dimension mismatch"
  | otherwise =
      Matrix
      { nRows = nr
      , nColumns = foldl' (+) 0 $ map nColumns matrices
      , columnPointers = V.scanl' (+) 0 $ V.concat $ map columnLengths matrices
      , rowIndices = V.concat $ map rowIndices matrices
      , values = V.concat $ map values matrices
      }
  where
    nr = nRows $ head matrices

vcat :: CxSparse a => [Matrix a] -> Matrix a
vcat matrices
  | null matrices = errorWithStackTrace "vcat: empty list"
  | any ((/= nc) . nColumns) matrices =
      errorWithStackTrace "vcat: column dimension mismatch"
  | otherwise = runST $ do
      let colPtrs =
            V.scanl' (+) 0  -- prefix sum to compute result column pointers
            $ foldl1 (V.zipWith (+))  -- lengths of columns in result
            $ map columnLengths matrices  -- lengths of columns in each matrix

      rows <- MV.new nz
      vals <- MV.new nz
      offs <- V.thaw $ V.map fromIntegral $ V.slice 0 nc colPtrs

      let copyMat mat roff = do
            let rs = V.map (+ roff) $ rowIndices mat
                xs = values mat
                ptrs = columnPointers mat
            V.forM_ (V.enumFromN 0 nc) $ \c -> do
              off <- MV.unsafeRead offs c
              start <- fromIntegral <$> V.unsafeIndexM ptrs c
              end <- fromIntegral <$> V.unsafeIndexM ptrs (c + 1)
              let len = end - start
              V.copy (MV.slice off len rows) (V.slice start len rs)
              V.copy (MV.slice off len vals) (V.slice start len xs)
              MV.unsafeWrite offs c $! off + len
      zipWithM_ copyMat matrices roffs

      let nRows = nr
          nColumns = nc
          columnPointers = colPtrs
      values <- V.unsafeFreeze vals
      rowIndices <- V.unsafeFreeze rows

      return Matrix {..}
  where
    nr = foldl' (+) 0 $ map nRows matrices
    roffs = scanl (+) 0 $ map (fromIntegral . nRows) matrices
    nc = nColumns $ head matrices
    nz = foldl' (+) 0 $ map (V.length . values) matrices

fromBlocks :: CxSparse a => [[Maybe (Matrix a)]] -> Matrix a
fromBlocks = vcat . map hcat . fixDimsByRow

fixDimsByRow :: Storable a => [[Maybe (Matrix a)]] -> [[Matrix a]]
fixDimsByRow rows = do
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

fromBlocksDiag :: CxSparse a => [[Maybe (Matrix a)]] -> Matrix a
fromBlocksDiag = fromBlocks . zipWith rejoin [0..] . List.transpose where
  rejoin = \n as -> let (rs, ls) = splitAt (length as - n) as in ls ++ rs

kronecker :: CxSparse a => Matrix a -> Matrix a -> Matrix a
kronecker = kronecker_go where
  {-# NOINLINE kronecker_go #-}
  kronecker_go _a _b =
    unsafePerformIO $
    withConstCs _a $ \_a ->
    withConstCs _b $ \_b ->
      cs_kron _a _b >>= fromCs False

takeDiag :: CxSparse a => Matrix a -> Vector a
takeDiag = takeDiag_go where
  {-# NOINLINE takeDiag_go #-}
  takeDiag_go _a@Matrix{..} =
    unsafePerformIO $
    withConstCs _a $ \_a ->
      V.unsafeFromForeignPtr0
      <$> (cs_diag _a >>= newForeignPtr finalizerFree)
      <*> pure (min nRows nColumns)

diag :: Storable a => Vector a -> Matrix a
diag values = Matrix{..}
  where
    nColumns = V.length values
    nRows = nColumns
    columnPointers = V.iterateN (nRows + 1) (+1) 0
    rowIndices = V.iterateN nColumns (+1) 0

ident :: (Num a, Storable a) => Int -> Matrix a
ident n = diag $ V.replicate n 1

zeros :: (Storable a) => Int -> Int -> Matrix a
zeros nRows nColumns = Matrix{..}
  where
    columnPointers = V.replicate (nColumns + 1) 0
    rowIndices = V.empty
    values = V.empty
