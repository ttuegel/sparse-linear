{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Numeric.LinearAlgebra.Sparse
    ( CxSparse()
    , Matrix(), nRows, nColumns
    , mul
    , compress
    , transpose, ctrans, hermitian
    , lin
    , add
    , gaxpy, gaxpy_, mulV
    , hcat, vcat, fromBlocks
    , kronecker
    , takeDiag, diag, ident
    , zeros
    , module Data.Complex
    ) where

import Control.Applicative
import Control.Monad (void)
import Data.Complex
import Data.Foldable
import Data.MonoTraversable
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.ForeignPtr.Safe (newForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree)
import Foreign.Marshal.Utils (with)
import Foreign.Storable
import GHC.Stack
import Prelude hiding (any)
import System.IO.Unsafe (unsafePerformIO)

import Data.Matrix.Sparse

mul :: CxSparse a => Matrix a -> Matrix a -> Matrix a
mul _a _b =
  unsafePerformIO $
  withConstCs _a $ \_a ->
  withConstCs _b $ \_b ->
    cs_multiply _a _b >>= fromCs
{-# INLINE mul #-}

compress
  :: (CxSparse a, Unbox a)
  => Int -> Int -> U.Vector (Int, Int, a) -> Matrix a
compress nr nc (U.unzip3 -> (rs, cs, xs)) =
  unsafePerformIO $
  withConstTriples nr nc (V.convert rs) (V.convert cs) (V.convert xs) $ \pcs ->
    cs_compress pcs >>= fromCs
{-# INLINE compress #-}

transpose :: CxSparse a => Matrix a -> Matrix a
transpose mat =
  unsafePerformIO $
  withConstCs mat $ \cs ->
    cs_transpose cs (V.length $ values mat) >>= fromCs
{-# INLINE transpose #-}

ctrans
  :: (CxSparse (Complex a), RealFloat a)
  => Matrix (Complex a) -> Matrix (Complex a)
ctrans = omap conjugate . transpose
{-# INLINE ctrans #-}

hermitian :: (CxSparse (Complex a), RealFloat a) => Matrix (Complex a) -> Bool
hermitian m = ctrans m == m
{-# INLINE hermitian #-}

lin :: CxSparse a => a -> Matrix a -> a -> Matrix a -> Matrix a
lin _alpha _a _beta _b =
  unsafePerformIO $
  withConstCs _a $ \_a ->
  withConstCs _b $ \_b ->
  with _alpha $ \_alpha ->
  with _beta $ \_beta ->
    cs_add _a _b _alpha _beta >>= fromCs
{-# INLINE lin #-}

add :: (CxSparse a, Num a) => Matrix a -> Matrix a -> Matrix a
add a b = lin 1 a 1 b
{-# INLINE add #-}

gaxpy_ :: (CxSparse a) => Matrix a -> IOVector a -> IOVector a -> IO ()
gaxpy_ _a _x _y =
  withConstCs _a $ \_a ->
  MV.unsafeWith _x $ \_x ->
  MV.unsafeWith _y $ \_y ->
    void $ cs_gaxpy _a _x _y
{-# INLINE gaxpy_ #-}

gaxpy :: CxSparse a => Matrix a -> Vector a -> Vector a -> Vector a
gaxpy a _x _y =
  unsafePerformIO $ do
    _y <- V.thaw _y
    _x <- V.unsafeThaw _x
    gaxpy_ a _x _y
    V.unsafeFreeze _y
{-# INLINE gaxpy #-}

mulV :: (CxSparse a, Num a) => Matrix a -> Vector a -> Vector a
mulV a _x =
  unsafePerformIO $ do
    _x <- V.unsafeThaw _x
    y <- MV.replicate (MV.length _x) 0
    gaxpy_ a _x y
    V.unsafeFreeze y
{-# INLINE mulV #-}

hcat :: Storable a => [Matrix a] -> Matrix a
hcat mats
  | null mats = errorWithStackTrace "no matrices"
  | any ((/= nr) . nRows) mats = errorWithStackTrace "row dimensions differ"
  | otherwise = Matrix
      { nRows = nr
      , nColumns = foldl' (+) 0 $ map nColumns mats
      , columnPointers = V.concat $ do
          let colps = map columnPointers mats
          (cps, off) <- zip colps $ 0 : map V.last colps
          return $ V.map (+ off) cps
      , rowIndices = V.concat $ map rowIndices mats
      , values = V.concat $ map values mats
      }
  where
    nr = nRows $ head mats
{-# INLINE hcat #-}

vcat :: (CxSparse a, Storable a) => [Matrix a] -> Matrix a
vcat = transpose . hcat . map transpose
{-# INLINE vcat #-}

fromBlocks :: (CxSparse a, Storable a) => [[Matrix a]] -> Matrix a
fromBlocks = vcat . map hcat
{-# INLINE fromBlocks #-}

kronecker :: CxSparse a => Matrix a -> Matrix a -> Matrix a
kronecker _a _b =
  unsafePerformIO $
  withConstCs _a $ \_a ->
  withConstCs _b $ \_b ->
    cs_kron _a _b >>= fromCs
{-# INLINE kronecker #-}

takeDiag :: CxSparse a => Matrix a -> Vector a
takeDiag _a@Matrix{..} =
  unsafePerformIO $
  withConstCs _a $ \_a ->
  V.unsafeFromForeignPtr0
    <$> (cs_diag _a >>= newForeignPtr finalizerFree)
    <*> pure (min nRows nColumns)
{-# INLINE takeDiag #-}

diag :: Storable a => Vector a -> Matrix a
diag values = Matrix{..}
  where
    nColumns = V.length values
    nRows = nColumns
    columnPointers = V.iterateN (nRows + 1) (+1) 0
    rowIndices = V.iterateN nColumns (+1) 0
{-# INLINE diag #-}

ident :: (Num a, Storable a) => Int -> Matrix a
ident n = diag $ V.replicate n 1
{-# INLINE ident #-}

zeros :: (Num a, Storable a) => Int -> Int -> Matrix a
zeros nRows nColumns = Matrix{..}
  where
    columnPointers = V.replicate (nColumns + 1) 0
    rowIndices = V.empty
    values = V.empty
{-# INLINE zeros #-}
