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
    , cmap, scale
    , hcat, vcat, fromBlocks
    , kronecker
    , takeDiag, diag, ident
    , zeros
    , module Data.Complex
    ) where

import Control.Monad (void)
import Data.Complex
import Data.Foldable
import Data.Vector.Mutability
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.ForeignPtr.Safe (newForeignPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Storable
import GHC.Stack
import Prelude hiding (any)
import System.IO.Unsafe (unsafePerformIO)

import Data.Matrix.Sparse
import Numeric.LinearAlgebra.Sparse.Internal

mul :: CxSparse a => Matrix a -> Matrix a -> Matrix a
mul a b = unsafePerformIO $
    unsafeWithMatrix a $ \csa ->
    unsafeWithMatrix b $ \csb ->
        cs_multiply csa csb >>= peek >>= fromCs
{-# INLINE mul #-}

compress
  :: (CxSparse a, Unbox a)
  => Int -> Int -> U.Vector (Int, Int, a) -> Matrix a
compress nr nc (U.unzip3 -> (rs, cs, xs)) = unsafePerformIO $
    unsafeWithTriples nr nc (V.convert rs) (V.convert cs) (V.convert xs)
      $ \pcs -> cs_compress pcs >>= peek >>= fromCs
{-# INLINE compress #-}

transpose :: CxSparse a => Matrix a -> Matrix a
transpose a = unsafePerformIO $
    unsafeWithMatrix a $ \cs ->
        cs_transpose cs (V.length $ values a) >>= peek >>= fromCs
{-# INLINE transpose #-}

ctrans
  :: (CxSparse (Complex a), RealFloat a)
  => Matrix (Complex a) -> Matrix (Complex a)
ctrans = cmap conjugate . transpose
{-# INLINE ctrans #-}

hermitian :: (CxSparse (Complex a), RealFloat a) => Matrix (Complex a) -> Bool
hermitian m = ctrans m == m
{-# INLINE hermitian #-}

lin :: CxSparse a => a -> Matrix a -> a -> Matrix a -> Matrix a
lin alpha a beta b = unsafePerformIO $
    unsafeWithMatrix a $ \csa ->
    unsafeWithMatrix b $ \csb ->
    with alpha $ \al ->
    with beta $ \bt ->
        cs_add csa csb al bt >>= peek >>= fromCs
{-# INLINE lin #-}

add :: (CxSparse a, Num a) => Matrix a -> Matrix a -> Matrix a
add a b = lin 1 a 1 b
{-# INLINE add #-}

gaxpy_ :: (CxSparse a, WithImm v, WithMut w) => Matrix a -> v a -> w a -> IO (w a)
gaxpy_ _a _x _y =
  withMut _y $ \_y ->
  unsafeWithMatrix _a $ \_a ->
  withImm _x $ \_x ->
    void $ cs_gaxpy _a _x _y
{-# INLINE gaxpy_ #-}

gaxpy :: CxSparse a => Matrix a -> Vector a -> Vector a -> Vector a
gaxpy a x y = unsafePerformIO $ gaxpy_ a x y
{-# INLINE gaxpy #-}

mulV :: (CxSparse a, Num a) => Matrix a -> Vector a -> Vector a
mulV a x = unsafePerformIO $ do
    y <- MV.replicate (V.length x) 0
    _ <- gaxpy_ a x y
    V.unsafeFreeze y
{-# INLINE mulV #-}

cmap :: (Storable a, Storable b) => (a -> b) -> Matrix a -> Matrix b
cmap = \f mat -> mat { values = V.map f $ values mat }
{-# INLINE cmap #-}

scale :: (Num a, Storable a) => a -> Matrix a -> Matrix a
scale = \x -> cmap (* x)
{-# INLINE scale #-}

hcat :: Storable a => [Matrix a] -> Matrix a
hcat mats
  | null mats = errorWithStackTrace "no matrices"
  | any ((/= nr) . nRows) mats =
      errorWithStackTrace "matrices must have the same number of rows"
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
kronecker a b = unsafePerformIO $
    unsafeWithMatrix a $ \csa ->
    unsafeWithMatrix b $ \csb ->
        cs_kron csa csb >>= peek >>= fromCs
{-# INLINE kronecker #-}

takeDiag :: CxSparse a => Matrix a -> Vector a
takeDiag a@Matrix{..} = unsafePerformIO $
    unsafeWithMatrix a $ \csa -> do
        d <- cs_diag csa >>= newForeignPtr cs_free
        return $ V.unsafeFromForeignPtr0 d nd
  where
    nd = min nRows nColumns
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
