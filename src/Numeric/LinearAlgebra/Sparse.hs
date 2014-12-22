{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Numeric.LinearAlgebra.Sparse
    ( CxSparse()
    , Matrix(nrows, ncols)
    , mul
    , compress
    , transpose, ctrans
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
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.ForeignPtr.Safe (newForeignPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Storable
import GHC.Stack
import Prelude hiding (any)
import System.IO.Unsafe (unsafePerformIO)

import Numeric.LinearAlgebra.Matrix.Sparse.Internal
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
        cs_transpose cs (V.length $ vals a) >>= peek >>= fromCs
{-# INLINE transpose #-}

ctrans
  :: (CxSparse (Complex a), RealFloat a)
  => Matrix (Complex a) -> Matrix (Complex a)
ctrans = cmap conjugate . transpose
{-# INLINE ctrans #-}

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

gaxpy :: CxSparse a => Matrix a -> Vector a -> Vector a -> Vector a
gaxpy a x y = unsafePerformIO $ do
    y_ <- V.thaw y
    gaxpy_ a x y_
    V.unsafeFreeze y_
{-# INLINE gaxpy #-}

gaxpy_ :: CxSparse a => Matrix a -> Vector a -> IOVector a -> IO ()
gaxpy_ a x y =
    unsafeWithMatrix a $ \csa ->
    V.unsafeWith x $ \px ->
    MV.unsafeWith y $ \py ->
        void $ cs_gaxpy csa px py
{-# INLINE gaxpy_ #-}

mulV :: (CxSparse a, Num a) => Matrix a -> Vector a -> Vector a
mulV a x = unsafePerformIO $ do
    y <- MV.replicate (V.length x) 0
    gaxpy_ a x y
    V.unsafeFreeze y
{-# INLINE mulV #-}

cmap :: (Storable a, Storable b) => (a -> b) -> Matrix a -> Matrix b
cmap = \f mat -> mat { vals = V.map f $ vals mat }
{-# INLINE cmap #-}

scale :: (Num a, Storable a) => a -> Matrix a -> Matrix a
scale = \x -> cmap (* x)
{-# INLINE scale #-}

hcat :: Storable a => [Matrix a] -> Matrix a
hcat mats
  | null mats = errorWithStackTrace "no matrices"
  | any ((/= nr) . nrows) mats =
      errorWithStackTrace "matrices must have the same number of rows"
  | otherwise = Matrix
      { nrows = nr
      , ncols = foldl' (+) 0 $ map ncols mats
      , colps = V.concat $ do
          let colpss = map colps mats
          (cps, off) <- zip colpss $ 0 : map V.last colpss
          return $ V.map (+ off) cps
      , rowixs = V.concat $ map rowixs mats
      , vals = V.concat $ map vals mats
      }
  where
    nr = nrows $ head mats
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
    nd = min nrows ncols
{-# INLINE takeDiag #-}

diag :: Storable a => Vector a -> Matrix a
diag vals = Matrix{..}
  where
    ncols = V.length vals
    nrows = ncols
    colps = V.iterateN (nrows + 1) (+1) 0
    rowixs = V.iterateN ncols (+1) 0
{-# INLINE diag #-}

ident :: (Num a, Storable a) => Int -> Matrix a
ident n = diag $ V.replicate n 1
{-# INLINE ident #-}

zeros :: (Num a, Storable a) => Int -> Int -> Matrix a
zeros nrows ncols = Matrix{..}
  where
    colps = V.replicate (ncols + 1) 0
    rowixs = V.empty
    vals = V.empty
{-# INLINE zeros #-}
