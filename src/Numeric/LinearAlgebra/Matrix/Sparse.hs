{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Numeric.LinearAlgebra.Matrix.Sparse where

import Control.Loop.For
import Control.Monad (void)
import Control.Monad.ST (runST)
import Data.Foldable
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.Marshal.Utils (with)
import Foreign.Storable
import GHC.Stack
import Prelude hiding (any)
import System.IO.Unsafe (unsafePerformIO)

import Numeric.LinearAlgebra.Matrix.Sparse.Internal

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
      $ \cs -> cs_compress cs >>= peek >>= fromCs
{-# INLINE compress #-}

transpose :: CxSparse a => Matrix a -> Matrix a
transpose a = unsafePerformIO $
    unsafeWithMatrix a $ \cs ->
        cs_transpose cs (V.length $ vals a) >>= peek >>= fromCs
{-# INLINE transpose #-}

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
gaxpy a x y = unsafePerformIO $
    unsafeWithMatrix a $ \csa ->
    V.unsafeWith x $ \px -> do
        y_ <- V.thaw y
        MV.unsafeWith y_ $ \py -> void $ cs_gaxpy csa px py
        V.unsafeFreeze y_
{-# INLINE gaxpy #-}

mulV :: (CxSparse a, Num a) => Matrix a -> Vector a -> Vector a
mulV a x = unsafePerformIO $
    unsafeWithMatrix a $ \csa ->
    V.unsafeWith x $ \px -> do
        y <- MV.replicate (V.length x) 0
        MV.unsafeWith y $ \py -> void $ cs_gaxpy csa px py
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

colSlice :: Int -> Matrix a -> ForI Int
colSlice c Matrix{..} = forI (colps V.! c) (colps V.! (c + 1))
{-# INLINE colSlice #-}

kronecker
  :: (CxSparse a, Num a, Storable a, Unbox a)
  => Matrix a -> Matrix a -> Matrix a
kronecker a b = runST $ do
    let nc = ncols a * ncols b
        nr = nrows a * nrows b
        nz = V.length (vals a) * V.length (vals b)
    colps_ <- MV.replicate 0 (nc + 1)
    rowixs_ <- MV.new nz
    vals_ <- MV.new nz
    pix <- newSTRef 0
    pcc <- newSTRef 0
    let acols = forI 0 (ncols a)
        bcols = forI 0 (ncols b)
    forM_ acols $ \ca -> forM_ bcols $ \cb -> do
        let aixs = colSlice ca a
            bixs = colSlice cb b
        forM_ aixs $ \aix -> forM_ bixs $ \bix -> do
            ix <- readSTRef pix
            ra <- V.unsafeIndexM (rowixs a) aix
            rb <- V.unsafeIndexM (rowixs b) bix
            MV.unsafeWrite rowixs_ ix $! ra * rb
            xa <- V.unsafeIndexM (vals a) aix
            xb <- V.unsafeIndexM (vals b) aix
            MV.unsafeWrite vals_ ix $! xa * xb
            modifySTRef' pix (+ 1)
        modifySTRef' pcc (+ 1)
        ix <- readSTRef pix
        cc <- readSTRef pcc
        MV.unsafeWrite colps_ cc ix
    colps__ <- V.unsafeFreeze colps_
    rowixs__ <- V.unsafeFreeze rowixs_
    vals__ <- V.unsafeFreeze vals_
    return Matrix
        { nrows = nr
        , ncols = nc
        , colps = colps__
        , rowixs = rowixs__
        , vals = vals__
        }
{-# INLINE kronecker #-}
