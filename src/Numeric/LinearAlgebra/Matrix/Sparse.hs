{-# LANGUAGE ViewPatterns #-}

module Numeric.LinearAlgebra.Matrix.Sparse where

import Control.Monad (void)
import Data.Foldable
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as Unbox
import Data.Vector.Storable (Vector)
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
  => Int -> Int -> Unbox.Vector (Int, Int, a) -> Matrix a
compress nr nc (Unbox.unzip3 -> (rs, cs, xs)) = unsafePerformIO $
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

kronecker
  :: (CxSparse a, Num a, Storable a, Unbox a)
  => Matrix a -> Matrix a -> Matrix a
kronecker a b = compress nr nc $ Unbox.fromList $ do
    ca <- [0..(ncols a - 1)]
    cb <- [0..(ncols b - 1)]
    let cc = ca * cb
        aStart = (colps a) V.! ca
        aEnd = (colps a) V.! (ca + 1)
    aix <- [aStart..(aEnd - 1)]
    let bStart = (colps b) V.! cb
        bEnd = (colps b) V.! (cb + 1)
    bix <- [bStart..(bEnd - 1)]
    let ra = (rowixs a) V.! aix
        rb = (rowixs b) V.! bix
        rc = ra * rb
        xa = (vals a) V.! aix
        xb = (vals b) V.! bix
        xc = xa * xb
    return (rc, cc, xc)
  where
    nc = ncols a * ncols b
    nr = nrows a * nrows b
{-# INLINE kronecker #-}
