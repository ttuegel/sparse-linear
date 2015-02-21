{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Matrix.Sparse
       ( Matrix(..), cmap, nonZero, slice, cdim, rdim
       , OrientK(..), Orient(..), Transpose, orient
       , compress, decompress, dedupInPlace
       , fromTriples, (><)
       , transpose, reorient
       , ctrans, hermitian
       , outer
       , gaxpy_, gaxpy, mulV
       , lin, add
       , mjoin, hjoin, vjoin
       , mcat, hcat, vcat
       , fromBlocks, fromBlocksDiag
       , kronecker
       , takeDiag, diag
       , ident, zeros
       , module Data.Complex.Enhanced
       ) where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import qualified Data.Foldable as F
import Data.Function (fix)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Monoid ((<>), Monoid(..), First(..))
import Data.MonoTraversable (Element, MonoFoldable(..), MonoFunctor(..))
import Data.Ord (comparing)
import Data.Proxy
import Data.Tuple (swap)
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Fusion.Stream as S
import Data.Vector.Fusion.Stream.Size
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Stack (errorWithStackTrace)

import Data.Complex.Enhanced
import Data.Matrix.Sparse.Lin
import Data.Matrix.Sparse.Mul
import Data.Matrix.Sparse.Slice
import qualified Data.Vector.Sparse as S
import Data.Vector.Util

data OrientK = Row | Col

class Orient (or :: OrientK) where
  -- | Switch between (major, minor) and (row, column) index representations
  orientSwap :: Proxy or -> (a, a) -> (a, a)

instance Orient Row where
  {-# INLINE orientSwap #-}
  orientSwap = \Proxy -> id

instance Orient Col where
  {-# INLINE orientSwap #-}
  orientSwap = \Proxy -> swap

type family Transpose (or :: OrientK) where
  Transpose Row = Col
  Transpose Col = Row

-- | Matrix in compressed sparse column (CSC) format.
data Matrix (or :: OrientK) a = Matrix
  { odim :: !Int -- ^ outer dimension (number of slices)
  , idim :: !Int -- ^ inner dimension (dimension of each slice)
  , pointers :: !(Vector Int)
                -- ^ starting index of each slice,
                -- last element is non-zero length
  , entries :: !(Vector (Int, a))
  }
  deriving (Eq, Read, Show)

cdim, rdim :: Orient or => Matrix or a -> Int
cdim mat@Matrix{..} = snd (orientSwap (orient mat) (odim, idim))
rdim mat@Matrix{..} = fst (orientSwap (orient mat) (odim, idim))

type instance Element (Matrix or a) = a

instance Unbox a => MonoFunctor (Matrix or a) where
  {-# INLINE omap #-}
  omap = \f mat ->
    let (indices, values) = U.unzip $ entries mat
    in mat { entries = U.zip indices $ U.map f values }

instance Unbox a => MonoFoldable (Matrix or a) where
  {-# INLINE ofoldMap #-}
  {-# INLINE ofoldr #-}
  {-# INLINE ofoldl' #-}
  {-# INLINE ofoldr1Ex #-}
  {-# INLINE ofoldl1Ex' #-}
  ofoldMap = \f Matrix{..} -> ofoldMap f $ snd $ U.unzip entries
  ofoldr = \f r Matrix{..} -> U.foldr f r $ snd $ U.unzip entries
  ofoldl' = \f r Matrix{..} -> U.foldl' f r $ snd $ U.unzip entries
  ofoldr1Ex = \f Matrix{..} -> U.foldr1 f $ snd $ U.unzip entries
  ofoldl1Ex' = \f Matrix{..} -> U.foldl1' f $ snd $ U.unzip entries

instance (Num a, Unbox a) => Num (Matrix Col a) where
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE negate #-}
  {-# INLINE abs #-}
  {-# INLINE signum #-}
  (+) = add
  (-) = \a b -> lin 1 a (-1) b
  (*) = \a b ->
    if odim a /= idim b
      then errorWithStackTrace "(*): inner dimension mismatch"
    else
      let (ptrs, ents) =
            unsafeMul (idim a) (odim b)
              (pointers a) (entries a)
              (pointers b) (entries b)
      in Matrix
      { odim = odim b
      , idim = idim a
      , pointers = ptrs
      , entries = ents
      }
  negate = omap negate
  abs = omap abs
  signum = omap signum
  fromInteger = errorWithStackTrace "fromInteger: not implemented"

instance (Num a, Unbox a) => Num (Matrix Row a) where
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE negate #-}
  {-# INLINE abs #-}
  {-# INLINE signum #-}
  (+) = add
  (-) = \a b -> lin 1 a (-1) b
  (*) = \a b ->
    if idim a /= odim b
      then errorWithStackTrace "(*): inner dimension mismatch"
    else
      let (ptrs, ents) = unsafeMul (idim b) (odim a)
                         (pointers b) (entries b)
                         (pointers a) (entries a)
      in Matrix
      { odim = odim a
      , idim = idim b
      , pointers = ptrs
      , entries = ents
      }
  negate = omap negate
  abs = omap abs
  signum = omap signum
  fromInteger = errorWithStackTrace "fromInteger: not implemented"

orient :: Matrix or a -> Proxy or
{-# INLINE orient #-}
orient = \_ -> Proxy

nonZero :: Unbox a => Matrix or a -> Int
{-# INLINE nonZero #-}
nonZero = \Matrix{..} -> U.last pointers

cmap :: (Unbox a, Unbox b) => (a -> b) -> Matrix or a -> Matrix or b
{-# INLINE cmap #-}
cmap = \f m ->
  let (indices, values) = U.unzip $ entries m
  in m { entries = U.zip indices $ U.map f values }

slice :: Unbox a => Matrix or a -> Int -> S.Vector a
{-# INLINE slice #-}
slice = \Matrix{..} c ->
  S.Vector
  { S.dim = idim
  , S.entries = unsafeSlice pointers c entries
  }

compress
  :: (Orient or, Num a, Unbox a)
  => Int  -- ^ number of rows
  -> Int  -- ^ number of columns
  -> Vector (Int, Int, a)  -- ^ (row, column, value)
  -> Matrix or a
{-# SPECIALIZE compress :: Int -> Int -> Vector (Int, Int, Double) -> Matrix Row Double #-}
{-# SPECIALIZE compress :: Int -> Int -> Vector (Int, Int, Double) -> Matrix Col Double #-}
{-# SPECIALIZE compress :: Int -> Int -> Vector (Int, Int, Complex Double) -> Matrix Row (Complex Double) #-}
{-# SPECIALIZE compress :: Int -> Int -> Vector (Int, Int, Complex Double) -> Matrix Col (Complex Double) #-}
compress nRows nColumns _triples = fix $ \mat -> runST $ do
  let (_rows, _cols, _vals) = U.unzip3 _triples
      (odim, idim) = orientSwap (orient mat) (nRows, nColumns)
      (_out, _inn) = orientSwap (orient mat) (_rows, _cols)
      ptrs = computePtrs odim _out

  let checkBounds bound prev ix this =
        prev <> (if this >= 0 && this < bound then mempty else First (Just ix))

  -- check bounds of row indices
  case getFirst (U.ifoldl' (checkBounds nRows) mempty _rows) of
    Nothing -> return ()
    Just ix ->
      errorWithStackTrace
      ("compress: row index out of bounds "
       ++ show (0 :: Int, nRows) ++ " at " ++ show ix)

  -- check bounds of column indices
  case getFirst (U.ifoldl' (checkBounds nColumns) mempty _cols) of
    Nothing -> return ()
    Just ix ->
      errorWithStackTrace
      ("compress: column index out of bounds "
       ++ show (0 :: Int, nColumns) ++ " at " ++ show ix)

  _out <- U.unsafeThaw _out
  _inn <- U.unsafeThaw _inn
  _vals <- U.unsafeThaw _vals
  let _entries = UM.zip _inn _vals

  Intro.sortBy (comparing fst) $ UM.zip _out _entries

  dels <- U.forM (U.enumFromN 0 odim) $ \m ->
    dedupInPlace idim $ unsafeMSlice ptrs m _entries

  let shifts = U.scanl' (+) 0 dels
      pointers = U.zipWith (-) ptrs shifts

  U.forM_ (U.enumFromN 0 odim) $ \m -> do
    shift <- U.unsafeIndexM shifts m
    when (shift > 0) $ do
      start <- U.unsafeIndexM ptrs m
      end <- U.unsafeIndexM ptrs (m + 1)
      let len = end - start
          start' = start - shift
      UM.move
        (UM.unsafeSlice start' len _entries)
        (UM.unsafeSlice start len _entries)

  let nz' = U.last pointers
  entries <- U.unsafeFreeze $ UM.unsafeSlice 0 nz' _entries

  return Matrix{..}

dedupInPlace
  :: (Num a, PrimMonad m, Unbox a)
  => Int -> MVector (PrimState m) (Int, a) -> m Int
{-# INLINE dedupInPlace #-}
dedupInPlace idim _entries = do
  Intro.sortBy (comparing fst) _entries
  let len = UM.length _entries
      (ixs, xs) = UM.unzip _entries
      dedup_go w r del
        | r < len = do
            ixr <- UM.unsafeRead ixs r
            ixw <- UM.unsafeRead ixs w
            if ixr == ixw
              then do
                UM.unsafeWrite ixs r idim
                x <- UM.unsafeRead xs r
                x' <- UM.unsafeRead xs w
                UM.unsafeWrite xs w $! x' + x
                dedup_go w (r + 1) (del + 1)
              else dedup_go r (r + 1) del
        | otherwise = return del
  del <- dedup_go 0 1 0
  Intro.sortBy (comparing fst) _entries
  return del

computePtrs :: Int -> Vector Int -> Vector Int
{-# INLINE computePtrs #-}
computePtrs n indices = runST $ do
  counts <- UM.replicate n 0
  -- scan the indices once, counting the occurrences of each index
  U.forM_ indices $ \ix -> do
    count <- UM.unsafeRead counts ix
    UM.unsafeWrite counts ix $! count + 1
  -- compute the index pointers by prefix-summing the occurrence counts
  U.scanl (+) 0 <$> U.unsafeFreeze counts

decompress :: Vector Int -> Vector Int
{-# INLINE decompress #-}
decompress = \ptrs -> U.create $ do
  indices <- UM.new $ U.last ptrs
  U.forM_ (U.enumFromN 0 $ U.length ptrs - 1) $ \c ->
    UM.set (unsafeMSlice ptrs c indices) c
  return indices

transpose :: Matrix or a -> Matrix (Transpose or) a
{-# INLINE transpose #-}
transpose = \Matrix{..} -> Matrix {..}

reorient :: Unbox a => Matrix (Transpose or) a -> Matrix or a
{-# INLINE reorient #-}
reorient Matrix{..} = runST $ do
  let (indices, values) = U.unzip entries
      nz = U.length values
      ptrs = computePtrs idim indices

  -- re-initialize row counts from row pointers
  count <- U.thaw $ U.unsafeSlice 0 idim ptrs

  _ixs <- UM.new nz
  _xs <- UM.new nz

  -- copy each column into place
  -- "major" and "minor" indices refer to the orientation of the original matrix
  U.forM_ (U.enumFromN 0 odim) $ \m -> do
    U.forM_ (unsafeSlice pointers m entries) $ \(n, x) -> do
      ix <- preincrement count n
      UM.unsafeWrite _ixs ix m
      UM.unsafeWrite _xs ix x

  _ixs <- U.unsafeFreeze _ixs
  _xs <- U.unsafeFreeze _xs

  return Matrix
    { odim = idim
    , idim = odim
    , pointers = ptrs
    , entries = U.zip _ixs _xs
    }

outer
  :: (Orient or, Num a, Unbox a)
  => S.Vector a -- ^ sparse column vector
  -> S.Vector a -- ^ sparse row vector
  -> Matrix or a
{-# INLINE outer #-}
outer = \sliceC sliceR -> fix $ \mat ->
  let -- indices of sliceM are outer (major) indices of result
      -- indices of sliceN are inner (minor) indices of result
      (sliceM, sliceN) = orientSwap (orient mat) (sliceC, sliceR)
      odim = S.dim sliceM
      idim = S.dim sliceN
      (indicesN, valuesN) = U.unzip $ S.entries sliceN
      (indicesM, valuesM) = U.unzip $ S.entries sliceM
      lenM = U.length valuesM
      lenN = U.length valuesN
      lengths = U.create $ do
        lens <- UM.replicate (odim + 1) 0
        U.forM_ indicesM $ \m -> UM.unsafeWrite lens m lenN
        return lens
      pointers = U.scanl' (+) 0 lengths
      indices = U.concat $ replicate lenM indicesN
      values = U.create $ do
        vals <- UM.new (lenM * lenN)
        U.forM_ (S.entries sliceM) $ \(ix, a) ->
          U.copy (unsafeMSlice pointers ix vals) $ U.map (* a) valuesN
        return vals
      entries = U.zip indices values
  in Matrix {..}

fromTriples
  :: (Orient or, Num a, Unbox a)
  => Int -> Int -> [(Int, Int, a)] -> Matrix or a
{-# INLINE fromTriples #-}
fromTriples = \nr nc -> compress nr nc . U.fromList

(><)
  :: (Orient or, Num a, Unbox a)
  => Int -> Int -> [(Int, Int, a)] -> Matrix or a
{-# INLINE (><) #-}
(><) = fromTriples

ctrans
  :: (IsReal a, Num a, Unbox a)
  => Matrix or a -> Matrix (Transpose or) a
{-# INLINE ctrans #-}
ctrans = omap conj . transpose

hermitian :: (Eq a, IsReal a, Num a, Unbox a) => Matrix or a -> Bool
{-# INLINE hermitian #-}
hermitian m = reorient (ctrans m) == m

lin :: (Num a, Unbox a) => a -> Matrix or a -> a -> Matrix or a -> Matrix or a
{-# SPECIALIZE lin :: Double -> Matrix Row Double -> Double -> Matrix Row Double -> Matrix Row Double #-}
{-# SPECIALIZE lin :: Double -> Matrix Col Double -> Double -> Matrix Col Double -> Matrix Col Double #-}
{-# SPECIALIZE lin :: (Complex Double) -> Matrix Row (Complex Double) -> (Complex Double) -> Matrix Row (Complex Double) -> Matrix Row (Complex Double) #-}
{-# SPECIALIZE lin :: (Complex Double) -> Matrix Col (Complex Double) -> (Complex Double) -> Matrix Col (Complex Double) -> Matrix Col (Complex Double) #-}
lin a matA b matB
  | idim matA /= idim matB =
      errorWithStackTrace "lin: inner dimensions differ"
  | odim matA /= odim matB =
      errorWithStackTrace "lin: outer dimensions differ"
  | otherwise =
      let (ptrs, ents) =
            unsafeLin (odim matA) (idim matA)
              a (pointers matA) (entries matA)
              b (pointers matB) (entries matB)
      in Matrix
      { odim = odim matA
      , idim = idim matA
      , pointers = ptrs
      , entries = ents
      }

add :: (Num a, Unbox a) => Matrix or a -> Matrix or a -> Matrix or a
{-# INLINE add #-}
add a b = lin 1 a 1 b

gaxpy_
  :: (GM.MVector v a, Orient or, Num a, PrimMonad m, Unbox a)
  => Matrix or a -> v (PrimState m) a -> v (PrimState m) a -> m ()
{-# INLINE gaxpy_ #-}
gaxpy_ mat@Matrix{..} xs ys
  | GM.length xs /= cdim mat =
      errorWithStackTrace "gaxpy_: matrix column dim does not match operand"
  | GM.length ys /= rdim mat =
      errorWithStackTrace "gaxpy_: matrix row dim does not match result"
  | otherwise =
      U.forM_ (U.enumFromN 0 odim) $ \m -> do
        U.forM_ (unsafeSlice pointers m entries) $ \(n, a) -> do
          let (r, c) = orientSwap (orient mat) (m, n)
          x <- GM.unsafeRead xs c
          y <- GM.unsafeRead ys r
          GM.unsafeWrite ys r $! y + a * x

gaxpy
  :: (G.Vector v a, Orient or, Num a, Unbox a)
  => Matrix or a -> v a -> v a -> v a
{-# INLINE gaxpy #-}
gaxpy = \a _x _y -> runST $ do
  _y <- G.thaw _y
  _x <- G.unsafeThaw _x
  gaxpy_ a _x _y
  G.unsafeFreeze _y

mulV :: (Orient or, Num a, G.Vector v a, Unbox a) => Matrix or a -> v a -> v a
{-# INLINE mulV #-}
mulV = \a _x -> runST $ do
  _x <- G.unsafeThaw _x
  y <- GM.replicate (rdim a) 0
  gaxpy_ a _x y
  G.unsafeFreeze y

mjoin :: Unbox a => Matrix or a -> Matrix or a -> Matrix or a
{-# INLINE mjoin #-}
mjoin a b
  | idim a /= idim b = errorWithStackTrace "mjoin: inner dimension mismatch"
  | otherwise = Matrix
      { odim = dm
      , idim = idim a
      , pointers = U.init (pointers a) U.++ (U.map (+ nza) $ pointers b)
      , entries = entries a U.++ entries b
      }
  where
    dm = odim a + odim b
    nza = nonZero a

mcat :: Unbox a => [Matrix or a] -> Matrix or a
{-# INLINE mcat #-}
mcat mats
  | null mats = errorWithStackTrace "mcat: empty list"
  | any (/= _idim) (map idim mats) =
      errorWithStackTrace "mcat: inner dimension mismatch"
  | otherwise =
      Matrix
      { odim = F.foldl' (+) 0 $ map odim mats
      , idim = _idim
      , pointers = U.scanl' (+) 0 $ U.concat $ map lengths mats
      , entries = U.concat $ map entries mats
      }
  where
    _idim = idim $ head mats
    lengths m = let ptrs = pointers m in U.zipWith (-) (U.tail ptrs) ptrs

hcat :: Unbox a => [Matrix Col a] -> Matrix Col a
{-# INLINE hcat #-}
hcat = mcat

vcat :: Unbox a => [Matrix Row a] -> Matrix Row a
{-# INLINE vcat #-}
vcat = mcat

hjoin :: Unbox a => Matrix Col a -> Matrix Col a -> Matrix Col a
{-# INLINE hjoin #-}
hjoin = mjoin

vjoin :: Unbox a => Matrix Row a -> Matrix Row a -> Matrix Row a
{-# INLINE vjoin #-}
vjoin = mjoin

fromBlocks :: (Num a, Unbox a) => [[Maybe (Matrix Col a)]] -> Matrix Row a
{-# SPECIALIZE fromBlocks :: [[Maybe (Matrix Col Double)]] -> Matrix Row Double #-}
{-# SPECIALIZE fromBlocks :: [[Maybe (Matrix Col (Complex Double))]] -> Matrix Row (Complex Double) #-}
fromBlocks = vcat . map (reorient . hcat) . adjustDims
  where
    adjustDims rows = do
      (r, row) <- zip [0..] rows
      return $ do
        (c, mat) <- zip [0..] row
        return $ case mat of
          Nothing -> zeros (heights U.! r) (widths U.! c)
          Just x -> x
      where
        cols = L.transpose rows
        incompatible = any (\xs -> let x = head xs in any (/= x) xs)
        underspecified = any null
        heightSpecs = map (map idim . catMaybes) rows
        widthSpecs = map (map odim . catMaybes) cols
        heights
          | underspecified heightSpecs =
              errorWithStackTrace "fixDimsByRow: underspecified heights"
          | incompatible heightSpecs =
              errorWithStackTrace "fixDimsByRow: incompatible heights"
          | otherwise = U.fromList $ map head heightSpecs
        widths
          | underspecified widthSpecs =
              errorWithStackTrace "fixDimsByRow: underspecified widths"
          | incompatible widthSpecs =
              errorWithStackTrace "fixDimsByRow: incompatible widths"
          | otherwise = U.fromList $ map head widthSpecs

fromBlocksDiag
  :: (Num a, Unbox a) => [[Maybe (Matrix Col a)]] -> Matrix Row a
{-# INLINE fromBlocksDiag #-}
fromBlocksDiag = fromBlocks . zipWith rejoin [0..] . L.transpose where
  rejoin = \n as -> let (rs, ls) = splitAt (length as - n) as in ls ++ rs

kronecker :: (Num a, Unbox a) => Matrix or a -> Matrix or a -> Matrix or a
{-# SPECIALIZE kronecker :: Matrix Row Double -> Matrix Row Double -> Matrix Row Double #-}
{-# SPECIALIZE kronecker :: Matrix Col Double -> Matrix Col Double -> Matrix Col Double #-}
{-# SPECIALIZE kronecker :: Matrix Row (Complex Double) -> Matrix Row (Complex Double) -> Matrix Row (Complex Double) #-}
{-# SPECIALIZE kronecker :: Matrix Col (Complex Double) -> Matrix Col (Complex Double) -> Matrix Col (Complex Double) #-}
kronecker matA matB =
  let dn = idim matA * idim matB
      dm = odim matA * odim matB
      nz = nonZero matA * nonZero matB

      ptrsA = pointers matA
      ptrsB = pointers matB
      lengthsA = U.zipWith (-) (U.tail ptrsA) ptrsA
      lengthsB = U.zipWith (-) (U.tail ptrsB) ptrsB
      ptrs = U.scanl' (+) 0
             $ G.unstream $ flip S.sized (Exact $ dm + 1)
             $ S.concatMap (\nzA -> S.map (* nzA) $ G.stream lengthsB)
             $ G.stream lengthsA

      (indicesA, valuesA) = U.unzip $ entries matA
      (indicesB, valuesB) = U.unzip $ entries matB
      idimB = idim matB

      nbs = S.enumFromStepN 0 1 $ odim matB
      nas = S.enumFromStepN 0 1 $ odim matA
      ns = S.concatMap (\na -> S.map ((,) na) nbs) nas

      kronecker_ixs (!na, !nb) =
        let as = streamSlice ptrsA na indicesA
            bs = streamSlice ptrsB nb indicesB
        in S.concatMap (\n -> S.map (+ n) bs) $ S.map (* idimB) as
      ixs = G.unstream $ flip S.sized (Exact nz)
            $ S.concatMap kronecker_ixs ns

      kronecker_xs (!na, !nb) =
        let as = streamSlice ptrsA na valuesA
            bs = streamSlice ptrsB nb valuesB
        in S.concatMap (\a -> S.map (* a) bs) as
      xs = G.unstream $ flip S.sized (Exact nz)
           $ S.concatMap kronecker_xs ns

  in Matrix
    { odim = dm
    , idim = dn
    , pointers = ptrs
    , entries = U.zip ixs xs
    }

takeDiag :: (Num a, Unbox a) => Matrix or a -> Vector a
{-# INLINE takeDiag #-}
takeDiag = \mat@Matrix{..} ->
  flip U.map (U.enumFromN 0 $ min odim idim) $ \m ->
    let (indices, values) = U.unzip $ S.entries $ slice mat m
    in case U.elemIndex m indices of
      Nothing -> 0
      Just ix -> values U.! ix

diag :: Unbox a => Vector a -> Matrix or a
{-# INLINE diag #-}
diag values = Matrix{..}
  where
    odim = U.length values
    idim = odim
    pointers = U.iterateN (odim + 1) (+1) 0
    indices = U.iterateN odim (+1) 0
    entries = U.zip indices values

ident :: (Num a, Unbox a) => Int -> Matrix or a
{-# INLINE ident #-}
ident n = diag $ U.replicate n 1

zeros :: (Orient or, Unbox a) => Int -> Int -> Matrix or a
{-# INLINE zeros #-}
zeros nRows nColumns = mat
  where
    pointers = U.replicate (odim + 1) 0
    indices = U.empty
    values = U.empty
    entries = U.zip indices values
    (odim, idim) = orientSwap (orient mat) (nRows, nColumns)
    mat = Matrix {..}
