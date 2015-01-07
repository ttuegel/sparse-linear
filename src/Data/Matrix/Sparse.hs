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
       ( Matrix(..), cmap, nonZero, slice
       , OrientK(..), Orient(..), Transpose, orient
       , compress, decompress, dedupInPlace
       , fromTriples, (><)
       , transpose, reorient
       , ctrans, hermitian
       , outer, mul
       , gaxpy_, gaxpy, mulV
       , lin, add
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
import Data.Function (fix)
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.MonoTraversable (Element, MonoFoldable(..), MonoFunctor(..))
import Data.Ord (comparing)
import Data.Proxy
import Data.Tuple (swap)
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector as Box
import qualified Data.Vector.Generic.Mutable as G
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import GHC.Stack (errorWithStackTrace)

import Data.Complex.Enhanced
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

type instance Element (Matrix or a) = a

instance Unbox a => MonoFunctor (Matrix or a) where
  {-# INLINE omap #-}
  omap = \f mat ->
    let (indices, values) = V.unzip $ entries mat
    in mat { entries = V.zip indices $ V.map f values }

instance Unbox a => MonoFoldable (Matrix or a) where
  {-# INLINE ofoldMap #-}
  {-# INLINE ofoldr #-}
  {-# INLINE ofoldl' #-}
  {-# INLINE ofoldr1Ex #-}
  {-# INLINE ofoldl1Ex' #-}
  ofoldMap = \f Matrix{..} -> ofoldMap f $ snd $ V.unzip entries
  ofoldr = \f r Matrix{..} -> V.foldr f r $ snd $ V.unzip entries
  ofoldl' = \f r Matrix{..} -> V.foldl' f r $ snd $ V.unzip entries
  ofoldr1Ex = \f Matrix{..} -> V.foldr1 f $ snd $ V.unzip entries
  ofoldl1Ex' = \f Matrix{..} -> V.foldl1' f $ snd $ V.unzip entries

instance (Num a, Unbox a) => Num (Matrix Col a) where
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE negate #-}
  {-# INLINE abs #-}
  {-# INLINE signum #-}
  (+) = add
  (-) = \a b -> lin 1 a (-1) b
  (*) = \matA matB -> matA `mul` reorient matB
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
  (*) = \matA matB -> reorient matA `mul` matB
  negate = omap negate
  abs = omap abs
  signum = omap signum
  fromInteger = errorWithStackTrace "fromInteger: not implemented"

orient :: Matrix or a -> Proxy or
{-# INLINE orient #-}
orient = \_ -> Proxy

nonZero :: Unbox a => Matrix or a -> Int
{-# INLINE nonZero #-}
nonZero = \Matrix{..} -> V.last pointers

cmap :: (Unbox a, Unbox b) => (a -> b) -> Matrix or a -> Matrix or b
{-# INLINE cmap #-}
cmap = \f m ->
  let (indices, values) = V.unzip $ entries m
  in m { entries = V.zip indices $ V.map f values }

slice :: Unbox a => Matrix or a -> Int -> S.Vector a
{-# INLINE slice #-}
slice = \Matrix{..} c ->
  let start = pointers V.! c
      end = pointers V.! (c + 1)
      len = end - start
  in S.Vector
  { S.dim = idim
  , S.entries = V.slice start len entries
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
  let (_rows, _cols, _vals) = V.unzip3 _triples
      (odim, idim) = orientSwap (orient mat) (nRows, nColumns)
      (_out, _inn) = orientSwap (orient mat) (_rows, _cols)
      ptrs = computePtrs odim _out

  _out <- V.unsafeThaw _out
  _inn <- V.unsafeThaw _inn
  _vals <- V.unsafeThaw _vals
  let _entries = MV.zip _inn _vals

  Intro.sortBy (comparing fst) $ MV.zip _out _entries

  dels <- V.forM (V.enumFromN 0 odim) $ \m -> do
    start <- V.unsafeIndexM ptrs m
    end <- V.unsafeIndexM ptrs (m + 1)
    let len = end - start
    dedupInPlace idim (MV.slice start len _entries)

  let shifts = V.scanl' (+) 0 dels
      pointers = V.zipWith (-) ptrs shifts

  V.forM_ (V.enumFromN 0 odim) $ \m -> do
    shift <- V.unsafeIndexM shifts m
    when (shift > 0) $ do
      start <- V.unsafeIndexM ptrs m
      end <- V.unsafeIndexM ptrs (m + 1)
      let len = end - start
          start' = start - shift
      MV.move (MV.slice start' len _entries) (MV.slice start len _entries)

  let nz' = V.last pointers
  entries <- V.unsafeFreeze $ MV.slice 0 nz' _entries

  return Matrix{..}

dedupInPlace
  :: (Num a, PrimMonad m, Unbox a)
  => Int -> MVector (PrimState m) (Int, a) -> m Int
{-# INLINE dedupInPlace #-}
dedupInPlace idim _entries = do
  Intro.sortBy (comparing fst) _entries
  let len = MV.length _entries
      (ixs, xs) = MV.unzip _entries
      dedup_go w r del
        | r < len = do
            ixr <- MV.unsafeRead ixs r
            ixw <- MV.unsafeRead ixs w
            if ixr == ixw
              then do
                MV.unsafeWrite ixs r idim
                x <- MV.unsafeRead xs r
                x' <- MV.unsafeRead xs w
                MV.unsafeWrite xs w $! x' + x
                dedup_go w (r + 1) (del + 1)
              else dedup_go r (r + 1) del
        | otherwise = return del
  del <- dedup_go 0 1 0
  Intro.sortBy (comparing fst) _entries
  return del

computePtrs :: Int -> Vector Int -> Vector Int
{-# INLINE computePtrs #-}
computePtrs n indices = runST $ do
  counts <- MV.replicate n 0
  -- scan the indices once, counting the occurrences of each index
  V.forM_ indices $ \ix -> do
    count <- MV.unsafeRead counts ix
    MV.unsafeWrite counts ix $! count + 1
  -- compute the index pointers by prefix-summing the occurrence counts
  V.scanl (+) 0 <$> V.unsafeFreeze counts

decompress :: Vector Int -> Vector Int
{-# INLINE decompress #-}
decompress = \ptrs -> V.create $ do
  indices <- MV.new $ V.last ptrs
  V.forM_ (V.enumFromN 0 $ V.length ptrs - 1) $ \c -> do
    start <- V.unsafeIndexM ptrs c
    end <- V.unsafeIndexM ptrs (c + 1)
    MV.set (MV.slice start (end - start) indices) c
  return indices

transpose :: Matrix or a -> Matrix (Transpose or) a
{-# INLINE transpose #-}
transpose = \Matrix{..} -> Matrix {..}

reorient :: Unbox a => Matrix (Transpose or) a -> Matrix or a
{-# INLINE reorient #-}
reorient mat@Matrix{..} = runST $ do
  let (indices, values) = V.unzip entries
      nz = V.length values
      ptrs = computePtrs idim indices

  -- re-initialize row counts from row pointers
  count <- V.thaw $ V.slice 0 idim ptrs

  _ixs <- MV.new nz
  _xs <- MV.new nz

  -- copy each column into place
  -- "major" and "minor" indices refer to the orientation of the original matrix
  V.forM_ (V.enumFromN 0 odim) $ \m -> do
    V.forM_ (S.entries $ slice mat m) $ \(n, x) -> do
      ix <- preincrement count n
      MV.unsafeWrite _ixs ix m
      MV.unsafeWrite _xs ix x

  _ixs <- V.unsafeFreeze _ixs
  _xs <- V.unsafeFreeze _xs

  return Matrix
    { odim = idim
    , idim = odim
    , pointers = ptrs
    , entries = V.zip _ixs _xs
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
      (indicesN, valuesN) = V.unzip $ S.entries sliceN
      (indicesM, valuesM) = V.unzip $ S.entries sliceM
      lenM = V.length valuesM
      lenN = V.length valuesN
      lengths = V.create $ do
        lens <- MV.replicate (odim + 1) 0
        V.forM_ indicesM $ \m -> MV.unsafeWrite lens m lenN
        return lens
      pointers = V.scanl' (+) 0 lengths
      indices = V.concat $ replicate lenM indicesN
      values = V.create $ do
        vals <- MV.new (lenM * lenN)
        V.forM_ (S.entries sliceM) $ \(ix, a) -> do
          start <- V.unsafeIndexM pointers ix
          end <- V.unsafeIndexM pointers (ix + 1)
          let len = end - start
          V.copy (MV.slice start len vals) $ V.map (* a) valuesN
        return vals
      entries = V.zip indices values
  in Matrix {..}

mul
  :: (Orient or, Num a, Unbox a)
  => Matrix Col a -> Matrix Row a -> Matrix or a
{-# INLINE mul #-}
mul = \matA matB ->
  if odim matA /= odim matB
    then errorWithStackTrace "mul: inner dimension mismatch"
  else
    let matZ = zeros (idim matA) (idim matB)
    in Box.foldl' add matZ $ do
      n <- Box.enumFromN 0 (odim matA)
      return $ outer (slice matA n) (slice matB n)

fromTriples
  :: (Orient or, Num a, Unbox a)
  => Int -> Int -> [(Int, Int, a)] -> Matrix or a
{-# INLINE fromTriples #-}
fromTriples = \nr nc -> compress nr nc . V.fromList

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
  | otherwise = runST $ do
      let odm = odim matA
          idm = idim matA
      ptrs <- MV.new (odm + 1)
      MV.unsafeWrite ptrs 0 0

      let nz = nonZero matA + nonZero matB
      _entries <- MV.new nz

      V.forM_ (V.enumFromN 0 odm) $ \ixM -> do
        off <- MV.unsafeRead ptrs ixM
        -- let dst = MV.slice off (nz - off) _entries
        len <- S.unsafeLinInto a (slice matA ixM) b (slice matB ixM) off _entries
        MV.unsafeWrite ptrs (ixM + 1) $! off + len

      pointers <- V.unsafeFreeze ptrs
      let nz' = V.last pointers
          odim = odm
          idim = idm
      entries <- V.unsafeFreeze $ MV.slice 0 nz' _entries

      return Matrix {..}

add :: (Num a, Unbox a) => Matrix or a -> Matrix or a -> Matrix or a
{-# INLINE add #-}
add a b = lin 1 a 1 b

unsafeDotInto
  :: (Num a, PrimMonad m, Unbox a)
  => Matrix Row a -> S.Vector a -> MVector (PrimState m) (Int, a) -> m Int
{-# INLINE unsafeDotInto #-}
unsafeDotInto = \matA bs cs -> do
  let (ixsB, xsB) = V.unzip $ S.entries bs
      (ixsC, xsC) = MV.unzip cs
      lenB = V.length ixsB
      mul_go !m !k
        | m < odim matA = do
            let as = S.entries $ slice matA m
                (ixsA, xsA) = V.unzip as
                lenA = V.length as
                dot_go !nz !acc !i !j
                  | i < lenA && j < lenB = do
                      rA <- V.unsafeIndexM ixsA i
                      rB <- V.unsafeIndexM ixsB j
                      case compare rA rB of
                        LT -> dot_go nz acc (i + 1) j
                        EQ -> do
                          xA <- V.unsafeIndexM xsA i
                          xB <- V.unsafeIndexM xsB j
                          dot_go True (acc + xA * xB) (i + 1) (j + 1)
                        GT -> dot_go nz acc i (j + 1)
                  | otherwise = do
                      when nz $ do
                        MV.unsafeWrite ixsC k m
                        MV.unsafeWrite xsC k acc
                      return nz
            nz <- dot_go False 0 0 0
            mul_go (m + 1) (if nz then k + 1 else k)
        | otherwise = return k
  mul_go 0 0

gaxpy_
  :: (G.MVector v a, Orient or, Num a, PrimMonad m, Unbox a)
  => Matrix or a -> v (PrimState m) a -> v (PrimState m) a -> m ()
{-# INLINE gaxpy_ #-}
gaxpy_ mat@Matrix{..} xs ys =
  V.forM_ (V.enumFromN 0 odim) $ \m -> do
    V.forM_ (S.entries $ slice mat m) $ \(n, a) -> do
      let (r, c) = orientSwap (orient mat) (m, n)
      x <- G.unsafeRead xs c
      y <- G.unsafeRead ys r
      G.unsafeWrite ys r $! y + a * x

gaxpy
  :: (Orient or, Num a, Unbox a)
  => Matrix or a -> Vector a -> Vector a -> Vector a
{-# INLINE gaxpy #-}
gaxpy = \a _x _y -> runST $ do
  _y <- V.thaw _y
  _x <- V.unsafeThaw _x
  gaxpy_ a _x _y
  V.unsafeFreeze _y

mulV :: (Orient or, Num a, Unbox a) => Matrix or a -> Vector a -> Vector a
{-# INLINE mulV #-}
mulV = \a _x -> runST $ do
  _x <- V.unsafeThaw _x
  y <- MV.replicate (MV.length _x) 0
  gaxpy_ a _x y
  V.unsafeFreeze y

mcat :: Unbox a => Matrix or a -> Matrix or a -> Matrix or a
{-# INLINE mcat #-}
mcat a b
  | idim a /= idim b = errorWithStackTrace "inner dimension mismatch"
  | otherwise = Matrix
      { odim = dm
      , idim = idim a
      , pointers = V.init (pointers a) V.++ (V.map (+ nza) $ pointers b)
      , entries = entries a V.++ entries b
      }
  where
    dm = odim a + odim b
    nza = nonZero a

hcat :: Unbox a => Matrix Col a -> Matrix Col a -> Matrix Col a
{-# INLINE hcat #-}
hcat = mcat

vcat :: Unbox a => Matrix Row a -> Matrix Row a -> Matrix Row a
{-# INLINE vcat #-}
vcat = mcat

fromBlocks :: (Num a, Unbox a) => [[Maybe (Matrix Col a)]] -> Matrix Row a
{-# SPECIALIZE fromBlocks :: [[Maybe (Matrix Col Double)]] -> Matrix Row Double #-}
{-# SPECIALIZE fromBlocks :: [[Maybe (Matrix Col (Complex Double))]] -> Matrix Row (Complex Double) #-}
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
        heightSpecs = map (map idim . catMaybes) rows
        widthSpecs = map (map odim . catMaybes) cols
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
  :: (Num a, Unbox a) => [[Maybe (Matrix Col a)]] -> Matrix Row a
{-# INLINE fromBlocksDiag #-}
fromBlocksDiag = fromBlocks . zipWith rejoin [0..] . List.transpose where
  rejoin = \n as -> let (rs, ls) = splitAt (length as - n) as in ls ++ rs

kronecker :: (Num a, Unbox a) => Matrix or a -> Matrix or a -> Matrix or a
{-# SPECIALIZE kronecker :: Matrix Row Double -> Matrix Row Double -> Matrix Row Double #-}
{-# SPECIALIZE kronecker :: Matrix Col Double -> Matrix Col Double -> Matrix Col Double #-}
{-# SPECIALIZE kronecker :: Matrix Row (Complex Double) -> Matrix Row (Complex Double) -> Matrix Row (Complex Double) #-}
{-# SPECIALIZE kronecker :: Matrix Col (Complex Double) -> Matrix Col (Complex Double) -> Matrix Col (Complex Double) #-}
kronecker matA matB = runST $ do
  let dn = idim matA * idim matB
      dm = odim matA * odim matB
      nz = nonZero matA * nonZero matB

      lengthsA = V.zipWith (-) (V.tail $ pointers matA) (pointers matA)
      lengthsB = V.zipWith (-) (V.tail $ pointers matB) (pointers matB)

  let ptrs = V.scanl' (+) 0
             $ V.concat
             $ map (\nzA -> V.map (* nzA) lengthsB)
             $ V.toList lengthsA

  _ixs <- MV.new nz
  _xs <- MV.new nz

  V.forM_ (V.enumFromN 0 $ odim matA) $ \mA -> do
    V.forM_ (V.enumFromN 0 $ odim matB) $ \mB -> do

      let (indicesA, _) = V.unzip $ S.entries $ slice matA mA
          lenA = V.length indicesA
          (indicesB, _) = V.unzip $ S.entries $ slice matB mB
          lenB = V.length indicesB
          m = mA * odim matB + mB

      let copyIxs ixA off
            | ixA < lenA = do
                nA <- V.unsafeIndexM indicesA ixA
                let nOff = nA * idim matB
                V.copy (MV.slice off lenB _ixs) $ V.map (+ nOff) indicesB
                copyIxs (ixA + 1) (off + lenB)
            | otherwise = return ()

      V.unsafeIndexM ptrs m >>= copyIxs 0

  V.forM_ (V.enumFromN 0 $ odim matA) $ \mA -> do
    V.forM_ (V.enumFromN 0 $ odim matB) $ \mB -> do

      let (_, valuesA) = V.unzip $ S.entries $ slice matA mA
          lenA = V.length valuesA
          (_, valuesB) = V.unzip $ S.entries $ slice matB mB
          lenB = V.length valuesB
          m = mA * odim matB + mB

      let copyXs ixA off
            | ixA < lenA = do
                a <- V.unsafeIndexM valuesA ixA
                V.copy (MV.slice off lenB _xs) $ V.map (* a) valuesB
                copyXs (ixA + 1) (off + lenB)
            | otherwise = return ()

      V.unsafeIndexM ptrs m >>= copyXs 0

  _ixs <- V.unsafeFreeze _ixs
  _xs <- V.unsafeFreeze _xs
  return Matrix
    { odim = dm
    , idim = dn
    , pointers = ptrs
    , entries = V.zip _ixs _xs
    }

takeDiag :: (Num a, Unbox a) => Matrix or a -> Vector a
{-# INLINE takeDiag #-}
takeDiag = \mat@Matrix{..} ->
  flip V.map (V.enumFromN 0 $ min odim idim) $ \m ->
    let (indices, values) = V.unzip $ S.entries $ slice mat m
    in case V.elemIndex m indices of
      Nothing -> 0
      Just ix -> values V.! ix

diag :: Unbox a => Vector a -> Matrix or a
{-# INLINE diag #-}
diag values = Matrix{..}
  where
    odim = V.length values
    idim = odim
    pointers = V.iterateN (odim + 1) (+1) 0
    indices = V.iterateN odim (+1) 0
    entries = V.zip indices values

ident :: (Num a, Unbox a) => Int -> Matrix or a
{-# INLINE ident #-}
ident n = diag $ V.replicate n 1

zeros :: (Orient or, Unbox a) => Int -> Int -> Matrix or a
{-# INLINE zeros #-}
zeros nRows nColumns = mat
  where
    pointers = V.replicate (odim + 1) 0
    indices = V.empty
    values = V.empty
    entries = V.zip indices values
    (odim, idim) = orientSwap (orient mat) (nRows, nColumns)
    mat = Matrix {..}
