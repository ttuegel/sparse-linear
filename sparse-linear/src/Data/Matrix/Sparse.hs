{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Matrix.Sparse
       ( Matrix(..), cmap, scale, nonZero, slice
       , compress, decompress, dedupInPlace
       , fromTriples, (><)
       , transpose, ctrans, hermitian
       , outer
       , axpy_, axpy, mulV
       , glin, lin
       , hjoin, hcat, vjoin, vcat
       , fromBlocks, fromBlocksDiag
       , kronecker
       , takeDiag, diag, blockDiag
       , ident, zeros
       , pack
       , Unbox, module Data.Complex.Enhanced
       ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Maybe (catMaybes)
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid ((<>), Monoid(..), First(..))
#else
import Data.Monoid ((<>), First(..))
#endif
import Data.MonoTraversable (Element, MonoFoldable(..), MonoFunctor(..))
import Data.Ord (comparing)
import qualified Data.Vector as Boxed
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as UM
import Foreign.Storable (Storable)
import GHC.Stack (errorWithStackTrace)
import qualified Numeric.LinearAlgebra.HMatrix as Dense

import Data.Complex.Enhanced
import Data.Matrix.Sparse.Mul
import Data.Matrix.Sparse.Slice
import qualified Data.Vector.Sparse as S
import qualified Data.Vector.Sparse.ScatterGather as SG
import Data.Vector.Util

-- | Matrix in compressed sparse column (CSC) format.
data Matrix v a = Matrix
  { ncols :: !Int -- ^ number of columns
  , nrows :: !Int -- ^ number of rows
  , pointers :: !(Vector Int)
                -- ^ starting index of each slice,
                -- last element is number of non-zero entries
  , entries :: v (Int, a)
  }

deriving instance Eq (v (Int, a)) => Eq (Matrix v a)
deriving instance Show (v (Int, a)) => Show (Matrix v a)
deriving instance Read (v (Int, a)) => Read (Matrix v a)

type instance Element (Matrix v a) = a

instance Unbox a => MonoFunctor (Matrix Vector a) where
  {-# INLINE omap #-}
  omap = cmap

instance Unbox a => MonoFoldable (Matrix Vector a) where
  {-# INLINE ofoldMap #-}
  {-# INLINE ofoldr #-}
  {-# INLINE ofoldl' #-}
  {-# INLINE ofoldr1Ex #-}
  {-# INLINE ofoldl1Ex' #-}
  ofoldMap = \f Matrix {..} -> ofoldMap f $ snd $ U.unzip entries
  ofoldr = \f r Matrix {..} -> U.foldr f r $ snd $ U.unzip entries
  ofoldl' = \f r Matrix {..} -> U.foldl' f r $ snd $ U.unzip entries
  ofoldr1Ex = \f Matrix {..} -> U.foldr1 f $ snd $ U.unzip entries
  ofoldl1Ex' = \f Matrix {..} -> U.foldl1' f $ snd $ U.unzip entries

instance (Num a, Unbox a) => Num (Matrix Vector a) where
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE negate #-}
  {-# INLINE abs #-}
  {-# INLINE signum #-}
  (+) = \a b -> glin 0 (+) a (+) b
  (-) = \a b -> glin 0 (+) a (-) b
  (*) = \a b ->
    if ncols a /= nrows b then oops "inner dimension mismatch"
    else
      let (ptrs, ents) = unsafeMul (nrows a) (ncols b)
                         (pointers a) (entries a)
                         (pointers b) (entries b)
      in Matrix
         { nrows = nrows a
         , ncols = ncols b
         , pointers = ptrs
         , entries = ents
         }
    where
      oops str = errorWithStackTrace ("(*): " ++ str)
  negate = omap negate
  abs = omap abs
  signum = omap signum
  fromInteger = errorWithStackTrace "fromInteger: not implemented"

nonZero :: Unbox a => Matrix Vector a -> Int
{-# INLINE nonZero #-}
nonZero = \Matrix {..} -> U.last pointers

cmap :: (Unbox a, Unbox b) => (a -> b) -> Matrix Vector a -> Matrix Vector b
{-# INLINE cmap #-}
cmap = \f m ->
  let (indices, values) = U.unzip $ entries m
  in m { entries = U.zip indices $ U.map f values }

scale :: (Num a, Unbox a) => a -> Matrix Vector a -> Matrix Vector a
{-# INLINE scale #-}
scale = \x -> cmap (* x)

slice :: Unbox a => Matrix Vector a -> Int -> S.Vector Vector a
{-# INLINE slice #-}
slice = \Matrix {..} c ->
  let (indices, values) = U.unzip (unsafeSlice pointers entries c)
  in S.Vector nrows indices values

compress
  :: (Num a, Unbox a)
  => Int  -- ^ number of rows
  -> Int  -- ^ number of columns
  -> Vector (Int, Int, a)  -- ^ (row, column, value)
  -> Matrix Vector a
{-# SPECIALIZE compress :: Int -> Int -> Vector (Int, Int, Double) -> Matrix Vector Double #-}
{-# SPECIALIZE compress :: Int -> Int -> Vector (Int, Int, Complex Double) -> Matrix Vector (Complex Double) #-}
compress nrows ncols _triples = runST $ do
  let (_rows, _cols, _vals) = U.unzip3 _triples
      ptrs = computePtrs ncols _cols

  let checkBounds bound prev ix this
        | this >= 0 && this < bound = prev <> mempty
        | otherwise = prev <> First (Just ix)

  -- check bounds of row indices
  case getFirst (U.ifoldl' (checkBounds nrows) mempty _rows) of
    Nothing -> return ()
    Just ix ->
      let bounds = show (0 :: Int, nrows)
      in oops ("row index out of bounds " ++ bounds ++ " at " ++ show ix)

  -- check bounds of column indices
  case getFirst (U.ifoldl' (checkBounds ncols) mempty _cols) of
    Nothing -> return ()
    Just ix ->
      let bounds = show (0 :: Int, ncols)
      in oops ("column index out of bounds " ++ bounds ++ " at " ++ show ix)

  _rows <- U.unsafeThaw _rows
  _cols <- U.unsafeThaw _cols
  _vals <- U.unsafeThaw _vals
  let _entries = UM.zip _rows _vals

  Intro.sortBy (comparing fst) $ UM.zip _cols _entries

  -- deduplicate columns
  -- sum entries so there is at most one entry for each row and column
  -- ndel is a vector holding the number of entries removed from each column
  ndel <- U.forM (U.enumFromN 0 ncols) $ \m ->
    dedupInPlace nrows $ unsafeMSlice ptrs m _entries

  let
    -- the number of indices each column should be shifted down in the
    -- entries vector
    shifts = U.scanl' (+) 0 ndel
    -- the final column-start pointers into the entries matrix
    pointers = U.zipWith (-) ptrs shifts

  -- perform the shifts
  U.forM_ (U.enumFromN 0 ncols) $ \m -> do
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
  entries <- U.force <$> U.unsafeFreeze (UM.unsafeSlice 0 nz' _entries)

  return Matrix {..}
  where
    oops str = errorWithStackTrace ("compress: " ++ str)

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
                UM.unsafeWrite xs w (x' + x)
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
    UM.unsafeWrite counts ix (count + 1)
  -- compute the index pointers by prefix-summing the occurrence counts
  U.scanl (+) 0 <$> U.unsafeFreeze counts

decompress :: Vector Int -> Vector Int
{-# INLINE decompress #-}
decompress = \ptrs -> U.create $ do
  indices <- UM.new $ U.last ptrs
  U.forM_ (U.enumFromN 0 $ U.length ptrs - 1) $ \c ->
    UM.set (unsafeMSlice ptrs c indices) c
  return indices

transpose :: Unbox a => Matrix Vector a -> Matrix Vector a
{-# INLINE transpose #-}
transpose Matrix {..} = runST $ do
  let (indices, values) = U.unzip entries
      nz = U.length values
      ptrs = computePtrs nrows indices

  -- re-initialize row counts from row pointers
  count <- U.thaw $ U.unsafeSlice 0 nrows ptrs

  _ixs <- UM.new nz
  _xs <- UM.new nz

  -- copy each column into place
  U.forM_ (U.enumFromN 0 ncols) $ \m -> do
    U.forM_ (unsafeSlice pointers entries m) $ \(n, x) -> do
      ix <- preincrement count n
      UM.unsafeWrite _ixs ix m
      UM.unsafeWrite _xs ix x

  _ixs <- U.unsafeFreeze _ixs
  _xs <- U.unsafeFreeze _xs

  return Matrix
    { ncols = nrows
    , nrows = ncols
    , pointers = ptrs
    , entries = U.zip _ixs _xs
    }

outer
  :: (Num a, Unbox a)
  => S.Vector Vector a -- ^ sparse column vector
  -> S.Vector Vector a -- ^ sparse row vector
  -> Matrix Vector a
{-# INLINE outer #-}
outer = \sliceC sliceR ->
  let -- indices of sliceM are outer (major) indices of result
      -- indices of sliceN are inner (minor) indices of result
      S.Vector nrows indicesR valuesR = sliceR
      S.Vector ncols indicesC valuesC = sliceC
      lenR = U.length valuesR
      lenC = U.length valuesC
      lengths = U.create $ do
        lens <- UM.replicate (ncols + 1) 0
        U.forM_ indicesC $ \m -> UM.unsafeWrite lens m lenR
        return lens
      pointers = U.scanl' (+) 0 lengths
      indices = U.concat $ replicate lenC indicesR
      values = U.create $ do
        vals <- UM.new (lenC * lenR)
        U.forM_ (U.zip indicesC valuesC) $ \(ix, a) ->
          U.copy (unsafeMSlice pointers ix vals) $ U.map (* a) valuesR
        return vals
      entries = U.zip indices values
  in Matrix {..}

fromTriples
  :: (Num a, Unbox a)
  => Int -> Int -> [(Int, Int, a)] -> Matrix Vector a
{-# INLINE fromTriples #-}
fromTriples = \nr nc -> compress nr nc . U.fromList

(><)
  :: (Num a, Unbox a)
  => Int -> Int -> [(Int, Int, a)] -> Matrix Vector a
{-# INLINE (><) #-}
(><) = fromTriples

ctrans
  :: (IsReal a, Num a, Unbox a)
  => Matrix Vector a -> Matrix Vector a
{-# INLINE ctrans #-}
ctrans = omap conj . transpose

hermitian :: (Eq a, IsReal a, Num a, Unbox a) => Matrix Vector a -> Bool
{-# INLINE hermitian #-}
hermitian m = (ctrans m) == m

toColumns :: Unbox a => Matrix Vector a -> Boxed.Vector (S.Vector Vector a)
{-# INLINE toColumns #-}
toColumns mat = Boxed.generate (ncols mat) (slice mat)

unsafeFromColumns :: Unbox a
                  => Boxed.Vector (S.Vector Vector a)
                  -> Matrix Vector a
{-# INLINE unsafeFromColumns #-}
unsafeFromColumns cols
  = Matrix
    { nrows = U.head lengths
    , ncols = Boxed.length cols
    , pointers = U.scanl' (+) 0 nonZeros
    , entries = U.concat (entries_col <$> Boxed.toList cols)
    }
  where
    lengths = U.convert (Boxed.map S.length cols)
    nonZeros = U.convert (Boxed.map S.nonZero cols)
    entries_col (S.Vector _ indices values) = U.zip indices values

glin :: (Unbox a, Unbox b, Unbox c)
     => c
     -> (c -> a -> c) -> Matrix Vector a
     -> (c -> b -> c) -> Matrix Vector b
     -> Matrix Vector c
{-# INLINE glin #-}
glin c fA matA fB matB
  | nrows matA /= nrows matB = oops "row number mismatch"
  | ncols matA /= ncols matB = oops "column number mismatch"
  | otherwise
      = unsafeFromColumns $ SG.run (nrows matA) $ do
        let scatterColumns colA colB = do
              SG.reset c
              SG.unsafeScatter colA fA
              SG.unsafeScatter colB fB
              SG.gather
        Boxed.zipWithM scatterColumns colsA colsB
  where
    oops str = errorWithStackTrace ("glin: " ++ str)
    colsA = toColumns matA
    colsB = toColumns matB

lin
  :: (Num a, Unbox a)
  => a -> Matrix Vector a -> a -> Matrix Vector a -> Matrix Vector a
{-# INLINE lin #-}
lin alpha matA beta matB
  = glin 0 (\r a -> r + alpha * a) matA (\r b -> r + beta * b) matB

axpy_
  :: (GM.MVector v a, Num a, PrimMonad m, Unbox a)
  => Matrix Vector a -> v (PrimState m) a -> v (PrimState m) a -> m ()
{-# INLINE axpy_ #-}
axpy_ Matrix {..} xs ys
  | GM.length xs /= ncols = oops "column dimension does not match operand"
  | GM.length ys /= nrows = oops "row dimension does not match result"
  | otherwise =
      U.forM_ (U.enumFromN 0 ncols) $ \c -> do
        U.forM_ (unsafeSlice pointers entries c) $ \(r, a) -> do
          x <- GM.unsafeRead xs c
          y <- GM.unsafeRead ys r
          GM.unsafeWrite ys r (a * x + y)
  where
    oops str = errorWithStackTrace ("axpy_: " ++ str)

axpy :: (Num a, Unbox a) => Matrix Vector a -> Vector a -> Vector a -> Vector a
{-# SPECIALIZE axpy :: Matrix Vector Double -> Vector Double -> Vector Double -> Vector Double #-}
{-# SPECIALIZE axpy :: Matrix Vector (Complex Double) -> Vector (Complex Double) -> Vector (Complex Double) -> Vector (Complex Double) #-}
axpy = \a _x _y -> runST $ do
  _y <- U.thaw _y
  _x <- U.unsafeThaw _x
  axpy_ a _x _y
  U.unsafeFreeze _y

mulV :: (G.Vector v a, Num a, Unbox a) => Matrix Vector a -> v a -> v a
{-# SPECIALIZE mulV :: Matrix Vector Double -> Vector Double -> Vector Double #-}
{-# SPECIALIZE mulV :: Matrix Vector (Complex Double) -> Vector (Complex Double) -> Vector (Complex Double) #-}
mulV = \a _x -> runST $ do
  _x <- G.unsafeThaw _x
  y <- GM.replicate (nrows a) 0
  axpy_ a _x y
  G.unsafeFreeze y

hjoin :: Unbox a => Matrix Vector a -> Matrix Vector a -> Matrix Vector a
{-# INLINE hjoin #-}
hjoin a b = hcat [a, b]

hcat :: Unbox a => [Matrix Vector a] -> Matrix Vector a
{-# SPECIALIZE hcat :: [Matrix Vector Double] -> Matrix Vector Double #-}
{-# SPECIALIZE hcat :: [Matrix Vector (Complex Double)] -> Matrix Vector (Complex Double) #-}
hcat mats
  | null mats = oops "empty list"
  | any (/= _nrows) (map nrows mats) = oops "nrows mismatch"
  | otherwise =
      Matrix
      { ncols = F.foldl' (+) 0 $ map ncols mats
      , nrows = _nrows
      , pointers = U.scanl' (+) 0 $ U.concat $ map lengths mats
      , entries = U.concat $ map entries mats
      }
  where
    _nrows = nrows $ head mats
    lengths m = let ptrs = pointers m in U.zipWith (-) (U.tail ptrs) ptrs
    oops str = errorWithStackTrace ("hcat: " ++ str)

vjoin :: Unbox a => Matrix Vector a -> Matrix Vector a -> Matrix Vector a
{-# INLINE vjoin #-}
vjoin a b = vcat [a, b]

vcat :: Unbox a => [Matrix Vector a] -> Matrix Vector a
{-# SPECIALIZE vcat :: [Matrix Vector Double] -> Matrix Vector Double #-}
{-# SPECIALIZE vcat :: [Matrix Vector (Complex Double)] -> Matrix Vector (Complex Double) #-}
vcat mats
  | null mats = oops "empty list"
  | any (/= _ncols) (map ncols mats) = oops "ncols mismatch"
  | otherwise =
      Matrix
      { ncols = _ncols
      , nrows = F.foldl' (+) 0 (map nrows mats)
      , pointers = _pointers
      , entries = U.create $ do
          _entries <- UM.new (U.last _pointers)
          let -- when concatenating matrices vertically, their row indices
              -- must be offset according to their position in the final matrix
              offsets = L.scanl' (+) 0 (map nrows mats)
          U.forM_ (U.enumFromN 0 _ncols) $ \c -> do
            let copyMatrix !ixD (Matrix {..}, off) = do
                  let copyWithOffset !ix (row, x) = do
                        UM.unsafeWrite _entries ix (row + off, x)
                        return (ix + 1)
                  U.foldM' copyWithOffset ixD (unsafeSlice pointers entries c)
            F.foldlM copyMatrix (_pointers U.! c) (zip mats offsets)
          return _entries
      }
  where
    oops str = errorWithStackTrace ("vcat: " ++ str)
    _ncols = ncols (head mats)
    _pointers = F.foldr1 (U.zipWith (+)) (map pointers mats)

fromBlocks :: (Num a, Unbox a) => [[Maybe (Matrix Vector a)]] -> Matrix Vector a
{-# SPECIALIZE fromBlocks :: [[Maybe (Matrix Vector Double)]] -> Matrix Vector Double #-}
{-# SPECIALIZE fromBlocks :: [[Maybe (Matrix Vector (Complex Double))]] -> Matrix Vector (Complex Double) #-}
fromBlocks = vcat . map hcat . adjustDims
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
        heightSpecs = map (map nrows . catMaybes) rows
        widthSpecs = map (map ncols . catMaybes) cols
        oops str = errorWithStackTrace ("fromBlocks: " ++ str)
        heights
          | underspecified heightSpecs = oops "underspecified heights"
          | incompatible heightSpecs = oops "incompatible heights"
          | otherwise = U.fromList $ map head heightSpecs
        widths
          | underspecified widthSpecs = oops "underspecified widths"
          | incompatible widthSpecs = oops "incompatible widths"
          | otherwise = U.fromList $ map head widthSpecs

fromBlocksDiag
  :: (Num a, Unbox a) => [[Maybe (Matrix Vector a)]] -> Matrix Vector a
{-# INLINE fromBlocksDiag #-}
fromBlocksDiag = fromBlocks . zipWith rejoin [0..] . L.transpose where
  rejoin = \n as -> let (rs, ls) = splitAt (length as - n) as in ls ++ rs

kronecker :: (Num a, Unbox a) => Matrix Vector a -> Matrix Vector a -> Matrix Vector a
{-# SPECIALIZE kronecker :: Matrix Vector Double -> Matrix Vector Double -> Matrix Vector Double #-}
{-# SPECIALIZE kronecker :: Matrix Vector (Complex Double) -> Matrix Vector (Complex Double) -> Matrix Vector (Complex Double) #-}
kronecker matA matB =
  let _nrows = nrows matA * nrows matB
      _ncols = ncols matA * ncols matB

      ptrsA = pointers matA
      ptrsB = pointers matB
      lengthsA = U.zipWith (-) (U.tail ptrsA) ptrsA
      lengthsB = U.zipWith (-) (U.tail ptrsB) ptrsB
      ptrs = U.scanl' (+) 0
             $ U.concatMap (\nzA -> U.map (* nzA) lengthsB) lengthsA

      (indicesA, valuesA) = U.unzip $ entries matA
      (indicesB, valuesB) = U.unzip $ entries matB

      nbs = U.enumFromStepN 0 1 $ ncols matB
      nas = U.enumFromStepN 0 1 $ ncols matA
      ns = U.concatMap (\na -> U.map ((,) na) nbs) nas

      kronecker_ixs (!na, !nb) =
        let as = unsafeSlice ptrsA indicesA na
            bs = unsafeSlice ptrsB indicesB nb
        in U.concatMap (\n -> U.map (+ n) bs) (U.map (* (nrows matB)) as)
      ixs = U.concatMap kronecker_ixs ns

      kronecker_xs (!na, !nb) =
        let as = unsafeSlice ptrsA valuesA na
            bs = unsafeSlice ptrsB valuesB nb
        in U.concatMap (\a -> U.map (* a) bs) as
      xs = U.concatMap kronecker_xs ns

  in Matrix
    { ncols = _ncols
    , nrows = _nrows
    , pointers = ptrs
    , entries = U.zip ixs xs
    }

takeDiag :: (Num a, Unbox a) => Matrix Vector a -> Vector a
{-# INLINE takeDiag #-}
takeDiag = \mat@Matrix {..} ->
  flip U.map (U.enumFromN 0 $ min nrows ncols) $ \m ->
    let S.Vector _ indices values = slice mat m
    in case U.elemIndex m indices of
      Nothing -> 0
      Just ix -> values U.! ix

diag :: Unbox a => Vector a -> Matrix Vector a
{-# INLINE diag #-}
diag values = Matrix{..}
  where
    ncols = U.length values
    nrows = ncols
    pointers = U.iterateN (ncols + 1) (+1) 0
    indices = U.iterateN ncols (+1) 0
    entries = U.zip indices values

blockDiag :: (Num a, Unbox a) => [Matrix Vector a] -> Matrix Vector a
{-# INLINE blockDiag #-}
blockDiag mats
  = fromBlocksDiag ((Just <$> mats) : replicate (len - 1) (replicate len Nothing))
  where
    len = length mats

ident :: (Num a, Unbox a) => Int -> Matrix Vector a
{-# INLINE ident #-}
ident n = diag $ U.replicate n 1

zeros :: Unbox a => Int -> Int -> Matrix Vector a
{-# INLINE zeros #-}
zeros nrows ncols = Matrix {..}
  where
    pointers = U.replicate (ncols + 1) 0
    indices = U.empty
    values = U.empty
    entries = U.zip indices values

pack :: (Dense.Container Dense.Vector a, Num a, Storable a, Unbox a)
     => Matrix Vector a -> Dense.Matrix a
{-# INLINE pack #-}
pack Matrix {..} =
  Dense.assoc (nrows, ncols) 0 $ do
    c <- [0..(ncols - 1)]
    begin <- U.unsafeIndexM pointers c
    end <- U.unsafeIndexM pointers (c + 1)
    let len = end - begin
        col = U.slice begin len entries
    U.toList (U.map (\(r, x) -> ((r, c), x)) col)
