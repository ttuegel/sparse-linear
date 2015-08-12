{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Matrix.Sparse
       ( Matrix(..), cmap, scale, nonZero, slice
       , compress, decompress, dedupInPlace
       , fromTriples, (><)
       , transpose, ctrans, hermitian
       , outer
       , axpy_, axpy, mulV
       , lin, add
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
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Fusion.Stream as S
import Data.Vector.Fusion.Stream.Size
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
import Data.Matrix.Sparse.ScatterGather
import Data.Matrix.Sparse.Slice
import qualified Data.Vector.Sparse as S
import Data.Vector.Util

-- | Matrix in compressed sparse column (CSC) format.
data Matrix a = Matrix
  { ncols :: !Int -- ^ number of columns
  , nrows :: !Int -- ^ number of rows
  , pointers :: !(Vector Int)
                -- ^ starting index of each slice,
                -- last element is number of non-zero entries
  , entries :: Vector (Int, a)
  }
  deriving (Eq, Read, Show)

type instance Element (Matrix a) = a

instance Unbox a => MonoFunctor (Matrix a) where
  {-# INLINE omap #-}
  omap = cmap

instance Unbox a => MonoFoldable (Matrix a) where
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

instance (Num a, Unbox a) => Num (Matrix a) where
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE negate #-}
  {-# INLINE abs #-}
  {-# INLINE signum #-}
  (+) = add
  (-) = \a b -> lin 1 a (-1) b
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

nonZero :: Unbox a => Matrix a -> Int
{-# INLINE nonZero #-}
nonZero = \Matrix {..} -> U.last pointers

cmap :: (Unbox a, Unbox b) => (a -> b) -> Matrix a -> Matrix b
{-# INLINE cmap #-}
cmap = \f m ->
  let (indices, values) = U.unzip $ entries m
  in m { entries = U.zip indices $ U.map f values }

scale :: (Num a, Unbox a) => a -> Matrix a -> Matrix a
{-# INLINE scale #-}
scale = \x -> cmap (* x)

slice :: Unbox a => Matrix a -> Int -> S.Vector a
{-# INLINE slice #-}
slice = \Matrix {..} c ->
  S.Vector
  { S.dim = nrows
  , S.entries = unsafeSlice pointers c entries
  }

compress
  :: (Num a, Unbox a)
  => Int  -- ^ number of rows
  -> Int  -- ^ number of columns
  -> Vector (Int, Int, a)  -- ^ (row, column, value)
  -> Matrix a
{-# SPECIALIZE compress :: Int -> Int -> Vector (Int, Int, Double) -> Matrix Double #-}
{-# SPECIALIZE compress :: Int -> Int -> Vector (Int, Int, Complex Double) -> Matrix (Complex Double) #-}
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

transpose :: Unbox a => Matrix a -> Matrix a
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
    U.forM_ (unsafeSlice pointers m entries) $ \(n, x) -> do
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
  => S.Vector a -- ^ sparse column vector
  -> S.Vector a -- ^ sparse row vector
  -> Matrix a
{-# INLINE outer #-}
outer = \sliceC sliceR ->
  let -- indices of sliceM are outer (major) indices of result
      -- indices of sliceN are inner (minor) indices of result
      nrows = S.dim sliceR
      ncols = S.dim sliceC
      (indicesR, valuesR) = U.unzip $ S.entries sliceR
      (indicesC, valuesC) = U.unzip $ S.entries sliceC
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
        U.forM_ (S.entries sliceC) $ \(ix, a) ->
          U.copy (unsafeMSlice pointers ix vals) $ U.map (* a) valuesR
        return vals
      entries = U.zip indices values
  in Matrix {..}

fromTriples
  :: (Num a, Unbox a)
  => Int -> Int -> [(Int, Int, a)] -> Matrix a
{-# INLINE fromTriples #-}
fromTriples = \nr nc -> compress nr nc . U.fromList

(><)
  :: (Num a, Unbox a)
  => Int -> Int -> [(Int, Int, a)] -> Matrix a
{-# INLINE (><) #-}
(><) = fromTriples

ctrans
  :: (IsReal a, Num a, Unbox a)
  => Matrix a -> Matrix a
{-# INLINE ctrans #-}
ctrans = omap conj . transpose

hermitian :: (Eq a, IsReal a, Num a, Unbox a) => Matrix a -> Bool
{-# INLINE hermitian #-}
hermitian m = (ctrans m) == m

lin :: (Num a, Unbox a) => a -> Matrix a -> a -> Matrix a -> Matrix a
{-# SPECIALIZE lin :: Double -> Matrix Double -> Double -> Matrix Double -> Matrix Double #-}
{-# SPECIALIZE lin :: (Complex Double) -> Matrix (Complex Double) -> (Complex Double) -> Matrix (Complex Double) -> Matrix (Complex Double) #-}
lin a matA b matB
  | nrows matA /= nrows matB = oops "nrows mismatch"
  | ncols matA /= ncols matB = oops "ncols mismatch"
  | otherwise =
      Matrix
      { ncols = ncols matA
      , nrows = nrows matA
      , pointers = ptrs
      , entries = ents
      }
  where
    oops str = errorWithStackTrace ("lin: " ++ str)

    ptrsA = pointers matA
    entriesA = entries matA
    ptrsB = pointers matB
    entriesB = entries matB
    odim = ncols matA
    idim = nrows matA

    (ptrs, ents) = runST $ do
      _ptrs <- UM.new (odim + 1)
      UM.unsafeWrite _ptrs 0 0

      -- This may allocate more memory than required (if the matrix patterns
      -- overlap), but at worst it only allocates min(U.last ptrsA, U.last ptrsB)
      -- more than required. This is much better than incremental allocation with
      -- copying!
      _entries <- UM.new (U.last ptrsA + U.last ptrsB)

      _work <- UM.zip <$> UM.replicate idim False <*> UM.new idim

      let unsafeLin_go n = do
            let sliceA = unsafeSlice ptrsA n entriesA
                sliceB = unsafeSlice ptrsB n entriesB
                dlen = U.length sliceA + U.length sliceB

            -- find the start of the current destination slice
            start <- UM.unsafeRead _ptrs n
            let (ixs, xs) = UM.unzip $ UM.slice start dlen _entries

            -- scatter/gather
            _pop <- unsafeScatter _work a sliceA ixs 0
            _pop <- unsafeScatter _work b sliceB ixs _pop
            unsafeGather _work ixs xs _pop

            -- record the start of the next slice
            UM.unsafeWrite _ptrs (n + 1) (start + _pop)

      U.mapM_ unsafeLin_go $ U.enumFromN 0 odim

      _ptrs <- U.unsafeFreeze _ptrs
      let nz' = U.last _ptrs
      _entries <- U.force <$> U.unsafeFreeze (UM.unsafeSlice 0 nz' _entries)

      return (_ptrs, _entries)

add :: (Num a, Unbox a) => Matrix a -> Matrix a -> Matrix a
{-# INLINE add #-}
add a b = lin 1 a 1 b

axpy_
  :: (GM.MVector v a, Num a, PrimMonad m, Unbox a)
  => Matrix a -> v (PrimState m) a -> v (PrimState m) a -> m ()
{-# INLINE axpy_ #-}
axpy_ Matrix {..} xs ys
  | GM.length xs /= ncols = oops "column dimension does not match operand"
  | GM.length ys /= nrows = oops "row dimension does not match result"
  | otherwise =
      U.forM_ (U.enumFromN 0 ncols) $ \c -> do
        U.forM_ (unsafeSlice pointers c entries) $ \(r, a) -> do
          x <- GM.unsafeRead xs c
          y <- GM.unsafeRead ys r
          GM.unsafeWrite ys r (a * x + y)
  where
    oops str = errorWithStackTrace ("axpy_: " ++ str)

axpy :: (Num a, Unbox a) => Matrix a -> Vector a -> Vector a -> Vector a
{-# SPECIALIZE axpy :: Matrix Double -> Vector Double -> Vector Double -> Vector Double #-}
{-# SPECIALIZE axpy :: Matrix (Complex Double) -> Vector (Complex Double) -> Vector (Complex Double) -> Vector (Complex Double) #-}
axpy = \a _x _y -> runST $ do
  _y <- U.thaw _y
  _x <- U.unsafeThaw _x
  axpy_ a _x _y
  U.unsafeFreeze _y

mulV :: (G.Vector v a, Num a, Unbox a) => Matrix a -> v a -> v a
{-# SPECIALIZE mulV :: Matrix Double -> Vector Double -> Vector Double #-}
{-# SPECIALIZE mulV :: Matrix (Complex Double) -> Vector (Complex Double) -> Vector (Complex Double) #-}
mulV = \a _x -> runST $ do
  _x <- G.unsafeThaw _x
  y <- GM.replicate (nrows a) 0
  axpy_ a _x y
  G.unsafeFreeze y

hjoin :: Unbox a => Matrix a -> Matrix a -> Matrix a
{-# INLINE hjoin #-}
hjoin a b = hcat [a, b]

hcat :: Unbox a => [Matrix a] -> Matrix a
{-# SPECIALIZE hcat :: [Matrix Double] -> Matrix Double #-}
{-# SPECIALIZE hcat :: [Matrix (Complex Double)] -> Matrix (Complex Double) #-}
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

vjoin :: Unbox a => Matrix a -> Matrix a -> Matrix a
{-# INLINE vjoin #-}
vjoin a b = vcat [a, b]

vcat :: Unbox a => [Matrix a] -> Matrix a
{-# SPECIALIZE vcat :: [Matrix Double] -> Matrix Double #-}
{-# SPECIALIZE vcat :: [Matrix (Complex Double)] -> Matrix (Complex Double) #-}
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
          ccount <- U.thaw (U.init _pointers)
          F.forM_ (zip mats rowOffsets) $ \(Matrix {..}, rowOff) -> do
            U.forM_ (U.enumFromN 0 ncols) $ \c -> do
              let sl = U.map (\(row, x) -> (rowOff + row, x))
                       (unsafeSlice pointers c entries)
              dst <- UM.unsafeRead ccount c
              U.copy (UM.slice dst (U.length sl) _entries) sl
              UM.unsafeWrite ccount c (dst + U.length sl)
          return _entries
      }
  where
    oops str = errorWithStackTrace ("vcat: " ++ str)
    _ncols = ncols (head mats)
    _pointers = F.foldr1 (U.zipWith (+)) (map pointers mats)
    -- when concatenating matrices vertically, their row indices must be
    -- offset according to their position in the final matrix
    rowOffsets = L.scanl' (+) 0 (map nrows mats)

fromBlocks :: (Num a, Unbox a) => [[Maybe (Matrix a)]] -> Matrix a
{-# SPECIALIZE fromBlocks :: [[Maybe (Matrix Double)]] -> Matrix Double #-}
{-# SPECIALIZE fromBlocks :: [[Maybe (Matrix (Complex Double))]] -> Matrix (Complex Double) #-}
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
  :: (Num a, Unbox a) => [[Maybe (Matrix a)]] -> Matrix a
{-# INLINE fromBlocksDiag #-}
fromBlocksDiag = fromBlocks . zipWith rejoin [0..] . L.transpose where
  rejoin = \n as -> let (rs, ls) = splitAt (length as - n) as in ls ++ rs

kronecker :: (Num a, Unbox a) => Matrix a -> Matrix a -> Matrix a
{-# SPECIALIZE kronecker :: Matrix Double -> Matrix Double -> Matrix Double #-}
{-# SPECIALIZE kronecker :: Matrix (Complex Double) -> Matrix (Complex Double) -> Matrix (Complex Double) #-}
kronecker matA matB =
  let _nrows = nrows matA * nrows matB
      _ncols = ncols matA * ncols matB
      nz = nonZero matA * nonZero matB

      ptrsA = pointers matA
      ptrsB = pointers matB
      lengthsA = U.zipWith (-) (U.tail ptrsA) ptrsA
      lengthsB = U.zipWith (-) (U.tail ptrsB) ptrsB
      ptrs = U.scanl' (+) 0
             $ G.unstream $ flip S.sized (Exact $ _ncols + 1)
             $ S.concatMap (\nzA -> S.map (* nzA) $ G.stream lengthsB)
             $ G.stream lengthsA

      (indicesA, valuesA) = U.unzip $ entries matA
      (indicesB, valuesB) = U.unzip $ entries matB

      nbs = S.enumFromStepN 0 1 $ ncols matB
      nas = S.enumFromStepN 0 1 $ ncols matA
      ns = S.concatMap (\na -> S.map ((,) na) nbs) nas

      kronecker_ixs (!na, !nb) =
        let as = streamSlice ptrsA na indicesA
            bs = streamSlice ptrsB nb indicesB
        in S.concatMap (\n -> S.map (+ n) bs) $ S.map (* (nrows matB)) as
      ixs = G.unstream $ flip S.sized (Exact nz)
            $ S.concatMap kronecker_ixs ns

      kronecker_xs (!na, !nb) =
        let as = streamSlice ptrsA na valuesA
            bs = streamSlice ptrsB nb valuesB
        in S.concatMap (\a -> S.map (* a) bs) as
      xs = G.unstream $ flip S.sized (Exact nz)
           $ S.concatMap kronecker_xs ns

  in Matrix
    { ncols = _ncols
    , nrows = _nrows
    , pointers = ptrs
    , entries = U.zip ixs xs
    }

takeDiag :: (Num a, Unbox a) => Matrix a -> Vector a
{-# INLINE takeDiag #-}
takeDiag = \mat@Matrix {..} ->
  flip U.map (U.enumFromN 0 $ min nrows ncols) $ \m ->
    let (indices, values) = U.unzip $ S.entries $ slice mat m
    in case U.elemIndex m indices of
      Nothing -> 0
      Just ix -> values U.! ix

diag :: Unbox a => Vector a -> Matrix a
{-# INLINE diag #-}
diag values = Matrix{..}
  where
    ncols = U.length values
    nrows = ncols
    pointers = U.iterateN (ncols + 1) (+1) 0
    indices = U.iterateN ncols (+1) 0
    entries = U.zip indices values

blockDiag :: (Num a, Unbox a) => [Matrix a] -> Matrix a
{-# INLINE blockDiag #-}
blockDiag mats
  = fromBlocksDiag ((Just <$> mats) : replicate (len - 1) (replicate len Nothing))
  where
    len = length mats

ident :: (Num a, Unbox a) => Int -> Matrix a
{-# INLINE ident #-}
ident n = diag $ U.replicate n 1

zeros :: Unbox a => Int -> Int -> Matrix a
{-# INLINE zeros #-}
zeros nrows ncols = Matrix {..}
  where
    pointers = U.replicate (ncols + 1) 0
    indices = U.empty
    values = U.empty
    entries = U.zip indices values

pack :: (Dense.Container Dense.Vector a, Num a, Storable a, Unbox a) => Matrix a -> Dense.Matrix a
{-# INLINE pack #-}
pack Matrix {..} =
  Dense.assoc (nrows, ncols) 0 $ do
    c <- [0..(ncols - 1)]
    begin <- U.unsafeIndexM pointers c
    end <- U.unsafeIndexM pointers (c + 1)
    let len = end - begin
        col = U.slice begin len entries
    U.toList (U.map (\(r, x) -> ((r, c), x)) col)
