{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.LinearAlgebra.Sparse
    ( Matrix
    , Unbox
    , OrientK(..), Orient(..)
    , Format(..), FormatK(..), FormatR(..)
    , compress, toU
    , slicesF, rowsF, colsF
    , pack, adjoint, unpack
    , empty, diag, ident
    , mulV, mulVM, mul, add
    ) where

import Control.Applicative hiding (empty)
import Control.Exception (assert)
import Control.Lens
import Control.Monad (liftM2, when)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.ST (runST)
import Data.Complex
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Vector.Algorithms.Intro (sortBy)
import Data.Vector.Algorithms.Search (binarySearchL)

import Data.Proxy.PolyKind

-- | Matrix order
data OrientK
    = Row  -- ^ row-major
    | Col -- ^ column-major

class Orient (ord :: OrientK) where
    -- | Operate on a (major, minor, ...) tuple as if it were in
    -- (row, column, ...) order, or vice versa
    reorient :: (Field1 s s a a, Field2 s s a a)
             => Proxy ord -> s -> s

instance Orient Row where
    reorient Proxy = id
    {-# INLINE reorient #-}

instance Orient Col where
    reorient Proxy x = x & _1 .~ (x ^. _2) & _2 .~ (x ^. _1)
    {-# INLINE reorient #-}

-- | Matrix formats
data FormatK
    = U -- ^ uncompressed
    | C -- ^ compressed

-- | Compressed sparse format
data Cx a = Cx !Int -- ^ minor dimension
               !(Vector Int) -- ^ starting indices of each major slice
               !(Vector (Int, a)) -- ^ (minor index, coefficient)
  deriving (Show)

instance (Eq a, Unbox a) => Eq (Cx a) where
    (==) (Cx minorA ixsA valsA) (Cx minorB ixsB valsB) =
        minorA == minorB && ixsA == ixsB && valsA == valsB
    {-# INLINE (==) #-} -- Just boilerplate

-- | Uncompressed sparse format
data Ux a = Ux !Int -- ^ row dimension
               !Int -- ^ column dimension
               !(Vector (Int, Int, a))
               -- ^ (row index, column index, coefficient)
  deriving (Show)

instance (Eq a, Unbox a) => Eq (Ux a) where
    (==) (Ux nRowsA nColsA coeffsA) (Ux nRowsB nColsB coeffsB) =
        nRowsA == nRowsB && nColsA == nColsB && coeffsA == coeffsB
    {-# INLINE (==) #-} -- Just boilerplate

data family Matrix :: FormatK -> OrientK -> * -> *
newtype instance Matrix C ord a = MatC (Tagged ord (Cx a))
  deriving (Show)
newtype instance Matrix U ord a = MatU (Tagged ord (Ux a))
  deriving (Show)

type Slice a = Vector (Int, a)

-- | A class for matrix traversals dependent on storage format. These
-- traversals have passive-voice names to indicate that they do no work,
-- i.e., using the traversal 'uncompressed' on a compressed matrix will
-- traverse no elements. Each instance of this class must have exactly one
-- matching traversal. You can write a function taking a matrix in any
-- format by traversing the input with each of these traversals, providing
-- a distinct implementation of each. For example:
-- @
-- f :: Format fmt => Matrix fmt or a -> Matrix fmt or a
-- f = forOf uncompressed uncompressedImplementation
--   . forOf compressed compressedImplementation
--   where
--     uncompressedImplementation :: Matrix U or a -> Matrix U or a
--     uncompressedImplementation = ...
--     compressedImplementation :: Matrix C or a -> Matrix C or a
--     compressedImplementation = ...
-- @
class Format (fmt :: FormatK) where
    demote :: Prism' (Either (Matrix U or a) (Matrix C or a)) (Matrix fmt or a)

instance Format U where
    demote = prism' Left (either Just (const Nothing))

instance Format C where
    demote = prism' Right (either (const Nothing) Just)

uncompressed :: Prism' (Matrix fmt or a) (Matrix U or a)
uncompressed = undefined

compressed :: Prism' (Matrix fmt or a) (Matrix C or a)
compressed = undefined

fromU :: (Format fmt, Orient or, Unbox a) => Matrix U or a -> Matrix fmt or a
fromU = view (from compress . re compressed)

toU :: (Format fmt, Orient or, Unbox a) => Matrix fmt or a -> Matrix U or a
toU = views uncompressed id
    . views compressed (^. from compress)

compress :: (Orient or, Unbox a) => Iso' (Matrix U or a) (Matrix C or a)
compress = iso compressU decompressC
  where
    compressU mat@(MatU ux) = MatC $ unproxy $ \witness ->
      let (Ux _ _ vals) = proxy ux witness
          (m, n) = mat ^. dimF
          (majors, minors, coeffs) = reorient witness $ U.unzip3 vals
          ixs = runST $ do
            majors_ <- U.thaw majors
            U.generateM m $ binarySearchL majors_
      in Cx n ixs $ U.zip minors coeffs
    decompressC mat@(MatC cx) = MatU $ unproxy $ \witness ->
      let (Cx _ ixs vals) = proxy cx witness
          (minors, coeffs) = U.unzip vals
          nnz = U.length vals
          lengths = flip U.imap ixs $ \m start ->
              let next = fromMaybe nnz (ixs U.!? succ m)
              in next - start
          majors =
              U.concatMap (\(r, len) -> U.replicate len r)
              $ U.indexed lengths
          (rows, cols) = reorient witness (majors, minors)
          (nRows, nCols) = view dim mat
      in Ux nRows nCols $ U.zip3 rows cols coeffs

class FormatR (fmt :: FormatK) where
    -- | The dimensions of a matrix in (row, column) orer.
    dim :: (Orient or, Unbox a) => Lens' (Matrix fmt or a) (Int, Int)

    -- | The dimensions of a matrix in (major, minor) orer.
    dimF :: (Orient or, Unbox a) => Lens' (Matrix fmt or a) (Int, Int)

    -- | The number of non-zero entries in the matrix.
    nonzero :: Unbox a => Matrix fmt or a -> Int

    transpose :: (Orient or, Unbox a) => Matrix fmt or a -> Matrix fmt or a
    reorder :: (Orient or, Orient or', Unbox a) => Matrix fmt or a -> Matrix fmt or' a

    slice :: (Functor f, Orient or, Unbox a)
          => Int
          -> LensLike f (Matrix fmt or a) (Matrix fmt or a) (Slice a) (Slice a)

    _eq :: (Eq a, Unbox a) => Matrix fmt or a -> Matrix fmt or a -> Bool
    _each :: (Unbox a, Unbox b)
          => Traversal (Matrix fmt or a) (Matrix fmt or b) a b

instance FormatR U where
    dim = lens getDim setDim
      where
        getDim (MatU ux) = let Ux r c _ = untag ux in (r, c)
        setDim (MatU ux) (r', c') =
            MatU $ unproxy $ \witness ->
                let Ux r c vals = proxy ux witness
                    inBounds (i, j, _) = i < r' && j < c'
                    truncateOutOfBounds | r' < r || c' < c = U.filter inBounds
                                        | otherwise = id
                in Ux r' c' $ truncateOutOfBounds vals

    dimF = lens getDim setDim
      where
        getDim mat@(MatU ux) =
            untag $ unproxy $ \witness ->
                let _ = proxy ux witness
                in reorient witness $ mat ^. dim
        setDim mat@(MatU ux) dim' =
            untag $ unproxy $ \witness ->
                let _ = proxy ux witness
                in mat & dim .~ reorient witness dim'

    nonzero (MatU ux) = let Ux _ _ vals = untag ux in U.length vals

    transpose (MatU ux) =
        MatU $ unproxy $ \witness ->
            let (Ux r c vals) = proxy ux witness
            in sortUx witness $ Ux c r $ U.map (\(x, y, z) -> (y, x, z)) vals

    reorder (MatU mat) = MatU $ unproxy $ \witness -> sortUx witness $ untag mat

    slice i = lens sliceG sliceS
      where
        sliceG mat@(MatU ux)
            | i < view (dimF . _1) mat =
                untag $ unproxy $ \witness ->
                    let Ux _ _ vals = proxy ux witness
                        (start, end) = getSliceExtentsU mat
                        (_, minors, coeffs) =
                            reorient witness
                            $ U.unzip3
                            $ U.slice start (end - start) vals
                    in U.zip minors coeffs
            | otherwise = error "sliceG: index out of bounds!"
        sliceS mat@(MatU ux) sl =
            MatU $ unproxy $ \witness ->
                let Ux r c vals = proxy ux witness
                    (start, end) = getSliceExtentsU mat
                    (minors, coeffs) = U.unzip sl
                    majors = U.replicate (U.length sl) i
                    (rows, cols) = reorient witness (majors, minors)
                    (prefix, _) = U.splitAt start vals
                    (_, suffix) = U.splitAt end vals
                in Ux r c $ prefix U.++ (U.zip3 rows cols coeffs) U.++ suffix
        getSliceExtentsU (MatU ux) =
            untag $ unproxy $ \witness ->
                let Ux _ _ vals = proxy ux witness
                    (majors, _, _) = reorient witness $ U.unzip3 vals
                in runST $ do
                    majors_ <- U.thaw majors
                    (,) <$> binarySearchL majors_ i
                        <*> binarySearchL majors_ (succ i)

    _eq (MatU a) (MatU b) = untag a == untag b

    _each f (MatU ux) =
        let Ux r c vals = untag ux
        in (MatU . copyTag ux . Ux r c) <$> (each . _3) f vals

instance FormatR C where
    dimF = lens getDimF setDimF
      where
        getDimF (MatC cx) =
            untag $ unproxy $ \witness ->
                let Cx minor ixs _ = proxy cx witness
                in (U.length ixs, minor)
        setDimF mat@(MatC cx) (major', minor') =
            MatC $ unproxy $ \witness ->
                let Cx _ ixs vals = proxy cx witness
                    (major, minor) = getDimF mat
                    nnz = U.length vals
                    truncateMinor
                        | minor' < minor = U.filter (\(j, _) -> j < minor')
                        | otherwise = id
                    truncateMajor
                        | major' < major =
                            U.take (fromMaybe nnz $ ixs U.!? succ major')
                        | otherwise = id
                    vals' = truncateMinor $ truncateMajor vals
                    ixs' = fixIxs $ case compare major' major of
                        EQ -> ixs
                        LT -> U.take major' ixs
                        GT -> ixs U.++ U.replicate (major' - major) nnz
                    fixIxs starts
                        | minor' < minor = flip U.map starts $ \start ->
                            let removed =
                                    U.length
                                    $ U.findIndices ((>= minor') . view _1)
                                    $ U.slice 0 start vals
                            in start - removed
                        | otherwise = starts
                in Cx minor' ixs' vals'

    dim = lens getDim setDim
      where
        getDim mat@(MatC cx) =
            untag $ unproxy $ \witness ->
                let _ = proxy cx witness
                in reorient witness $ mat ^. dimF
        setDim mat@(MatC cx) dim' =
            untag $ unproxy $ \witness ->
                let _ = proxy cx witness
                in mat & dimF .~ reorient witness dim'

    nonzero (MatC cx) = let Cx _ _ vals = untag cx in U.length vals

    transpose = view $ from compress . to transpose . compress
    reorder = view $ from compress . to reorder . compress

    slice i = lens sliceG sliceS
      where
        sliceG (MatC cx)
            | i < U.length starts = U.slice start (end - start) vals
            | otherwise = error "sliceG: major index out of bounds"
          where
            Cx _ starts vals = untag cx
            start = starts U.! i
            end = fromMaybe (U.length vals) $ starts U.!? succ i

        sliceS (MatC cx) sl =
            MatC $ unproxy $ \witness ->
                let Cx minor starts vals = proxy cx witness
                    start = starts U.! i
                    end = fromMaybe (U.length vals) $ starts U.!? i
                    dLen = U.length sl - (end - start)
                    starts' = flip U.imap starts $ \j x ->
                        if j > i then (x + dLen) else x
                    prefix = U.take start vals
                    suffix = U.drop end vals
                    vals' = prefix U.++ sl U.++ suffix
                in if i < U.length starts
                      then Cx minor starts' vals'
                      else error "sliceS: major index out of bounds"

    _eq (MatC a) (MatC b) = untag a == untag b

    _each f (MatC cx) =
        let Cx mnr ixs vals = untag cx
        in (MatC . copyTag cx . Cx mnr ixs) <$> (each . _2) f vals

instance (Eq a, FormatR fmt, Unbox a) => Eq (Matrix fmt ord a) where
    (==) = _eq

generate :: Int -> (Int -> a) -> [a]
generate len f = map f $ take len $ [0..]

empty :: (Format fmt, FormatR fmt, Orient or, Unbox a) => Matrix fmt or a
empty = fromU $ pack 1 1 $ U.empty

-- | Fold over the slices in a matrix using 'sliceG'.
slicesF :: (FormatR fmt, Orient or, Unbox a)
        => Fold (Matrix fmt or a) (Slice a)
slicesF = folding $ \mat -> generate (mat ^. dimF . _1) $ \i -> mat ^. slice i

rowsF :: (FormatR fmt, Unbox a) => Fold (Matrix fmt Row a) (Slice a)
rowsF = slicesF

colsF :: (FormatR fmt, Unbox a) => Fold (Matrix fmt Col a) (Slice a)
colsF = slicesF

sortUx :: (Orient or, Unbox a) => Proxy or -> Ux a -> Ux a
sortUx witness (Ux nr nc triples) =
    let (majors, minors, coeffs) = reorient witness $ U.unzip3 triples
        (rows', cols', coeffs') =
            reorient witness
            $ U.unzip3
            $ U.modify (sortBy comparator)
            $ U.zip3 majors minors coeffs
    in Ux nr nc $ U.zip3 rows' cols' coeffs'
  where
    comparator a b = comparing (view _1) a b <> comparing (view _2) a b

pack :: (Format fmt, FormatR fmt, Orient or, Unbox a)
     => Int -> Int -> Vector (Int, Int, a) -> Matrix fmt or a
pack r c v
    | not (r > 0) = error "pack: row dimension must be positive!"
    | not (c > 0) = error "pack: column dimension must be positive!"
    | U.any outOfBounds v = error "pack: Index out of bounds!"
    | otherwise = fromU $ MatU $ unproxy $ \witness -> sortUx witness $ Ux r c v
  where
    outOfBounds (i, j, _) = i >= r || i < 0 || j >= c || j < 0

diag :: (Format fmt, FormatR fmt, Orient or, Unbox a) => Vector a -> Matrix fmt or a
diag v =
    let len = U.length v
        ixs = U.enumFromN 0 len
    in pack len len $ U.zip3 ixs ixs v

ident :: (Format fmt, FormatR fmt, Num a, Orient or, Unbox a) => Int -> Matrix fmt or a
ident i = diag $ U.replicate i 1

mulV  :: (Num a, Unbox a, V.Vector v a) => Matrix C Row a -> v a -> v a
mulV mat xs_
    | c == U.length xs =
        V.convert $ U.create $ do
            ys <- MU.new r
            iforMOf_ (indexing rowsF) mat $ \ixR row -> do
              let (cols, coeffs) = U.unzip row
              MU.write ys ixR
                $ U.sum $ U.zipWith (*) coeffs
                $ U.backpermute xs cols
            return ys
    | otherwise = error "mulV: matrix width does not match vector length!"
  where
    xs = V.convert xs_
    (r, c) = view dim mat

mulVM :: (MV.MVector v a, Num a, PrimMonad m, Unbox a)
      => Matrix C Row a -> v (PrimState m) a -> v (PrimState m) a -> m ()
mulVM mat src dst
    | c /= MV.length src =
        error "mulVM: input vector dimension does not match matrix width"
    | r /= MV.length dst =
        error "mulVM: output vector dimension does not match matrix height"
    | otherwise =
        iforMOf_ (indexing rowsF) mat $ \i row -> do
            let (cols, coeffs) = U.unzip row
            x <- U.mapM (MV.read src) cols
            MV.write dst i $ U.sum $ U.zipWith (*) coeffs x
  where
    (r, c) = view dim mat

mul :: (Num a, Orient or, Unbox a)
    => Matrix C Col a -> Matrix C Row a -> Matrix C or a
mul a b
    | inner == inner' =
        foldl' add empty_ $ generate inner $ \i -> expand (a ^. slice i) (b ^. slice i)
    | otherwise = error "mul: matrix inner dimensions do not match!"
  where
    (inner, left) = view dimF a
    (inner', right) = view dimF b
    empty_ = set dim (left, right) empty
    expand :: (Num a, Orient or, Unbox a)
           => Vector (Int, a) -> Vector (Int, a) -> Matrix C or a
    expand ls rs = pack left right $ U.concatMap (\(r, x) -> U.map (\(c, y) -> (r, c, x * y)) rs) ls

add :: (Num a, Orient or, Unbox a)
    => Matrix C or a -> Matrix C or a -> Matrix C or a
add a b =
    let (majorA, minorA) = view dimF a
        (valsC, ixsC) = runST $ do
          vals <- MU.new $ nonzero a + nonzero b
          ixs <- MU.new majorA
          let go i start
                | i < majorA = do
                  MU.unsafeWrite ixs i start
                  let sliceA = a ^. slice i
                      sliceB = b ^. slice i
                  len <- addSlicesInto
                    (MU.slice start (U.length sliceA + U.length sliceB) vals)
                    sliceA sliceB
                  go (succ i) (start + len)
                | otherwise = return start
          len <- go 0 0
          (,) <$> U.unsafeFreeze (MU.unsafeSlice 0 len vals) <*> U.unsafeFreeze ixs
    in assert (view dimF a == view dimF b)
        $ MatC $ tag Proxy $ Cx minorA ixsC valsC
  where
    addSlicesInto :: (Monad m, Num a, PrimMonad m, Unbox a)
                  => MVector (PrimState m) (Int, a)
                  -> Vector (Int, a)
                  -> Vector (Int, a)
                  -> m Int
    addSlicesInto dst src1 src2 = do
      let len1 = U.length src1
          len2 = U.length src2
          len = MU.length dst
      MU.move (MU.slice 0 len1 dst) =<< U.thaw src1
      MU.move (MU.slice len1 len2 dst) =<< U.thaw src2
      sortBy (comparing fst) dst

      -- Accumulate elements in same minor dimension by adding their
      -- coefficients together. Set the minor dimension of duplicate
      -- elements to (-1) so we can find them later.
      let (ns, xs) = MU.unzip dst -- unzip to avoid looking up both fields
          accumulate update check =
            when (check < len) $ do
              -- do the elements have the same minor dimension?
              -- i.e., is one a duplicate?
              dup <- liftM2 (==) (MU.read ns update) (MU.read ns check)
              if dup
                then do
                  -- add the duplicate coefficients
                  x <- liftM2 (+) (MU.read xs update) (MU.read xs check)
                  -- insert new coefficient in place of first duplicate
                  MU.write xs update x
                  -- mark second duplicate for removal
                  MU.write ns check (-1)
                  accumulate update (succ check)
                else accumulate check (succ check)
      accumulate 0 1

      sortBy (comparing fst) dst
      start <- binarySearchL (fst $ MU.unzip dst) 0
      let len' = len - start
      when (start > 0) $ MU.move (MU.slice 0 len' dst) (MU.slice start len' dst)
      return len'

instance (FormatR fmt, Unbox a, Unbox b) =>
    Each (Matrix fmt ord a) (Matrix fmt ord b) a b where
    each = _each

adjoint :: (FormatR fmt, Orient or, RealFloat a, Unbox a)
        => Matrix fmt or (Complex a) -> Matrix fmt or (Complex a)
adjoint = over each conjugate . transpose

unpack :: Matrix U or a -> Vector (Int, Int, a)
unpack (MatU ux) = let Ux _ _ triples = untag ux in triples
