{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Numeric.LinearAlgebra.Sparse
    ( Matrix
    , Unbox
    , OrientK(..), Orient(..)
    , Format(..), FormatK(..)
    , dim, dimF, nonzero
    , reorder, transpose, adjoint
    , pack, unpack, deduplicate
    , compress, compressed, uncompressed
    , compressed', uncompressed'
    , _slices, _rows, _cols, slice
    , empty, diag, ident
    , mulV, mulVM, mul, add
    ) where

import Control.Applicative hiding (empty)
import Control.Lens
import Control.Monad (liftM2, when)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.ST (runST)
import Data.Complex
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid
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

class Format (fmt :: FormatK) where
    -- | One ring to rule them all
    --   One ring to find them
    --   One ring to bring them all
    --   And in the darkness bind them
    --   In the land or Mordor, where the shadows lie.
    formats :: (Functor f, Profunctor p, Profunctor q)
            => Optical p q f (Matrix U or a) (Matrix U or b) c d
            -> Optical p q f (Matrix C or a) (Matrix C or b) c d
            -> Optical p q f (Matrix fmt or a) (Matrix fmt or b) c d

instance Format U where
    formats f _ = f

instance Format C where
    formats _ f = f

-- | Convert between uncompressed and compressed matrix formats.  The most
-- useful thing about this 'Iso' is that it's forgetful in reverse: if you
-- know you have an uncompressed matrix, you can still return a matrix of
-- *any* format. That sounds like you'll be doing a lot a extra
-- compression/decompression, but not if you use 'formats', which will
-- automatically select the side that won't do any work!
uncompressed :: (Format fmt, Orient or, Unbox a)
             => Iso' (Matrix fmt or a) (Matrix U or a)
uncompressed = formats (iso id id) (from compress)

-- | The other side of 'uncompressed'.
compressed :: (Format fmt, Orient or, Unbox a)
           => Iso' (Matrix fmt or a) (Matrix C or a)
compressed = formats compress (iso id id)

compressed' :: (Format fmt, Orient or, Unbox a)
            => Prism' (Matrix fmt or a) (Matrix C or a)
compressed' = formats (prism' (view uncompressed) (const Nothing))
                      (prism' id Just)

uncompressed' :: (Format fmt, Orient or, Unbox a)
              => Prism' (Matrix fmt or a) (Matrix U or a)
uncompressed' = formats (prism' id Just)
                        (prism' (view compressed) (const Nothing))

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

dim :: (Format fmt, Orient or, Unbox a) => Lens' (Matrix fmt or a) (Int, Int)
dim = formats dimU dimC
  where
    dimU = lens getDimU setDimU
    getDimU (MatU ux) = let Ux r c _ = untag ux in (r, c)
    setDimU (MatU ux) (r', c') =
        MatU $ unproxy $ \witness ->
            let Ux r c vals = proxy ux witness
                inBounds (i, j, _) = i < r' && j < c'
                truncateOutOfBounds | r' < r || c' < c = U.filter inBounds
                                    | otherwise = id
            in Ux r' c' $ truncateOutOfBounds vals
    dimC = lens getDimC setDimC
    getDimC mat@(MatC cx) =
        untag $ unproxy $ \witness ->
            let _ = proxy cx witness
            in reorient witness $ mat ^. dimF
    setDimC mat@(MatC cx) dim' =
        untag $ unproxy $ \witness ->
            let _ = proxy cx witness
            in mat & dimF .~ reorient witness dim'

dimF :: (Format fmt, Orient or, Unbox a) => Lens' (Matrix fmt or a) (Int, Int)
dimF = formats dimU dimC
  where
    dimU = lens getDimU setDimU
    getDimU mat@(MatU ux) =
        untag $ unproxy $ \witness ->
            let _ = proxy ux witness
            in reorient witness $ mat ^. dim
    setDimU mat@(MatU ux) dim' =
        untag $ unproxy $ \witness ->
            let _ = proxy ux witness
            in mat & dim .~ reorient witness dim'
    dimC = lens getDimC setDimC
    getDimC (MatC cx) =
        untag $ unproxy $ \witness ->
            let Cx minor ixs _ = proxy cx witness
            in (U.length ixs, minor)
    setDimC mat@(MatC cx) (major', minor') =
        MatC $ unproxy $ \witness ->
            let Cx _ ixs vals = proxy cx witness
                (major, minor) = getDimC mat
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

nonzero :: (Format fmt, Unbox a) => Getter (Matrix fmt or a) Int
nonzero = formats (to nonzeroU) (to nonzeroC)
  where
    nonzeroU (MatU ux) = let Ux _ _ triples = untag ux in U.length triples
    nonzeroC (MatC cx) = let Cx _ _ vals = untag cx in U.length vals

reorder :: (Format fmt, Unbox a) => Iso' (Matrix fmt Col a) (Matrix fmt Row a)
reorder = formats (reorderU . from uncompressed) (reorderC . from compressed)
--reorder = view $ formats (reorderU . from uncompressed) reorderC
  where
    reorderU_ (MatU ux) = MatU $ unproxy $ \witness ->
        sortUx witness $ untag ux
    reorderU :: Unbox a => Iso' (Matrix U Col a) (Matrix U Row a)
    reorderU = iso reorderU_ reorderU_
    reorderC :: Unbox a => Iso' (Matrix C Col a) (Matrix C Row a)
    reorderC = from compress . reorderU . compress . from compressed

transpose :: (Format fmt, Unbox a) => Iso' (Matrix fmt Row a) (Matrix fmt Col a)
transpose = formats (iso transposeU transposeU . from uncompressed) (iso transposeC transposeC . from compressed)
  where
    transposeU :: Unbox a => Matrix U or a -> Matrix U or' a
    transposeU (MatU ux) =
        let Ux r c vals = untag ux
            (rows, cols, coeffs) = U.unzip3 vals
        in MatU $ tag Proxy $ Ux c r $ U.zip3 cols rows coeffs
    transposeC :: Matrix C or a -> Matrix C or' a
    transposeC (MatC cx) = MatC $ tag Proxy $ untag cx

outOfBounds :: String -> Int -> (Int, Int) -> String
outOfBounds prefix i bnds =
    prefix ++ " " ++ show i ++ " out of bounds " ++ show bnds

slice :: (Format fmt, Functor f, Orient or, Unbox a)
      => Int -> LensLike' f (Matrix fmt or a) (Slice a)
slice i = formats (lens sliceGU sliceSU) (lens sliceGC sliceSC)
  where
    sliceGU mat@(MatU ux)
        | i < view (dimF . _1) mat =
            untag $ unproxy $ \witness ->
                let Ux _ _ vals = proxy ux witness
                    (start, end) = getSliceExtentsU mat
                    (_, minors, coeffs) =
                        reorient witness
                        $ U.unzip3
                        $ U.slice start (end - start) vals
                in U.zip minors coeffs
        | otherwise = error $ outOfBounds "sliceGU:" i (mat ^. dimF)
    sliceSU mat@(MatU ux) sl
        | i < view (dimF . _1) mat =
            MatU $ unproxy $ \witness ->
                let Ux r c vals = proxy ux witness
                    (start, end) = getSliceExtentsU mat
                    (minors, coeffs) = U.unzip sl
                    majors = U.replicate (U.length sl) i
                    (rows, cols) = reorient witness (majors, minors)
                    (prefix, _) = U.splitAt start vals
                    (_, suffix) = U.splitAt end vals
                in Ux r c $ prefix U.++ (U.zip3 rows cols coeffs) U.++ suffix
        | otherwise = error $ outOfBounds "sliceSU:" i (mat ^. dimF)
    getSliceExtentsU (MatU ux) =
        untag $ unproxy $ \witness ->
            let Ux _ _ vals = proxy ux witness
                (majors, _, _) = reorient witness $ U.unzip3 vals
            in runST $ do
                majors_ <- U.thaw majors
                (,) <$> binarySearchL majors_ i
                    <*> binarySearchL majors_ (succ i)
    sliceGC mat@(MatC cx)
        | i < U.length starts = U.slice start (end - start) vals
        | otherwise = error $ outOfBounds "sliceGC:" i (mat ^. dimF)
      where
        Cx _ starts vals = untag cx
        start = starts U.! i
        end = fromMaybe (U.length vals) $ starts U.!? succ i
    sliceSC mat@(MatC cx) sl
        | i < (mat ^. dimF . _1) =
            MatC $ unproxy $ \witness ->
                let Cx minor starts vals = proxy cx witness
                    start = starts U.! i
                    end = fromMaybe (U.length vals) $ starts U.!? succ i
                    dLen = U.length sl - (end - start)
                    starts' = flip U.imap starts $ \j x ->
                        if j > i then (x + dLen) else x
                    prefix = U.take start vals
                    suffix = U.drop end vals
                    vals' = prefix U.++ sl U.++ suffix
                in Cx minor starts' vals'
        | otherwise = error $ outOfBounds "sliceSC:" i (mat ^. dimF)

instance (Eq a, Format fmt, Orient or, Unbox a) => Eq (Matrix fmt or a) where
    (==) = view $ formats (to goU) (to goC)
      where
        goU (MatU a) (view uncompressed -> MatU b) = untag a == untag b
        goC (MatC a) (view compressed -> MatC b) = untag a == untag b

generate :: Int -> (Int -> a) -> [a]
generate len f = map f $ take len $ [0..]

empty :: (Format fmt, Orient or, Unbox a) => Matrix fmt or a
empty = view (from uncompressed) $ pack 1 1 $ U.empty

_rows :: (Format fmt, Unbox a, Unbox b)
      => Traversal (Matrix fmt Row a) (Matrix fmt Row b) (Slice a) (Slice b)
_rows = _slices

_cols :: (Format fmt, Unbox a, Unbox b)
      => Traversal (Matrix fmt Col a) (Matrix fmt Col b) (Slice a) (Slice b)
_cols = _slices

insertSlice :: (Format fmt, Orient or, Unbox a)
            => Int -> Matrix fmt or a -> Slice a -> Matrix fmt or a
insertSlice i mat sl = mat & dimF . _1 %~ (max (succ i))
                           & dimF . _2 %~ (max (succ j))
                           & slice i .~ sl
  where
    j = U.foldl' max 0 $ fst $ U.unzip sl

loop :: Int -> b -> (b -> Int -> b) -> b
loop n acc f = foldl' f acc [0..(n - 1)]

_slices :: (Format fmt, Orient or, Unbox a, Unbox b)
        => Traversal (Matrix fmt or a) (Matrix fmt or b) (Slice a) (Slice b)
_slices f mat =
    loop (mat ^. dimF . _1) (pure $ empty & dim .~ (mat ^. dim))
    $ \acc i -> insertSlice i <$> acc <*> f (mat ^. slice i)

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

pack :: (Format fmt, Orient or, Unbox a)
     => Int -> Int -> Vector (Int, Int, a) -> Matrix fmt or a
pack r c v
    | not (r > 0) = error "pack: row dimension must be positive!"
    | not (c > 0) = error "pack: column dimension must be positive!"
    | U.any outOfBounds v = error "pack: Index out of bounds!"
    | otherwise = view (from uncompressed) $ MatU $ unproxy $ \witness -> sortUx witness $ Ux r c v
  where
    outOfBounds (i, j, _) = i >= r || i < 0 || j >= c || j < 0

diag :: (Format fmt, Orient or, Unbox a) => Vector a -> Matrix fmt or a
diag v =
    let len = U.length v
        ixs = U.enumFromN 0 len
    in pack len len $ U.zip3 ixs ixs v

ident :: (Format fmt, Num a, Orient or, Unbox a) => Int -> Matrix fmt or a
ident i = diag $ U.replicate i 1

mulV  :: (Num a, Unbox a, V.Vector v a) => Matrix C Row a -> v a -> v a
mulV mat xs_
    | c == U.length xs =
        V.convert $ U.create $ do
            ys <- MU.new r
            iforMOf_ (indexing rows) mat $ \ixR row -> do
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
        iforMOf_ (indexing rows) mat $ \i row -> do
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
    expand ls rs =
        pack left right
        $ U.concatMap (\(r, x) -> U.map (\(c, y) -> (r, c, x * y)) rs) ls

add :: (Format fmt, Num a, Orient or, Unbox a)
    => Matrix fmt or a -> Matrix fmt or a -> Matrix fmt or a
add a b
    | a ^. dim == b ^. dim = deduplicate $ mappend a b
    | otherwise = error $ "add: matrix dimension " ++ show (a ^. dim)
                        ++ " does not match " ++ show (b ^. dim)

-- This is spitting out a matrix whose dimensions don't match the input
deduplicate :: (Format fmt, Num a, Orient or, Unbox a)
            => Matrix fmt or a -> Matrix fmt or a
deduplicate = over _slices deduplicateSlice

deduplicateSlice :: (Num a, Unbox a) => Vector (Int, a) -> Vector (Int, a)
deduplicateSlice v_ = runST $ do
    v <- U.thaw v_
    len <- deduplicateSliceM v
    U.freeze $ MU.slice 0 len v

deduplicateSliceM :: (Monad m, Num a, PrimMonad m, Unbox a)
                  => MVector (PrimState m) (Int, a) -> m Int
deduplicateSliceM dst = do
    let (ns, xs) = MU.unzip dst
        len = MU.length dst
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

    sortBy (comparing fst) dst
    accumulate 0 1
    sortBy (comparing fst) dst
    start <- binarySearchL (fst $ MU.unzip dst) 0
    let len' = len - start
    when (start > 0) $ MU.move (MU.slice 0 len' dst) (MU.slice start len' dst)
    return $ min len len'

instance (Format fmt, Unbox a, Unbox b) =>
    Each (Matrix fmt ord a) (Matrix fmt ord b) a b where
    each = formats eachU eachC
      where
        eachU f (MatU ux) =
            let Ux r c vals = untag ux
            in (MatU . copyTag ux . Ux r c) <$> (each . _3) f vals
        eachC f (MatC cx) =
            let Cx mnr ixs vals = untag cx
            in (MatC . copyTag cx . Cx mnr ixs) <$> (each . _2) f vals

adjoint :: (Format fmt, RealFloat a, Unbox a)
        => Iso' (Matrix fmt Row (Complex a)) (Matrix fmt Col (Complex a))
adjoint = conjugate' . transpose
  where
    --conjugate' :: (FormatR fmt, RealFloat a, Unbox a) => Iso' (Matrix fmt Row (Complex a)) (Matrix fmt Row (Complex a))
    conjugate' = iso (over each conjugate) (over each conjugate)

unpack :: Matrix U or a -> Vector (Int, Int, a)
unpack (MatU ux) = let Ux _ _ triples = untag ux in triples

-- | 'mempty' is just 'empty' and 'mappend' is enlarging, duplicating addition
instance (Format fmt, Orient or, Unbox a) => Monoid (Matrix fmt or a) where

    mempty = empty

    mappend ma mb = runST $ do
        -- Initialize work space
        valsC <- MU.new nnzC
        startsC <- MU.new mjrC
        MU.set startsC nnzC

        -- Copy the values into place
        let copySlicesFrom i startC
                | i < mjrC = do
                    let sliceA | i < mjrA = ma ^. slice i
                               | otherwise = U.empty
                        lenA = U.length sliceA
                        sliceB | i < mjrB = mb ^. slice i
                               | otherwise = U.empty
                        lenB = U.length sliceB
                        sliceCA = MU.slice startC lenA valsC
                        sliceCB = MU.slice (startC + lenA) lenB valsC
                    U.copy sliceCA sliceA
                    U.copy sliceCB sliceB
                    MU.write startsC i startC
                    copySlicesFrom (succ i) (startC + lenA + lenB)
                | otherwise = return ()
        copySlicesFrom 0 0

        -- Freeze work space into new matrix
        starts <- U.unsafeFreeze startsC
        vals <- U.unsafeFreeze valsC
        return $! view (from compressed)
                $ MatC $ tag Proxy $ Cx mnrC starts vals
      where
        (mjrA, mnrA) = ma ^. dimF
        (mjrB, mnrB) = mb ^. dimF
        mjrC = max mjrA mjrB
        mnrC = max mnrA mnrB
        nnzC = ma ^. nonzero + mb ^. nonzero
