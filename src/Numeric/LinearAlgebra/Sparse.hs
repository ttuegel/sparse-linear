{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.LinearAlgebra.Sparse
    ( Matrix
    , Unbox
    , OrderK(..), OrderR(..)
    , FormatK(..), FormatR(..)
    , slicesF, rowsF, colsF, slice
    , pack, reorder, adjoint
    , empty, diag, ident
    , mulV, mulVM, mul, add
    ) where

import Control.Applicative hiding (empty)
import Control.Exception (assert)
import Control.Lens
import Control.Monad (liftM, liftM2, when)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.ST (runST)
import Data.AEq
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
import Data.Vector.Algorithms.Search (binarySearchL, binarySearchLBy)

import Data.Proxy.PolyKind

-- | Matrix order
data OrderK
    = Row -- ^ row-major
    | Col -- ^ column-major

-- | Matrix formats
data FormatK
    = U -- ^ uncompressed
    | C -- ^ compressed

class OrderR (ord :: OrderK) where
    -- | Extract the major index from a tuple in (row, column, ...) order.
    major :: (Field1 s s a a, Field2 s s a a)
          => Lens' (Tagged ord s) a
    -- | Extract the minor index from a tuple in (row, column, ...) order
    minor :: (Field1 s s a a, Field2 s s a a)
          => Lens' (Tagged ord s) a
    -- | Extract the row index from a tuple in (major, minor, ...) order.
    row :: (Field1 s s a a, Field2 s s a a)
        => Lens' (Tagged ord s) a
    -- | Extract the column index from a tuple in (major, minor, ...) order
    col :: (Field1 s s a a, Field2 s s a a)
        => Lens' (Tagged ord s) a

{-# INLINE comparator #-}
comparator  :: (Field1 s s a a, Field2 s s a a, Ord a, OrderR ord)
            => Proxy ord -> s -> s -> Ordering
comparator witness a' b' =
    let a = tag witness a'
        b = tag witness b'
    in comparing (view major) a b <> comparing (view minor) a b

instance OrderR Row where
    major f = fmap (tag Proxy) . _1 f . untag
    minor f = fmap (tag Proxy) . _2 f . untag
    row f = fmap (tag Proxy) . _1 f . untag
    col f = fmap (tag Proxy) . _2 f . untag
    {-# INLINE major #-}
    {-# INLINE minor #-}
    {-# INLINE row #-}
    {-# INLINE col #-}

instance OrderR Col where
    major f = fmap (tag Proxy) . _2 f . untag
    minor f = fmap (tag Proxy) . _1 f . untag
    row f = fmap (tag Proxy) . _2 f . untag
    col f = fmap (tag Proxy) . _1 f . untag
    {-# INLINE major #-}
    {-# INLINE minor #-}
    {-# INLINE row #-}
    {-# INLINE col #-}

-- | Compressed sparse format
data Cx a = Cx !Int -- ^ minor dimension
               !(Vector Int) -- ^ starting indices of each major slice
               !(Vector (Int, a)) -- ^ (minor index, coefficient)
  deriving (Show)

instance (Eq a, Unbox a) => Eq (Cx a) where
    (==) (Cx mnrA ixsA valsA) (Cx mnrB ixsB valsB) =
        mnrA == mnrB && ixsA == ixsB && valsA == valsB

instance (AEq a, Unbox a) => AEq (Cx a) where
    (===) (Cx mnrA ixsA valsA) (Cx mnrB ixsB valsB) =
        let (nsA, coeffsA) = U.unzip valsA
            (nsB, coeffsB) = U.unzip valsB
        in mnrA == mnrB && ixsA == ixsB && nsA == nsB
            && U.and (U.zipWith (===) coeffsA coeffsB)

    (~==) (Cx mnrA ixsA valsA) (Cx mnrB ixsB valsB) =
        let (nsA, coeffsA) = U.unzip valsA
            (nsB, coeffsB) = U.unzip valsB
        in mnrA == mnrB && ixsA == ixsB && nsA == nsB
            && U.and (U.zipWith (~==) coeffsA coeffsB)

-- | Uncompressed sparse format
data Ux a = Ux !Int -- ^ row dimension
               !Int -- ^ column dimension
               !(Vector (Int, Int, a))
               -- ^ (row index, column index, coefficient)
  deriving (Show)

instance (Eq a, Unbox a) => Eq (Ux a) where
    (==) (Ux rA cA valsA) (Ux rB cB valsB) =
        rA == rB && cA == cB && valsA == valsB

instance (AEq a, Unbox a) => AEq (Ux a) where
    (===) (Ux rA cA valsA) (Ux rB cB valsB) =
        let (rsA, csA, coeffsA) = U.unzip3 valsA
            (rsB, csB, coeffsB) = U.unzip3 valsB
        in rA == rB && cA == cB && rsA == rsB && csA == csB
            && U.and (U.zipWith (===) coeffsA coeffsB)

    (~==) (Ux rA cA valsA) (Ux rB cB valsB) =
        let (rsA, csA, coeffsA) = U.unzip3 valsA
            (rsB, csB, coeffsB) = U.unzip3 valsB
        in rA == rB && cA == cB && rsA == rsB && csA == csB
            && U.and (U.zipWith (~==) coeffsA coeffsB)

data family Matrix :: FormatK -> OrderK -> * -> *
newtype instance Matrix C ord a = MatC (Tagged ord (Cx a))
  deriving (Show)
newtype instance Matrix U ord a = MatU (Tagged ord (Ux a))
  deriving (Show)


class FormatR (fmt :: FormatK) where
    -- | The dimensions of a matrix in (row, column) order.
    dim :: (OrderR ord, Unbox a) => Lens' (Matrix fmt ord a) (Int, Int)

    -- | The dimensions of a matrix in format-specific (major, minor)
    -- order.
    dimF :: (OrderR ord, Unbox a) => Lens' (Matrix fmt ord a) (Int, Int)

    -- | The number of non-zero entries in the matrix.
    nonzero :: Unbox a => Matrix fmt ord a -> Int

    compress :: (OrderR ord, Unbox a) => Matrix fmt ord a -> Matrix C ord a

    decompress :: (OrderR ord, Unbox a) => Matrix fmt ord a -> Matrix U ord a

    fromU :: (OrderR ord, Unbox a) => Matrix U ord a -> Matrix fmt ord a

    fromC :: (OrderR ord, Unbox a) => Matrix C ord a -> Matrix fmt ord a

    transpose :: (OrderR ord, Unbox a) => Matrix fmt ord a -> Matrix fmt ord a

    sliceG  :: (OrderR ord, Unbox a)
            => Int -> Matrix fmt ord a -> Vector (Int, a)
    sliceS  :: (OrderR ord, Unbox a)
            => Int -> Matrix fmt ord a -> Vector (Int, a) -> Matrix fmt ord a

instance FormatR U where
    dim = lens getDim setDim
      where
        getDim (MatU ux) = let Ux r c _ = untag ux in (r, c)
        setDim (MatU ux) (r', c') =
            MatU $ unproxy $ \witness ->
                let Ux r c vals = proxy ux witness
                    vals' | r' < r || c' < c =
                        U.filter (\(i, j, _) -> i < r' && j < c') vals
                          | otherwise = vals
                in Ux r' c' vals'

    dimF = lens getDimF setDimF
      where
        getDimF mat@(MatU ux) =
            let _dim = copyTag ux $ view dim mat
                mjr = view major _dim
                mnr = view minor _dim
            in (mjr, mnr)
        setDimF mat@(MatU ux) _dimF =
            let r = view row $ copyTag ux _dimF
                c = view col $ copyTag ux _dimF
            in set dim (r, c) mat

    nonzero (MatU ux) =
      let Ux _ _ vals = untag ux
      in U.length vals

    compress mat@(MatU ux) =
        MatC $ unproxy $ \witness ->
          let (Ux _ _ vals) = proxy ux witness
              (rows_, cols_, coeffs) = U.unzip3 vals
              (m, n) = view dimF mat
              majors = view major $ tag witness (rows_, cols_)
              minors = view minor $ tag witness (rows_, cols_)
              ixs = runST $ do
                majorsM <- U.thaw majors
                U.generateM m $ binarySearchL majorsM
          in Cx n ixs $ U.zip minors coeffs

    decompress x = x

    transpose (MatU ux) =
        MatU $ unproxy $ \witness ->
            let (Ux r c vals) = proxy ux witness
            in sortUx witness $ Ux c r $ U.map (\(x, y, z) -> (y, x, z)) vals

    sliceG i mat@(MatU ux) =
        untag $ unproxy $ \witness ->
            let Ux _ _ vals = proxy ux witness
                (start, end) = getSliceExtentsU i mat
                (rs, cs, xs) = U.unzip3 $ U.slice start (end - start) vals
                minors = view minor $ copyTag ux (rs, cs)
            in assert (i < view (dimF . _1) mat) $ U.zip minors xs

    sliceS i mat@(MatU ux) sl =
        MatU $ unproxy $ \witness ->
            let Ux r c vals = proxy ux witness
                (start, end) = getSliceExtentsU i mat
                (mnrs, xs) = U.unzip sl
                mjrs = U.replicate (U.length sl) i
                _ixs = copyTag ux $ (mjrs, mnrs)
                rs = view row _ixs
                cs = view col _ixs
                (prefix, _) = U.splitAt start vals
                (_, suffix) = U.splitAt end vals
            in Ux r c $ prefix U.++ (U.zip3 rs cs xs) U.++ suffix

    fromU x = x
    fromC = decompress

    {-# INLINE dim #-}
    {-# INLINE dimF #-}
    {-# INLINE nonzero #-}
    {-# INLINE compress #-}
    {-# INLINE decompress #-}
    {-# INLINE transpose #-}
    {-# INLINE sliceG #-}
    {-# INLINE sliceS #-}
    {-# INLINE fromU #-}
    {-# INLINE fromC #-}

getSliceExtentsU :: (OrderR ord, Unbox a) => Int -> Matrix U ord a -> (Int, Int)
getSliceExtentsU i (MatU ux) =
    untag $ unproxy $ \witness ->
        let Ux _ _ vals = proxy ux witness
            startEl = untag $ set major i $ tag witness (0, 0, undefined)
            endEl = untag $ set major (succ i) $ tag witness (0, 0, undefined)
        in runST $ do
            vals_ <- U.thaw vals
            start <- binarySearchLBy (comparator witness) vals_ startEl
            end <- binarySearchLBy (comparator witness) vals_ endEl
            return (start, end)

instance FormatR C where
    dim = lens getDim setDim
      where
        getDim mat@(MatC cx) =
            let _dimF = copyTag cx $ view dimF mat
                r = view row _dimF
                c = view col _dimF
            in (r, c)
        setDim mat@(MatC cx) _dim =
            let mjr' = view major $ copyTag cx _dim
                mnr' = view minor $ copyTag cx _dim
            in set dimF (mjr', mnr') mat


    dimF = lens getDimF setDimF
      where
        getDimF (MatC cx) =
            let Cx mnr ixs _ = untag cx
                mjr = U.length ixs
            in (mjr, mnr)
        setDimF mat@(MatC cx) (mjr', mnr') =
            MatC $ unproxy $ \witness ->
                let Cx _ ixs vals = proxy cx witness
                    (mjr, mnr) = getDimF mat
                    nnz = U.length vals
                    dMjr = mjr' - mjr
                    ixs' = case compare dMjr 0 of
                        EQ -> ixs
                        LT -> U.take mjr' ixs
                        GT -> ixs U.++ U.replicate dMjr nnz
                    vals' = case compare dMjr 0 of
                        EQ -> vals
                        LT -> U.take (ixs U.! (succ mjr')) vals
                        GT -> vals
                    vals'' | mnr' < mnr = U.filter (\(j, _) -> j < mnr') vals
                           | otherwise = vals
                    ixs''
                        | mnr' < mnr = flip U.map ixs' $ \start ->
                            let numRemoved =
                                    U.length
                                    $ U.findIndices ((>= mnr') . view _1)
                                    $ U.slice 0 start vals'
                            in start - numRemoved
                        | otherwise = ixs'
                in Cx mnr' ixs'' vals''

    compress x = x

    nonzero (MatC cx) =
      let Cx _ _ vals = untag cx
      in U.length vals

    decompress mat@(MatC cx) =
        MatU $ unproxy $ \witness ->
          let (Cx _ majorIxs valsC) = proxy cx witness
              (minors, coeffs) = U.unzip valsC
              m = view (dimF . _1) mat
              majorLengths =
                U.generate m $ \r ->
                  let this = majorIxs U.! r
                      next = fromMaybe (U.length valsC) (majorIxs U.!? (succ r))
                  in next - this
              majors =
                U.concatMap (\(r, len) -> U.replicate len r)
                $ U.indexed majorLengths
              rows_ = view row $ tag witness (majors, minors)
              cols_ = view col $ tag witness (majors, minors)
              (nr, nc) = view dim mat
          in Ux nr nc $ U.zip3 rows_ cols_ coeffs

    transpose = compress . transpose . decompress

    sliceG i (MatC cx) =
        let Cx _ starts vals = untag cx
            start = starts U.! i
            end = fromMaybe (U.length vals) $ starts U.!? succ i
        in assert (i < U.length starts)
        $ (U.slice start $ end - start) vals

    sliceS i (MatC cx) sl =
        MatC $ unproxy $ \witness ->
            let Cx mnr starts vals = proxy cx witness
                start = starts U.! i
                end = fromMaybe (U.length vals) $ starts U.!? i
                dLen = U.length sl - (end - start)
                starts' = flip U.imap starts
                    $ \j x -> if j > i then (x + dLen) else x
                prefix = U.take start vals
                suffix = U.drop end vals
                vals' = prefix U.++ sl U.++ suffix
            in assert (i < U.length starts)
            $ Cx mnr starts' vals'

    fromC x = x
    fromU = compress


    {-# INLINE dim #-}
    {-# INLINE dimF #-}
    {-# INLINE nonzero #-}
    {-# INLINE compress #-}
    {-# INLINE decompress #-}
    {-# INLINE sliceG #-}
    {-# INLINE sliceS #-}
    {-# INLINE transpose #-}
    {-# INLINE fromU #-}
    {-# INLINE fromC #-}

instance (Eq a, Unbox a) => Eq (Matrix C ord a) where
    (==) (MatC a) (MatC b) = untag a == untag b

instance (Eq a, Unbox a) => Eq (Matrix U ord a) where
    (==) (MatU a) (MatU b) = untag a == untag b

instance (AEq a, Unbox a) => AEq (Matrix C ord a) where
    (===) (MatC a) (MatC b) = untag a === untag b
    (~==) (MatC a) (MatC b) = untag a ~== untag b

instance (AEq a, Unbox a) => AEq (Matrix U ord a) where
    (===) (MatU a) (MatU b) = untag a === untag b
    (~==) (MatU a) (MatU b) = untag a ~== untag b

{-# INLINE generate #-}
generate :: Int -> (Int -> a) -> [a]
generate len f = map f $ take len $ [0..]

{-# INLINE empty #-}
empty :: (FormatR fmt, OrderR ord, Unbox a) => Matrix fmt ord a
empty = fromU $ pack 0 0 $ U.empty

{-# INLINE slice #-}
-- | 'Lens' for accessing slices of a matrix. If you aren't modifying the
-- matrix, you probably want 'sliceG' instead. This function will create
-- a copy of the matrix.
slice :: (FormatR fmt, OrderR ord, Unbox a)
      => Int -> Lens' (Matrix fmt ord a) (Vector (Int, a))
slice i = lens (sliceG i) (sliceS i)

{-# INLINE slicesF #-}
-- | Fold over the slices in a matrix using 'sliceG'.
slicesF :: (FormatR fmt, OrderR ord, Unbox a)
        => Fold (Matrix fmt ord a) (Vector (Int, a))
slicesF = folding $ \mat -> generate (view (dimF . _1) mat) $ \i -> sliceG i mat

{-# INLINE rowsF #-}
rowsF :: (FormatR fmt, Unbox a)
      => Fold (Matrix fmt Row a) (Vector (Int, a))
rowsF = slicesF

{-# INLINE colsF #-}
colsF :: (FormatR fmt, Unbox a)
      => Fold (Matrix fmt Col a) (Vector (Int, a))
colsF = slicesF

{-# INLINE sortUx #-}
sortUx :: (OrderR ord, Unbox a) => Proxy ord -> Ux a -> Ux a
sortUx witness (Ux nr nc vals) =
    Ux nr nc $ U.modify (sortBy $ comparator witness) vals

{-# INLINE reorder #-}
reorder :: (OrderR ord', Unbox a) => Matrix U ord a -> Matrix U ord' a
reorder (MatU mat) =
    MatU $ unproxy $ \witness -> sortUx witness $ untag mat

{-# INLINE pack #-}
pack :: (FormatR fmt, OrderR ord, Unbox a)
     => Int -> Int -> Vector (Int, Int, a) -> Matrix fmt ord a
pack r c v
    | U.any (\(i, j, _) -> i >= r || i < 0 || j >= c || j < 0) v =
        error "pack: Index out of bounds!"
    | otherwise = fromU $ MatU $ unproxy $ \witness -> sortUx witness $ Ux r c v

diag :: (FormatR fmt, OrderR ord, Unbox a) => Vector a -> Matrix fmt ord a
diag v =
    let len = U.length v
        ixs = U.enumFromN 0 len
    in pack len len $ U.zip3 ixs ixs v

ident :: (FormatR fmt, Num a, OrderR ord, Unbox a) => Int -> Matrix fmt ord a
ident i = diag $ U.replicate i 1

{-# INLINE mulV #-}
mulV  :: (Num a, Unbox a, V.Vector v a) => Matrix C Row a -> v a -> v a
mulV mat xs_ =
    assert (c == U.length xs)
    $ V.convert $ U.create $ do
      ys <- MU.new r
      iforMOf_ (indexing rowsF) mat $ \ixR _row -> do
        let (cols_, coeffs) = U.unzip _row
        MU.write ys ixR
          $ U.sum $ U.zipWith (*) coeffs
          $ U.backpermute xs cols_
      return ys
  where
    xs = V.convert xs_
    (r, c) = view dim mat

{-# INLINE mulVM #-}
mulVM :: (MV.MVector v a, Num a, PrimMonad m, Unbox a)
      => Matrix C Row a -> v (PrimState m) a -> v (PrimState m) a -> m ()
mulVM mat src dst =
    assert (c == MV.length src)
    $ assert (r == MV.length dst)
    $ iforMOf_ (indexing rowsF) mat $ \ixR _row -> do
      let (_cols, _coeffs) = U.unzip _row
      x <- liftM (U.sum . U.zipWith (*) _coeffs) $ U.mapM (MV.read src) _cols
      MV.write dst ixR x
  where
    (r, c) = view dim mat

{-# INLINE mul #-}
mul :: (Num a, OrderR ord, Unbox a)
    => Matrix C Col a -> Matrix C Row a -> Matrix C ord a
mul a b =
    assert (inner == inner')
    $ foldl' add empty_ $ generate inner $ \i -> expand (sliceG i a) (sliceG i b)
  where
    (inner, left) = view dimF a
    (inner', right) = view dimF b
    empty_ = set dim (left, right) empty
    expand :: (Num a, OrderR ord, Unbox a)
           => Vector (Int, a) -> Vector (Int, a) -> Matrix C ord a
    expand ls rs = MatC $ unproxy $ \witness ->
      let dimC = tag witness (left, right)
          lsrs = tag witness (ls, rs)
          mjr = view major dimC
          mnr = view minor dimC
          ms = view major lsrs
          ns = view minor lsrs
          vals = U.concatMap (\(_, x) -> U.map (\(n, y) -> (n, x * y)) ns) ms
          stride = U.length ns
          ixs = U.iterateN mjr (+ stride) 0
      in Cx mnr ixs vals

{-# INLINE add #-}
add :: (Num a, OrderR ord, Unbox a)
    => Matrix C ord a -> Matrix C ord a -> Matrix C ord a
add a b =
    let (majorA, minorA) = view dimF a
        (valsC, ixsC) = runST $ do
          vals <- MU.new $ nonzero a + nonzero b
          ixs <- MU.new majorA
          let go i start
                | i < majorA = do
                  MU.unsafeWrite ixs i start
                  let sliceA = sliceG i a
                      sliceB = sliceG i b
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

instance Unbox a => Each (Matrix C ord a) (Matrix C ord a) a a where
    each f mat@(MatC cx) =
        let Cx mnr ixs vals = untag cx
        in (MatC . copyTag cx . Cx mnr ixs) <$> (each . _2) f vals

instance Unbox a => Each (Matrix U ord a) (Matrix U ord a) a a where
    each f mat@(MatU ux) =
        let Ux r c vals = untag ux
        in (MatU . copyTag ux . Ux r c) <$> (each . _3) f vals

adjoint :: ( Each (Matrix fmt ord (Complex a))
                  (Matrix fmt ord (Complex a))
                  (Complex a) (Complex a)
           , FormatR fmt, OrderR ord, RealFloat a, Unbox a
           )
        => Matrix fmt ord (Complex a) -> Matrix fmt ord (Complex a)
adjoint = over each conjugate . transpose
