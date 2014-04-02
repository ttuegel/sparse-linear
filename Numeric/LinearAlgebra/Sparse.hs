{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.LinearAlgebra.Sparse where

import Control.Exception (assert)
import Control.Lens
import Control.Monad (liftM, when)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.ST (runST)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import Data.Ord (comparing)
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Vector.Algorithms.Intro (sortBy)
import Data.Vector.Algorithms.Search (binarySearchL)

-- | PolyKind Proxy
data Proxy p = Proxy

-- | PolyKind Tagged
newtype Tagged t a = Tagged { untag :: a }

{-# INLINE tag #-}
tag :: Proxy t -> a -> Tagged t a
tag Proxy = Tagged

{-# INLINE unproxy #-}
unproxy :: (Proxy t -> a) -> Tagged t a
unproxy f = let witness = Proxy in tag witness (f witness)

{-# INLINE proxy #-}
proxy :: Tagged t a -> Proxy t -> a
proxy tagged Proxy = untag tagged

{-# INLINE copyTag #-}
copyTag :: Tagged t a -> b -> Tagged t b
copyTag t x = untag $ unproxy $ \witness -> let _ = proxy t witness in tag witness x

instance Functor (Tagged t) where
    fmap f tagged = untag $ unproxy $ \p -> tag p $ f $ untag tagged

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

-- | Uncompressed sparse format
data Ux a = Ux !Int -- ^ row dimension
               !Int -- ^ column dimension
               !(Vector (Int, Int, a))
               -- ^ (row index, column index, coefficient)

data family Matrix :: FormatK -> OrderK -> * -> *
newtype instance Matrix C ord a = MatC (Tagged ord (Cx a))
newtype instance Matrix U ord a = MatU (Tagged ord (Ux a))

class FormatR (fmt :: FormatK) where
    -- | The dimensions of a matrix in (row, column) order.
    dim :: OrderR ord => Matrix fmt ord a -> (Int, Int)

    -- | The dimensions of a matrix in format-specific (major, minor)
    -- order.
    dimF :: OrderR ord => Matrix fmt ord a -> (Int, Int)

    compress :: (OrderR ord, Unbox a) => Matrix fmt ord a -> Matrix C ord a

    decompress :: (OrderR ord, Unbox a) => Matrix fmt ord a -> Matrix U ord a

instance FormatR U where
    dim (MatU (Tagged (Ux r c _))) = (r, c)

    dimF mat@(MatU ux) =
      -- TODO: Find a lens-y way to do this
      let _dim = copyTag ux $ dim mat
          m = view major _dim
          n = view minor _dim
      in (m, n)

    compress mat@(MatU ux) =
        MatC $ unproxy $ \witness ->
          let (Ux _ _ vals) = proxy ux witness
              (rows_, cols_, coeffs) = U.unzip3 vals
              (m, n) = dimF mat
              majors = view major $ tag witness (rows_, cols_)
              minors = view minor $ tag witness (rows_, cols_)
              ixs = runST $ do
                majorsM <- U.thaw majors
                U.generateM m $ binarySearchL majorsM
          in Cx n ixs $ U.zip minors coeffs

    decompress x = x

    {-# INLINE dim #-}
    {-# INLINE dimF #-}
    {-# INLINE compress #-}
    {-# INLINE decompress #-}

instance FormatR C where
    dimF (MatC (Tagged (Cx n ixs _))) = (U.length ixs, n)

    dim mat@(MatC cx) =
      -- TODO: Find a lens-y way to do this
      let _dim = copyTag cx $ dimF mat
          r = view row _dim
          c = view col _dim
      in (r, c)

    compress x = x

    decompress mat@(MatC cx) =
        MatU $ unproxy $ \witness ->
          let (Cx _ majorIxs valsC) = proxy cx witness
              (minors, coeffs) = U.unzip valsC
              (m, _) = dimF mat
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
              (nr, nc) = dim mat
          in Ux nr nc $ U.zip3 rows_ cols_ coeffs

    {-# INLINE dim #-}
    {-# INLINE dimF #-}
    {-# INLINE compress #-}
    {-# INLINE decompress #-}

-- Names come from Wikipedia: http://en.wikipedia.org/wiki/Sparse_matrix
type MatrixCSR = Matrix C Row
type MatrixCSC = Matrix C Col
type MatrixCOO = Matrix U Row

{-# INLINE generate #-}
generate :: Int -> (Int -> a) -> [a]
generate len f = map f $ take len $ [0..]

-- | I was going to put this in a class to abstract over the matrix format,
-- but I realized that I would just end up compressing the matrix before
-- slicing, anyway. It's a Fold, not a Traversable, because it's not
-- suitable for modifying the matrix. If you want to do that, you'll
-- probably get better speed by manipulating the structure directly. This
-- is just for consumption.
{-# INLINE slices #-}
slices :: (Unbox a, OrderR ord) => Fold (Matrix C ord a) (Vector (Int, a))
slices = folding $ \mat -> generate (fst $ dimF mat) $ slice mat

{-# INLINE rows #-}
rows :: Unbox a => Fold (Matrix C Row a) (Vector (Int, a))
rows = slices

{-# INLINE cols #-}
cols :: Unbox a => Fold (Matrix C Col a) (Vector (Int, a))
cols = slices

{-# INLINE sortUx #-}
sortUx :: (OrderR ord, Unbox a) => Proxy ord -> Ux a -> Ux a
sortUx witness (Ux nr nc vals) =
    Ux nr nc $ U.modify (sortBy sorter) vals
  where
    _major = view major . tag witness
    _minor = view minor . tag witness
    sorter a b = (comparing _major a b) `mappend` (comparing _minor a b)

{-# INLINE reorder #-}
reorder :: (OrderR ord', Unbox a) => Matrix U ord a -> Matrix U ord' a
reorder (MatU mat) =
    MatU $ unproxy $ \witness -> sortUx witness $ untag mat

{-# INLINE pack #-}
pack :: (OrderR ord, Unbox a)
     => Int -> Int -> Vector (Int, Int, a) -> Matrix U ord a
pack r c v = MatU $ unproxy $ \witness -> sortUx witness $ Ux r c v

mv :: (Num a, Unbox a) => Matrix C Row a -> Vector a -> Vector a
mv mat xs =
    assert (nCols == U.length xs)
    $ U.create $ do
      ys <- MU.new nRows
      iforMOf_ (indexing rows) mat $ \ixR _row -> do
        let (cols_, coeffs) = U.unzip _row
        MU.write ys ixR $ U.sum $ U.zipWith (*) coeffs $ U.backpermute xs cols_
      return ys
  where
    (MatC (Tagged (Cx nCols ixs _))) = mat
    nRows = U.length ixs

mvM :: (Unbox a, Num a, PrimMonad m)
    => Matrix C Row a
    -> MVector (PrimState m) a
    -> MVector (PrimState m) a -> m ()
mvM mat src dst =
    assert (nCols == MU.length src)
    $ assert (nRows == MU.length dst)
    $ iforMOf_ (indexing rows) mat $ \ixR _row -> do
      let (_cols, _coeffs) = U.unzip _row
      r <- liftM (U.sum . U.zipWith (*) _coeffs) $ U.mapM (MU.read src) _cols
      MU.write dst ixR r
  where
    (MatC (Tagged (Cx nCols ixs _))) = mat
    nRows = U.length ixs

{-
mm :: Matrix C Col a
   -> Matrix C Row a
   -> Matrix U ord a
mm = undefined
-}

slice :: Unbox a => Matrix C ord a -> Int -> Vector (Int, a)
slice mat@(MatC cx) i =
    let Cx _ _ vals = untag cx
    in (uncurry U.slice $ sliceStartLen mat i) vals

sliceStartLen :: Unbox a => Matrix C ord a -> Int -> (Int, Int)
sliceStartLen (MatC cx) i =
    let Cx _ starts vals = untag cx
        start = starts U.! i
        end = fromMaybe (U.length vals) $ starts U.!? i
    in assert (i < U.length starts) $ (start, start - end)

copyImm :: (PrimMonad m, Unbox a)
        => MVector (PrimState m) a -> Vector a -> m ()
copyImm dst src =
    assert (U.length src == MU.length dst)
    $ U.forM_ (U.enumFromN 0 $ U.length src)
    $ \i -> U.unsafeIndexM src i >>= MU.unsafeWrite dst i

add :: (OrderR ord, Unbox a)
    => Matrix C ord a -> Matrix C ord a -> Matrix C ord a
add matA@(MatC a) matB@(MatC b) =
    MatC $ unproxy $ \witness ->
      let Cx minorA ixsA valsA = proxy a witness
          Cx minorB ixsB valsB = proxy b witness
          nnzA = U.length valsA
          nnzB = U.length valsB
          majorA = U.length ixsA
          majorB = U.length ixsB
          minorC = minorA
          (valsC, ixsC) = runST $ do
            vals <- MU.new $ nnzA + nnzB
            ixs <- MU.new majorA
            let go (i, start) = when (i < majorA) $ do
                  let sliceA = slice matA i
                      sliceB = slice matB i
                      sliceC =
                        -- TODO: remove zeros
                        -- TODO: collect coeffs in same minor dimension
                        U.modify (sortBy (comparing fst))
                        $ sliceA U.++ sliceB
                      len = U.length sliceC
                  MU.unsafeWrite ixs i start
                  copyImm (MU.slice start len vals) sliceC
                  go (succ i, start + len)
            go (0, 0)
            vals_ <- U.unsafeFreeze vals
            ixs_ <- U.unsafeFreeze ixs
            return (vals_, ixs_)
      in assert (minorA == minorB)
         $ assert (majorA == majorB)
         $ Cx minorC ixsC valsC
