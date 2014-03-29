{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.LinearAlgebra.Sparse where

import Control.Applicative ((<$>))
import Control.Exception (assert)
import Control.Lens
import Control.Monad (when)
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
import Data.Vector.Algorithms.Search (Comparison, binarySearchL)

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
data Ux a = Ux !Int -- ^ major dimension
               !Int -- ^ minor dimension
               !(Vector (Int, Int, a))
               -- ^ (row index, column index, coefficient)

data family Matrix :: FormatK -> OrderK -> * -> *
newtype instance Matrix C ord a = MatC (Tagged ord (Cx a))
newtype instance Matrix U ord a = MatU (Tagged ord (Ux a))

-- Names come from Wikipedia: http://en.wikipedia.org/wiki/Sparse_matrix
type MatrixCSR = Matrix C Row
type MatrixCSC = Matrix C Col
type MatrixCOO = Matrix U Row

{-# INLINE compress #-}
compress :: (OrderR ord, Unbox a) => Matrix U ord a -> Matrix C ord a
compress (MatU mat) =
    MatC $ unproxy $ \witness ->
      let (Ux nRows nCols vals) = untag mat
          (rows, columns, coeffs) = U.unzip3 vals
          majDim = view major $ tag witness (nRows, nCols)
          minDim = view minor $ tag witness (nRows, nCols)
          majors = view major $ tag witness (rows, columns)
          minors = view minor $ tag witness (rows, columns)
          majorIxs = runST $ do
            majorsM <- U.thaw majors
            U.generateM majDim
              $ \i -> binarySearchL majorsM i
      in Cx minDim majorIxs $ U.zip minors coeffs

{-# INLINE decompress #-}
decompress :: (OrderR ord, Unbox a) => Matrix C ord a -> Matrix U ord a
decompress (MatC mat) =
    MatU $ unproxy $ \witness ->
      let (Cx minDim majorIxs valsC) = untag mat
          (minors, coeffs) = U.unzip valsC
          majDim = U.length majorIxs
          majorLengths =
            U.generate majDim $ \r ->
              let this = majorIxs U.! r
                  next = fromMaybe (U.length valsC) (majorIxs U.!? (succ r))
              in next - this
          majors =
            U.concatMap (\(r, len) -> U.replicate len r)
            $ U.indexed majorLengths
          rows = view row $ tag witness (majors, minors)
          cols = view col $ tag witness (majors, minors)
          nRows = view row $ tag witness (majDim, minDim)
          nCols = view row $ tag witness (majDim, minDim)
      in Ux nRows nCols $ U.zip3 rows cols coeffs

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
slices :: Unbox a => Fold (Matrix C ord a) (Vector (Int, a))
slices = folding $ \(MatC mat) ->
    let Cx minDim ixs vals = untag mat
    in map (\(start, l) -> U.slice start l vals) $ extents ixs $ U.length vals

{-# INLINE extents #-}
extents :: Vector Int -> Int -> [(Int, Int)]
extents ixs len =
    generate (U.length ixs) $ \i ->
      let start = ixs U.! i
          end = fromMaybe len $ ixs U.!? i
      in (start, end - start)

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
        let (cols, coeffs) = U.unzip _row
        MU.write ys ixR $ U.sum $ U.zipWith (*) coeffs $ U.backpermute xs cols
      return ys
  where
    (MatC (Tagged (Cx nCols ixs _))) = mat
    nRows = U.length ixs

{-
mm :: Matrix C Col a
   -> Matrix C Row a
   -> Matrix U ord a
mm = undefined

mvM :: (Unbox a, Num a, PrimMonad m)
    => Matrix C Row a
    -> MVector (PrimState m) a
    -> MVector (PrimState m) a -> m ()
mvM (MatC (Cx nCols rows vals)) src dst =
    assert (nCols == MU.length src)
    $ assert (nRows == MU.length dst)
    $ U.forM_ (U.indexed $ extents rows)
    $ \(r, (start, end)) ->
      let go i y
            | i < end = do
              (c, a) <- U.unsafeIndexM vals i
              x <- MU.unsafeRead src c
              go (succ i) $! y + a * x
            | otherwise = MU.unsafeWrite dst r y
      in go start 0
  where
    nRows = U.length rows
-}
