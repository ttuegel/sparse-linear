{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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
import Data.Vector.Algorithms.Search (Comparison, binarySearchP)

-- | Matrix order
data OrderK
    = Row -- ^ row-major
    | Col -- ^ column-major

-- | The GADT is needed for the class constraint, which I'm really not
-- happy about. It shouldn't be necessary, but it is for sortUx.
data OrderP :: OrderK -> * where
    OrderP :: Order ord => OrderP ord

newtype Ordered (ord :: OrderK) a = Ordered a

{-# INLINE order #-}
order :: Order ord => OrderP ord -> a -> Ordered ord a
order _ x = Ordered x

{-# INLINE unorder #-}
unorder :: Order ord => Ordered ord a -> (OrderP ord, a)
unorder (Ordered x) = (OrderP, x)

{-# INLINE unorder_ #-}
unorder_ :: Order ord => Ordered ord a -> a
unorder_ = snd . unorder

instance Order ord => Functor (Ordered ord) where
    fmap f x = order OrderP $ f $ unorder_ x

{-
instance Order ord => Ord (Ordered ord a) where
    compare a b =
      comparing (view major) a b `mappend` comparing (view minor) a b

instance Eq (Ordered ord a) where
    (==) a b = view major a == view major b && view minor a == view minor b
-}

-- | Matrix formats
data FormatK
    = U -- ^ uncompressed
    | C -- ^ compressed

class Order ord where
    -- | Extract the major index from a tuple in (row, column, ...) order.
    major :: (Field1 s s a a, Field2 s s a a) => Lens' (Ordered ord s) a
    -- | Extract the minor index from a tuple in (row, column, ...) order
    minor :: (Field1 s s a a, Field2 s s a a) => Lens' (Ordered ord s) a
    -- | Extract the row index from a tuple in (major, minor, ...) order.
    row :: (Field1 s s a a, Field2 s s a a) => Lens' (Ordered ord s) a
    -- | Extract the column index from a tuple in (major, minor, ...) order
    col :: (Field1 s s a a, Field2 s s a a) => Lens' (Ordered ord s) a

instance Order Row where
    major f s = fmap (order OrderP) $ _1 f $ unorder_ s
    minor f s = fmap (order OrderP) $ _2 f $ unorder_ s
    row f s = fmap (order OrderP) $ _1 f $ unorder_ s
    col f s = fmap (order OrderP) $ _2 f $ unorder_ s

instance Order Col where
    major f s = fmap (order OrderP) $ _2 f $ unorder_ s
    minor f s = fmap (order OrderP) $ _1 f $ unorder_ s
    row f s = fmap (order OrderP) $ _2 f $ unorder_ s
    col f s = fmap (order OrderP) $ _1 f $ unorder_ s

data Cx (ord :: OrderK) a = Cx (OrderP ord) !Int !(Vector Int) !(Vector (Int, a))
data Ux (ord :: OrderK) a = Ux (OrderP ord) !Int !Int !(Vector (Int, Int, a))

data family Matrix :: FormatK -> OrderK -> * -> *
newtype instance Matrix C ord a = MatC (Cx ord a)
newtype instance Matrix U ord a = MatU (Ux ord a)

compress :: (Order ord, Unbox a) => Matrix U ord a -> Matrix C ord a
compress (MatU (Ux witness majorDim minorDim vals)) =
    MatC (Cx witness minorDim majorIxs valsC)
  where
    _minor = view minor . order witness
    _major = view major . order witness
    valsC = U.map (\x -> (_minor x, view _3 x)) vals
    majorIxs = runST $ do
      valsM <- U.thaw vals
      U.generateM majorDim
        $ \i -> binarySearchP (\x -> (_major x) == i) valsM

decompress :: (Order ord, Unbox a) => Matrix C ord a -> Matrix U ord a
decompress (MatC (Cx witness minorDim majorIxs valsC)) =
    MatU (Ux witness majorDim minorDim vals)
  where
    majorDim = U.length majorIxs
    (minors, coeffs) = U.unzip valsC
    majors =
      U.concatMap (\(r, len) -> U.replicate len r) $ U.indexed majorLengths
    majorLengths =
      U.generate majorDim $ \r ->
        let this = majorIxs U.! r
            next = fromMaybe (U.length valsC) (majorIxs U.!? (succ r))
        in next - this
    vals =
      let ixs = order witness (majors, minors)
      in U.zip3 (view row ixs) (view col ixs) coeffs

sortUx :: (Order ord, Unbox a) => Ux ord' a -> Ux ord a
sortUx (Ux _ nr nc vals) =
    Ux witness nr nc $ U.modify (sortBy sorter) vals
  where
    -- This witness is why OrderP must be a GADT. For some reason, the
    -- Order ord constraint isn't propagated to witness. If I could figure
    -- out why, OrderP could be an ordinary ADT.
    witness = OrderP
    _major = view major . order witness
    _minor = view minor . order witness
    sorter a b =
      (comparing _major a b) `mappend` (comparing _minor a b)

     {-
reorder :: Matrix U ord a -> Matrix U ord' a
reorder (MatU ux) = MatU $ sortUx ux
-}
-- Names come from Wikipedia: http://en.wikipedia.org/wiki/Sparse_matrix
type MatrixCSR = Matrix C Row
type MatrixCSC = Matrix C Col
type MatrixCOO = Matrix U Row

{-
pack :: (Order ord, Unbox a)
     => Int -> Int -> Vector (Int, Int, a) -> Matrix ord a
pack r c v = order $ MatUn (Ux r c v)
-}

{-# INLINE extents #-}
extents :: Vector Int -> Vector (Int, Int)
extents ixs = U.postscanr' (\start (end, _) -> (start, end)) (len, 0) ixs
  where
    len = U.length ixs

{-
mm :: Matrix C Col a
   -> Matrix C Row a
   -> Matrix U ord a
mm = undefined

mv :: (Num a, Unbox a) => Matrix C Row a -> Vector a -> Vector a
mv (MatC (Cx nCols rows vals)) vec =
    assert (nCols == U.length vec)
    $ flip U.map (extents rows)
    $ \(start, end) ->
      let (cols, coeffs) = U.unzip $ U.slice start (end - start) vals
      in U.sum $ U.zipWith (*) coeffs $ U.backpermute vec cols

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
