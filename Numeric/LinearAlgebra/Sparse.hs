{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.LinearAlgebra.Sparse where

import Control.Applicative ((<$>))
import Control.Exception (assert)
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.ST (runST)
import Data.Maybe (fromMaybe)
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Vector.Algorithms.Intro (sortBy)
import Data.Vector.Algorithms.Search (Comparison, binarySearchP)

-- | Matrix formats
data FmtK
    = R CompK -- ^ row-ordered matrix with specified compression
    | C CompK -- ^ column-ordered matrix with specified compression
    | Un      -- ^ unordered matrix (uncompressed only)

-- | Matrix compression
data CompK
    = U -- ^ uncompressed
    | Z -- ^ compressed

data Zx a = Zx !Int !(Vector Int) !(Vector (Int, a))
data Ux a = Ux !Int !Int !(Vector (Int, Int, a))

data family Matrix :: FmtK -> * -> *
newtype instance Matrix (R Z) a = MatZR (Zx a)
newtype instance Matrix (C Z) a = MatZC (Zx a)
newtype instance Matrix (R U) a = MatUR (Ux a)
newtype instance Matrix (C U) a = MatUC (Ux a)
newtype instance Matrix Un a = MatUn (Ux a)

pack :: Unbox a => Int -> Int -> Vector (Int, Int, a) -> Matrix Un a
pack r c v = MatUn (Ux r c v)

{-# INLINE extents #-}
extents :: Vector Int -> Vector (Int, Int)
extents ixs = U.postscanr' (\start (end, _) -> (start, end)) (len, 0) ixs
  where
    len = U.length ixs

class Compressed f where
    compress :: Unbox a => Matrix (f U) a -> Matrix (f Z) a
    decompress :: Unbox a => Matrix (f Z) a -> Matrix (f U) a

class Ordered f where
    order :: Unbox a => Matrix Un a -> Matrix (f U) a
    deorder :: Unbox a => Matrix (f U) a -> Matrix Un a

instance Compressed R where
    {-# INLINE compress #-}
    compress (MatUR (Ux nRows nCols vals)) =
        runST $ do
          valsM <- U.thaw vals
          rows <- U.generateM nRows
            $ \i -> binarySearchP (\(r, _, _) -> r == i) valsM
          vals' <- U.map (\(_, c, x) -> (c, x)) <$> U.freeze valsM
          return $! MatZR (Zx nCols rows vals')

    {-# INLINE decompress #-}
    decompress (MatZR (Zx nCols rows vals)) =
        MatUR $ Ux nRows nCols $ U.create $ do
          vals' <- U.unsafeThaw $ U.map (\(c, x) -> (-1, c, x)) vals
          U.forM_ (U.indexed $ extents rows) $ \(r, (start, end)) ->
            let go i
                  | i < end = do
                    (_, c, x) <- MU.read vals' i
                    MU.write vals' i (r, c, x)
                    go $ succ i
                  | otherwise = return ()
            in go start
          return vals'
      where
        nRows = U.length rows
        len = U.length vals

instance Ordered R where
    {-# INLINE order #-}
    order (MatUn (Ux nRows nCols vals)) =
        MatUR $ Ux nRows nCols $ U.modify (sortBy rowOrd) vals
      where
        {-# INLINE rowOrd #-}
        rowOrd :: Comparison (Int, Int, a)
        rowOrd (ra, ca, _) (rb, cb, _) =
            case compare ra rb of
              EQ -> compare ca cb
              x -> x

    {-# INLINE deorder #-}
    deorder (MatUR mat) = MatUn mat

instance Compressed C where
    {-# INLINE compress #-}
    compress (MatUC (Ux nRows nCols vals)) =
        runST $ do
          valsM <- U.thaw vals
          cols <- U.generateM nRows
            $ \i -> binarySearchP (\(_, c, _) -> c == i) valsM
          vals' <- U.map (\(r, _, x) -> (r, x)) <$> U.freeze valsM
          return $! MatZC (Zx nRows cols vals')

    {-# INLINE decompress #-}
    decompress (MatZC (Zx nRows cols vals)) =
        MatUC $ Ux nRows nCols $ U.create $ do
          vals' <- U.unsafeThaw $ U.map (\(r, x) -> (r, -1, x)) vals
          U.forM_ (U.indexed $ extents cols) $ \(c, (start, end)) ->
            let go i
                  | i < end = do
                    (r, _, x) <- MU.read vals' i
                    MU.write vals' i (r, c, x)
                    go $ succ i
                  | otherwise = return ()
            in go start
          return vals'
      where
        nCols = U.length cols
        len = U.length vals

instance Ordered C where
    {-# INLINE order #-}
    order (MatUn (Ux nRows nCols vals)) =
        MatUC $ Ux nRows nCols $ U.modify (sortBy colOrd) vals
      where
        {-# INLINE colOrd #-}
        colOrd :: Comparison (Int, Int, a)
        colOrd (ra, ca, _) (rb, cb, _) =
            case compare ca cb of
              EQ -> compare ra rb
              x -> x

    {-# INLINE deorder #-}
    deorder (MatUC mat) = MatUn mat

mm :: Ordered f => Matrix (C Z) a -> Matrix (R Z) a -> Matrix (f U) a
mm = undefined

mv :: (Num a, Unbox a) => Matrix (R Z) a -> Vector a -> Vector a
mv (MatZR (Zx nCols rows vals)) vec =
    assert (nCols == U.length vec)
    $ flip U.map (extents rows)
    $ \(start, end) ->
      let (cols, coeffs) = U.unzip $ U.slice start (end - start) vals
      in U.sum $ U.zipWith (*) coeffs $ U.backpermute vec cols

mvM :: (Unbox a, Num a, PrimMonad m)
    => Matrix (R Z) a -> MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
mvM (MatZR (Zx nCols rows vals)) src dst =
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
