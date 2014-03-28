{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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
data FormatK
    = RowMajor CompressionK -- ^ row-ordered matrix
    | ColMajor CompressionK -- ^ column-ordered matrix
    | UnOrd                 -- ^ unordered matrix (uncompressed only)

-- | Compression
data CompressionK
    = UnComp  -- ^ uncompressed
    | Comp    -- ^ compressed

data Cx a = Cx !Int !(Vector Int) !(Vector (Int, a))
data Ux a = Ux !Int !Int !(Vector (Int, Int, a))

data family Matrix :: FormatK -> * -> *
newtype instance Matrix (RowMajor Comp) a = MatCR (Cx a)
newtype instance Matrix (ColMajor Comp) a = MatCC (Cx a)
newtype instance Matrix (RowMajor UnComp) a = MatUR (Ux a)
newtype instance Matrix (ColMajor UnComp) a = MatUC (Ux a)
newtype instance Matrix UnOrd a = MatUn (Ux a)

-- Names come from Wikipedia: http://en.wikipedia.org/wiki/Sparse_matrix
type MatrixCOO = Matrix UnOrd
type MatrixCSR = Matrix (RowMajor Comp)
type MatrixCSC = Matrix (ColMajor Comp)

pack :: (Order ord, Unbox a)
     => Int -> Int -> Vector (Int, Int, a) -> Matrix ord a
pack r c v = order $ MatUn (Ux r c v)

{-# INLINE extents #-}
extents :: Vector Int -> Vector (Int, Int)
extents ixs = U.postscanr' (\start (end, _) -> (start, end)) (len, 0) ixs
  where
    len = U.length ixs

class Compress f where
    compress :: Unbox a => Matrix (f UnComp) a -> Matrix (f Comp) a
    decompress :: Unbox a => Matrix (f Comp) a -> Matrix (f UnComp) a

class Order ord where
    order :: Unbox a => Matrix UnOrd a -> Matrix ord a
    deorder :: Unbox a => Matrix ord a -> Matrix UnOrd a

instance Compress RowMajor where
    {-# INLINE compress #-}
    compress (MatUR (Ux nRows nCols vals)) =
        runST $ do
          valsM <- U.thaw vals
          rows <- U.generateM nRows
            $ \i -> binarySearchP (\(r, _, _) -> r == i) valsM
          vals' <- U.map (\(_, c, x) -> (c, x)) <$> U.freeze valsM
          return $! MatCR (Cx nCols rows vals')

    {-# INLINE decompress #-}
    decompress (MatCR (Cx nCols rows vals)) =
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

instance Order (RowMajor UnComp) where
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

instance Compress ColMajor where
    {-# INLINE compress #-}
    compress (MatUC (Ux nRows nCols vals)) =
        runST $ do
          valsM <- U.thaw vals
          cols <- U.generateM nRows
            $ \i -> binarySearchP (\(_, c, _) -> c == i) valsM
          vals' <- U.map (\(r, _, x) -> (r, x)) <$> U.freeze valsM
          return $! MatCC (Cx nRows cols vals')

    {-# INLINE decompress #-}
    decompress (MatCC (Cx nRows cols vals)) =
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

instance Order (ColMajor UnComp) where
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

instance Order UnOrd where
    {-# INLINE order #-}
    order x = x

    {-# INLINE deorder #-}
    deorder x = x

mm :: Matrix (ColMajor Comp) a
   -> Matrix (RowMajor Comp) a
   -> Matrix (ord UnComp) a
mm = undefined

mv :: (Num a, Unbox a) => Matrix (RowMajor Comp) a -> Vector a -> Vector a
mv (MatCR (Cx nCols rows vals)) vec =
    assert (nCols == U.length vec)
    $ flip U.map (extents rows)
    $ \(start, end) ->
      let (cols, coeffs) = U.unzip $ U.slice start (end - start) vals
      in U.sum $ U.zipWith (*) coeffs $ U.backpermute vec cols

mvM :: (Unbox a, Num a, PrimMonad m)
    => Matrix (RowMajor Comp) a
    -> MVector (PrimState m) a
    -> MVector (PrimState m) a -> m ()
mvM (MatCR (Cx nCols rows vals)) src dst =
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
