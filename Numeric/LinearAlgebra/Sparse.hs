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
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Vector.Algorithms.Intro (sortBy)
import Data.Vector.Algorithms.Search (Comparison, binarySearchP)

data CS a = CS !Int !(Vector Int) !(Vector (Int, a))
data CO a = CO !Int !Int !(Vector (Int, Int, a))

data FormatK = CSR | CSC | COO

data family Matrix :: FormatK -> * -> *
newtype instance Matrix CSR a = MatrixCSR (CS a)
newtype instance Matrix CSC a = MatrixCSC (CS a)
newtype instance Matrix COO a = MatrixCOO (CO a)

class Format f where
    coo :: Unbox a => Matrix f a -> Matrix COO a
    csr :: Unbox a => Matrix f a -> Matrix CSR a
    csc :: Unbox a => Matrix f a -> Matrix CSC a

instance Format COO where
    coo = id

    csr (MatrixCOO (CO nRows nCols vals)) =
        runST $ do
          valsM <- U.thaw vals
          sortBy rowOrdering valsM
          rows <- U.generateM nRows
            $ \i -> binarySearchP (\(r, _, _) -> r == i) valsM
          vals' <- U.map (\(_, c, x) -> (c, x)) <$> U.freeze valsM
          return $! MatrixCSR (CS nCols rows vals')
      where
        rowOrdering :: Comparison (Int, Int, a)
        rowOrdering (ra, ca, _) (rb, cb, _) =
            case compare ra rb of
              EQ -> compare ca cb
              x -> x

    csc (MatrixCOO (CO nRows nCols vals)) =
        runST $ do
          valsM <- U.thaw vals
          sortBy colOrdering valsM
          cols <- U.generateM nRows
            $ \i -> binarySearchP (\(_, c, _) -> c == i) valsM
          vals' <- U.map (\(r, _, x) -> (r, x)) <$> U.freeze valsM
          return $! MatrixCSC (CS nRows cols vals')
      where
        colOrdering :: Comparison (Int, Int, a)
        colOrdering (ra, ca, _) (rb, cb, _) =
            case compare ca cb of
              EQ -> compare ra rb
              x -> x

instance Format CSR where
    coo (MatrixCSR (CS nCols rows vals)) =
        MatrixCOO $ CO nRows nCols $ U.create $ do
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

    csr = id
    csc = csc . coo

instance Format CSC where
    coo (MatrixCSC (CS nRows cols vals)) =
        MatrixCOO $ CO nRows nCols $ U.create $ do
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

    csr = csr . coo
    csc = id

extents :: Vector Int -> Vector (Int, Int)
extents ixs =
    U.postscanr' (\start (end, _) -> (start, end)) (len, 0) ixs
  where
    len = U.length ixs

mm :: Matrix CSR a -> Matrix CSC a -> Matrix COO a
mm = undefined

mv :: (Num a, Unbox a) => Matrix CSR a -> Vector a -> Vector a
mv (MatrixCSR (CS nCols rows vals)) vec =
    assert (nCols == U.length vec)
    $ flip U.map (extents rows)
    $ \(start, end) ->
      let (cols, coeffs) = U.unzip $ U.slice start (end - start) vals
      in U.sum $ U.zipWith (*) coeffs $ U.backpermute vec cols

mvM :: Matrix CSR a -> v (PrimState m) a -> m (v (PrimState m) a)
mvM = undefined
