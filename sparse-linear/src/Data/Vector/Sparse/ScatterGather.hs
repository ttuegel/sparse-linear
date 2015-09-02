module Data.Vector.Sparse.ScatterGather
       ( SG
       , scatter, unsafeScatter, unsafeScatterIndices, unsafeScatterValues
       , gather, count, gatherIndices, gatherValues
       ) where

import Control.Monad.ST ( runST )
import Control.Monad.Trans.Reader ( ReaderT(..), asks )
import Data.Vector.Generic ( Mutable, Vector )
import qualified Data.Vector.Unboxed as Unboxed

import qualified Data.Vector.Sparse as Sparse

-- * The 'SG' Monad

data StateSG v s a
  = StateSG
    { pattern :: !(Unboxed.MVector s Bool)
    , values :: !(Mutable v s a)
    }

newtype SG v s a b = SG { runSG :: ReaderT (StateSG v s a) (ST s) b }
  deriving (Applicative, Functor, Monad)

run :: Int -> a -> (forall s. SG v s a b) -> b

-- * Scatter operations

scatter :: (Vector u a, Vector v b) => Sparse.Vector v b -> (a -> b -> a) -> SG u s a ()

unsafeScatter :: (Vector u a, Vector v b) => Sparse.Vector v b -> (a -> b -> a) -> SG u s a ()

unsafeScatterIndices :: Unboxed.Vector Int -> SG v s a ()

unsafeScatterValues :: (Vector u a, Vector v b) => v b -> (a -> b -> a) -> SG u s a ()

-- * Gather operations

gather :: Vector v a => SG v s a (Sparse.Vector v a)

count :: SG v s a Int

gatherIndices :: Maybe Int -> SG v s a (Unboxed.Vector Int)

gatherValues :: Vector v a => Maybe Int -> SG v s a (v a)
