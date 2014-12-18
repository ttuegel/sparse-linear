module Control.Loop.For where

import Data.Foldable

data ForI a = ForI !Int !Int !(Int -> a)

instance Functor ForI where
    fmap = \f (ForI i0 iend g) -> ForI i0 iend (f . g)
    {-# INLINE fmap #-}

instance Foldable ForI where
    foldr f r0 (ForI i0 iend g) = foldr_go i0 where
      foldr_go i
        | i < iend = f (g i) $ foldr_go (i + 1)
        | otherwise = r0
    {-# INLINE foldr #-}

    foldl' f r0 (ForI i0 iend g) = foldl'_go r0 i0 where
      foldl'_go r1 i
        | i < iend = let r2 = f r1 (g i) in r2 `seq` foldl'_go r2 (i + 1)
        | otherwise = r1
    {-# INLINE foldl' #-}

forI :: Int -> Int -> ForI Int
forI i0 iend = ForI i0 iend id
{-# INLINE forI #-}
