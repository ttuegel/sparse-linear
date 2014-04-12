{-# LANGUAGE BangPatterns #-}
module Data.Vector.Algorithms.Search.Immutable where

import Data.Bits
import Control.Monad.ST
import Data.Vector.Generic hiding (cmp)
import Prelude hiding (length)

type Comparison e = e -> e -> Ordering

-- | Given a predicate that is guaranteed to be monotone on the indices [l,u) in
-- a given vector, finds the index in [l,u] at which the predicate turns from
-- False to True (yielding u if the entire interval is False).
binarySearchPBounds :: (Vector v e)
                    => (e -> Bool) -> v e -> Int -> Int -> Int
binarySearchPBounds p vec l_ u_ = runST $ loop l_ u_
  where
    loop !l !u
        | u <= l    = return l
        | otherwise = do
            e <- unsafeIndexM vec k
            if p e then loop l k else loop (k+1) u
      where k = (u + l) `shiftR` 1
{-# INLINE binarySearchPBounds #-}
--
-- | Finds the lowest index in a given sorted vector at which the given element
-- could be inserted while maintaining the sortedness.
binarySearchL :: (Vector v e, Ord e) => v e -> e -> Int
binarySearchL = binarySearchLBy compare
{-# INLINE binarySearchL #-}

-- | Finds the lowest index in a given vector, which must be sorted with respect to
-- the given comparison function, at which the given element could be inserted
-- while preserving the sortedness.
binarySearchLBy :: (Vector v e) => Comparison e -> v e -> e -> Int
binarySearchLBy cmp vec e = binarySearchLByBounds cmp vec e 0 (length vec)
{-# INLINE binarySearchLBy #-}

-- | Given a vector sorted with respect to a given comparison function on indices
-- in [l,u), finds the lowest index in [l,u] at which the given element could be
-- inserted while preserving sortedness.
binarySearchLByBounds :: (Vector v e) => Comparison e -> v e -> e -> Int -> Int -> Int
binarySearchLByBounds cmp vec e = binarySearchPBounds p vec
 where p e' = case cmp e' e of LT -> False ; _ -> True
{-# INLINE binarySearchLByBounds #-}
