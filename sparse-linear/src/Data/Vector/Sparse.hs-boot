module Data.Vector.Sparse where

import qualified Data.Vector.Unboxed as U

data Vector v a
  = Vector { length :: !Int
           , indices :: U.Vector Int
           , values :: v a
           }
