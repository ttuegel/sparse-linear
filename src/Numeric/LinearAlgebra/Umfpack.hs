{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Numeric.LinearAlgebra.Umfpack where

import Control.Applicative
import Data.Complex
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vec
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as Mut
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable(peek))
import Foreign.Storable.Complex ()

import Numeric.LinearAlgebra.Umfpack.Internal

class Umfpack a where
    type UmfpackEntries a

    umfpackSymbolic
      :: Int
      -> Int
      -> IOVector Int
      -> IOVector Int
      -> UmfpackEntries a
      -> IO (Symbolic a)

    umfpackNumeric
      :: IOVector Int
      -> IOVector Int
      -> UmfpackEntries a
      -> Symbolic a
      -> IO (Numeric a)

    -- | Solve the system A x = b.
    umfpackSolve
      :: IOVector Int
      -> IOVector Int
      -> UmfpackEntries a  -- ^ A
      -> UmfpackEntries a  -- ^ x
      -> UmfpackEntries a  -- ^ b
      -> Numeric a
      -> IO ()

    umfpackFreeSymbolic :: Symbolic a -> IO ()
    umfpackFreeNumeric :: Numeric a -> IO ()

instance Umfpack Double where
    type UmfpackEntries Double = IOVector Double

    umfpackSymbolic r c ap_ ai_ ax_ =
        wrap_umfpack $
        Mut.unsafeWith ap_ $ \ap ->
        Mut.unsafeWith ai_ $ \ai ->
        Mut.unsafeWith ax_ $ \ax ->
        alloca $ \sym ->
            (,)
              <$> c_symbolic_real
                (fromIntegral r)
                (fromIntegral c)
                ap ai ax
                sym
                nullPtr nullPtr
              <*> peek sym
    {-# INLINE umfpackSymbolic #-}

    umfpackNumeric ap_ ai_ ax_ sym =
        wrap_umfpack $
        Mut.unsafeWith ap_ $ \ap ->
        Mut.unsafeWith ai_ $ \ai ->
        Mut.unsafeWith ax_ $ \ax ->
        alloca $ \num ->
            (,)
              <$> c_numeric_real
                ap ai ax
                sym
                num
                nullPtr nullPtr
              <*> peek num
    {-# INLINE umfpackNumeric #-}

    umfpackSolve ap_ ai_ ax_ xx_ bx_ num =
        wrap_umfpack $
        Mut.unsafeWith ap_ $ \ap ->
        Mut.unsafeWith ai_ $ \ai ->
        Mut.unsafeWith ax_ $ \ax ->
        Mut.unsafeWith xx_ $ \xx ->
        Mut.unsafeWith bx_ $ \bx ->
          (,)
            <$> c_solve_real
              0 ap ai ax
              xx
              bx
              num
              nullPtr nullPtr
            <*> pure ()
    {-# INLINE umfpackSolve #-}

    umfpackFreeSymbolic = c_free_symbolic_real
    {-# INLINE umfpackFreeSymbolic #-}

    umfpackFreeNumeric = c_free_numeric_real
    {-# INLINE umfpackFreeNumeric #-}

instance Umfpack (Complex Double) where
    type UmfpackEntries = (IOVector Double, IOVector Double)

    umfpackSymbolic r c ap_ ai_ (ax_, az_) =
        wrap_umfpack $
        Mut.unsafeWith ap_ $ \ap ->
        Mut.unsafeWith ai_ $ \ai ->
        Mut.unsafeWith ax_ $ \ax ->
        Mut.unsafeWith az_ $ \az ->
        alloca $ \sym ->
            (,)
              <$> c_symbolic_complex
                (fromIntegral r)
                (fromIntegral c)
                ap ai ax az
                sym
                nullPtr nullPtr
              <*> peek sym
    {-# INLINE umfpackSymbolic #-}

    umfpackNumeric ap_ ai_ (ax_, az_) sym =
        wrap_umfpack $
        Mut.unsafeWith ap_ $ \ap ->
        Mut.unsafeWith ai_ $ \ai ->
        Mut.unsafeWith ax_ $ \ax ->
        Mut.unsafeWith az_ $ \az ->
        alloca $ \num ->
            (,)
              <$> c_numeric_complex
                ap ai ax az
                sym
                num
                nullPtr nullPtr
              <*> peek num
    {-# INLINE umfpackNumeric #-}

    umfpackSolve ap_ ai_ (ax_, az_) (xx_, xz_) (bx_, bz_) num =
        wrap_umfpack $
        Mut.unsafeWith ap_ $ \ap ->
        Mut.unsafeWith ai_ $ \ai ->
        Mut.unsafeWith ax_ $ \ax ->
        Mut.unsafeWith az_ $ \az ->
        Mut.unsafeWith xx_ $ \xx ->
        Mut.unsafeWith xz_ $ \xz ->
        Mut.unsafeWith bx_ $ \bx ->
        Mut.unsafeWith bz_ $ \bz ->
          (,)
            <$> c_solve_complex
              0 ap ai ax az
              xx xz
              bx bz
              num
              nullPtr nullPtr
            <*> pure ()
    {-# INLINE umfpackSolve #-}

    umfpackFreeSymbolic = c_free_symbolic_complex
    {-# INLINE umfpackFreeSymbolic #-}

    umfpackFreeNumeric = c_free_numeric_complex
    {-# INLINE umfpackFreeNumeric #-}

linearSolve
  :: Umfpack a
  => (Int, Int)
  -> Unboxed.Vector (Int, Int, a)
  -> [Unboxed.Vector a]
  -> [Unboxed.Vector a]
linearSolve (r, c) coords rhss = unsafePerformIO $ do
    (cols, rows, entries) <- csc coords
    sym <- umfpackSymbolic r c cols rows entries
    num <- umfpackNumeric cols rows entries sym
    umfpackFreeSymbolic sym
    forM rhss $ \rhs -> do
        mRHS <- Mut.thaw rhs
        when (Mut.length mRHS /= c) $
          errorWithStackTrace "linearSolve: wrong RHS dimension"
        mSoln <- Mut.replicate c 0
