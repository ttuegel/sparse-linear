{-# LANGUAGE PolyKinds #-}

module Data.Proxy.PolyKind where

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

{-# INLINE copyTag #-}
copyTag :: Tagged t a -> b -> Tagged t b
copyTag t x = untag $ unproxy $ \witness -> let _ = proxy t witness in tag witness x

instance Functor (Tagged t) where
    fmap f tagged = copyTag tagged $ f $ untag tagged

