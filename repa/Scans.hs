{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Odph -rtsopts -fno-liberate-case
            -fllvm -optlo-O3 #-}

module Scans where

-- As we don't rely much on Prelude, no qualification is OK
import Data.Array.Repa
import Data.Bits
import Data.Vector.Unboxed (Unbox)

log2 1 = 0
log2 n = 1 + log2 (n `div` 2)

-- | One level in sklansky network, fusing pairs of blocks of width 2^k
sklansky' :: (Source r e, Shape sh, Monad m, Unbox e)
          => (e -> e -> e)
          -> Array r (sh :. Int) e
          -> Int {- ^ level -}
          -> m (Array U (sh :. Int) e)
sklansky' o a k = computeUnboxedP $ traverse a id f
  where f get sh' @ (sh :. i)
          | i `testBit` k = get (sh :. clearBits i) `o` get sh'
          | otherwise   = get sh'
        clearBits i = (i `clearBit` k) .|. (bit k - 1) -- `Bit hackery'

-- | Sloooooow
sklansky :: (Shape sh, Monad m, Unbox e)
         => (e -> e -> e)
         -> Array U (sh :. Int) e
         -> m (Array U (sh :. Int) e)
sklansky o a = foldM (sklansky' o) a [0 .. log2 n]
  where (_ :. n) = extent a

foldM :: Monad m
      => (a -> b -> m a)
      -> a
      -> [b]
      -> m a
foldM f a [] = return a
foldM f a (b : bs) = do
  a' <- f a b
  foldM f a' bs
