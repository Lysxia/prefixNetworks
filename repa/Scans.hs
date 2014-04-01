{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Odph -rtsopts -fno-liberate-case
            -fllvm -optlo-O3 #-}

module Scans where

-- As we don't rely much on Prelude, no qualification is OK
import Data.Array.Repa
import Data.Bits

log2 1 = 0
log2 n = 1 + log2 (n `div` 2)

-- | One level in sklansky network, fusing pairs of blocks of width 2^k
sklansky' :: (Source r e, Shape sh)
          => (e -> e -> e)
          -> Array r (sh :. Int) e
          -> Int {- ^ level -}
          -> Array D (sh :. Int) e
sklansky' o a k = traverse a id f
  where f get sh' @ (sh :. i)
          | i `testBit` k = get (sh :. clearBits i) `o` get sh'
          | otherwise   = get sh'
        clearBits i = (i `clearBit` k) .|. (bit k - 1) -- `Bit hackery'

sklansky :: (Source r e, Shape sh)
         => (e -> e -> e)
         -> Array r (sh :. Int) e
         -> Array D (sh :. Int) e
sklansky o a = foldl (sklansky' o) (delay a) [0 .. log2 n]
  where (_ :. n) = extent a
        
