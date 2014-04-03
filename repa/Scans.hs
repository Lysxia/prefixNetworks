{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Odph -rtsopts -fno-liberate-case
            -fllvm -optlo-O3 #-}

module Scans where

import Prelude hiding (map, (++))
-- As we don't rely much on Prelude, no qualification is OK
import Data.Array.Repa as Repa
import Data.Bits
import Data.Vector.Unboxed as Unboxed (Unbox, scanl1)
import Data.Array.Repa.Eval (Target)

log2 1 = 0
log2 n = 1 + log2 (n `div` 2)

-- | Sequential scan

scanS :: Unbox e
      => (e -> e -> e)
      -> Array U DIM1 e
      -> Array U DIM1 e
scanS (?) a = fromUnboxed sh . Unboxed.scanl1 (?) . toUnboxed $ a
  where sh = extent a

--

-- | One level in sklansky network, fusing pairs of blocks of width 2^k
sklanskyA' :: (Source r e, Shape sh, Monad m, Unbox e)
          => (e -> e -> e)
          -> Array r (sh :. Int) e
          -> Int {- ^ level -}
          -> m (Array U (sh :. Int) e)
sklanskyA' (?) a k = computeUnboxedP $ traverse a id f
  where f get sh' @ (sh :. i)
          | i `testBit` k = get (sh :. clearBits i) ? get sh'
          | otherwise   = get sh'
        clearBits i = (i `clearBit` k) .|. (bit k - 1) -- `Bit hackery'

-- | Sloooooow
sklanskyA :: (Shape sh, Monad m, Unbox e)
         => (e -> e -> e)
         -> Array U (sh :. Int) e
         -> m (Array U (sh :. Int) e)
sklanskyA (?) a = foldM (sklanskyA' (?)) a [0 .. log2 n]
  where (_ :. n) = extent a

--

brentKungA :: (Unbox e, Monad m)
           => Int                        -- ^ Size threshold
           -> (e -> e -> e)
           -> Array U (Z :. Int) e
           -> m (Array U (Z :. Int) e)
brentKungA th (?) a = brentKungA' th (?) a >>= computeUnboxedP

brentKungA' :: (Unbox e, Monad m)
            => Int                        -- ^ Size threshold
            -> (e -> e -> e)
            -> Array U (Z :. Int) e
            -> m (Array D (Z :. Int) e)
brentKungA' th (?) a
  | n <= th    = return $ delay $ scanS (?) a
  | otherwise = do
    b <- computeUnboxedP $ traverse a half pairwise
    c <- brentKungA' th (?) b
    let merge (sh :. n)
          | n == 0         = a ! (sh :. 0)
          | n `mod` 2 == 0 = (c ! (sh :. n `div` 2 - 1)) ? (a ! (sh :. n))
          | otherwise      = c ! (sh :. n `div` 2)
    return $ c `deepSeqArray` fromFunction sh merge
  where sh @ (_ :. n) = extent a
        half (sh :. n) = sh :. n `div` 2
        pairwise get (sh :. n) = (get (sh :. 2 * n)) ? (get (sh :. 2 * n + 1))

--

foldM :: Monad m
      => (a -> b -> m a)
      -> a
      -> [b]
      -> m a
foldM f a [] = return a
foldM f a (b : bs) = do
  a' <- f a b
  foldM f a' bs
