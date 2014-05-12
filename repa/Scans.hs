{-# LANGUAGE TypeOperators, BangPatterns #-}
{-# OPTIONS_GHC -Odph -rtsopts -fno-liberate-case
            -fllvm -optlo-O3 #-}

module Scans where

import Prelude hiding ( length, last, map, (++), zipWith )

-- As we don't rely much on Prelude, no qualification is OK
import Data.Array.Repa as Repa
import Data.Bits
import Data.Vector.Unboxed as Vect
  ( Vector
  , length
  , last
  , postscanl
  , scanl1' )

import qualified Data.Vector.Unboxed as Vect
  ( (!)
  , map
  , slice )

import Data.Array.Repa.Eval
  ( Target, Load )
import Data.Array.Repa.Repr.Unboxed
  ( Unbox )
import Data.Array.Repa.Repr.Vector
  ( V, computeVectorP, fromListVector )
import Control.Monad
  ( liftM )

log2 1 = 0
log2 n = 1 + log2 (n `div` 2)

-- | Sequential scan

scanS :: Unbox e
      => (e -> e -> e)
      -> Array U DIM1 e
      -> Array U DIM1 e
scanS (?) a = fromUnboxed sh . Vect.scanl1' (?) . toUnboxed $ a
  where sh = extent a

--

(>>>) :: (Shape sh, Source r e) => Array r sh e -> b -> b
(>>>) = deepSeqArray

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
brentKungA th (?) a = liftM delay (brentKungA' th (?) a) >>= computeUnboxedP

brentKungA' :: (Unbox e, Monad m)
            => Int                        -- ^ Size threshold
            -> (e -> e -> e)
            -> Array U (Z :. Int) e
            -> m (Array U (Z :. Int) e)
brentKungA' th (?) a
  | n <= th    = return $ scanS (?) a
  | otherwise = do
    b <- computeUnboxedP $ traverse a half pairwise
    c <- brentKungA' th (?) b
    let merge (sh :. n)
          | n == 0         = a ! (sh :. 0)
          | n `mod` 2 == 0 = (c ! (sh :. n `div` 2 - 1)) ? (a ! (sh :. n))
          | otherwise      = c ! (sh :. n `div` 2)
        d = fromFunction sh merge
    computeUnboxedP $ c >>> d
  where sh @ (_ :. n) = extent a
        half (sh :. n) = sh :. n `div` 2
        pairwise get (sh :. i) = (get (sh :. 2 * i)) ? (get (sh :. 2 * i + 1))

--

-- | Split an array @a = [| 0 .. (k + 1) * m + r |]@, @0 <= r <= k@,
-- to an array of @k@ vectors of length @m@
-- (leaving an array of length @m + r@).
--
-- > vectorSplit k a
--
-- > [| [| 0 .. m - 1 |] .. [|(k - 1) * m .. k * m - 1 |] |]
--
vectorSplit
  :: (Shape sh, Unbox e)
  => Int
  -> Array U (sh :. Int) e
  -> Array V (sh :. Int) (Vector e)
vectorSplit k a = vLinear
  where
    (sh :. n) = extent a
    aUx = toUnboxed a
    m = n `div` (k + 1)
    slices = do
      j <- [ 0, n .. length aUx - n ]
      i <- [ 0 .. (k - 1) ]
      return $ Vect.slice (j + i * m) m aUx
    vLinear = fromListVector (sh :. k) slices

-- | Returns the remaining elements not covered by the previous array
-- (length @m + r@)
vectorLast
  :: (Shape sh, Unbox e)
  => Int
  -> Array U (sh :. Int) e
  -> Array V sh (Vector e)
{-# INLINE vectorLast #-}
vectorLast k a = vLast
  where
    (sh :. n) = extent a
    aUx = toUnboxed a
    k' = n `mod` (k + 1) + n `div` (k + 1)
    slices = do
      j <- [ 0, n .. length aUx - n ]
      return $ Vect.slice (j + n - k') k' aUx
    vLast = fromListVector sh slices

vMapScan
  :: (Shape sh, Unbox e, Monad m)
  => (e -> e -> e)
  -> Array V sh (Vector e)
  -> m (Array V sh (Vector e))
{-# INLINE vMapScan #-}
vMapScan (?) a = computeVectorP $ map (scanl1' (?)) a

fastScan :: (Shape sh, Unbox e, Monad m)
         => Int -- ^ Number of threads
         -> (e -> e -> e)
         -> Array U (sh :. Int) e
         -> m (Array U (sh :. Int) e)
fastScan k (?) a = do
  scand <- vMapScan (?) $ split
  let
    factors = map last scand
    blockSums = fromListUnboxed (sh :. k) . scanl1 (?) $ toList factors
    tail = extract (zeroDim :. 1) (sh :. k - 1) scand
    scanTail = zipWith (Vect.map . (?)) blockSums tail
    penultim = slice blockSums $ Any :. (k - 1)
    scanLast = zipWith (Vect.postscanl (?)) penultim vLast
    finalScan = extract zeroDim (sh :. 1) scand
             ++ scanTail
             ++ reshape (sh :. 1) scanLast
  finalScand <- scand >>> computeVectorP finalScan
  let
    get (sh :. i)
      | q == k + 1 = finalScand ! (sh :. k) Vect.! (m + r)
      | otherwise  = finalScand ! (sh :. q) Vect.! r
      where
        q = i `div` m
        r = i `mod` m
  computeUnboxedP $ finalScand >>> fromFunction sh' get
  where
    sh' @ (sh :. n) = extent a
    split = vectorSplit k a
    vLast = vectorLast k a
    m = n `div` (k + 1)

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
