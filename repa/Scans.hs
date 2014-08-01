{-# LANGUAGE TypeOperators, BangPatterns #-}
{-# OPTIONS_GHC -Odph -rtsopts -fno-liberate-case
            -fllvm -optlo-O3 #-}

module Scans where

import Prelude

-- As we don't rely much on Prelude, no qualification is OK
import Data.Array.Repa ( (:.) (..), Z (..), U, D, Array )
import qualified Data.Array.Repa as R
import Data.Array.Repa.Eval
  ( Target, Load )
import Data.Array.Repa.Repr.Unboxed
  ( Unbox )
import Data.Array.Repa.Repr.Vector
  ( V, computeVectorS, computeVectorP, fromListVector )
import Data.List

import Data.Bits

import qualified Data.Vector.Unboxed as V

import Control.Monad
  ( liftM )

type Vector = V.Vector

log2 1 = 0
log2 n = 1 + log2 (n `div` 2)

-- | Sequential scan

scanS :: Unbox e
      => (e -> e -> e)
      -> Array U R.DIM1 e
      -> Array U R.DIM1 e
scanS (?) a = R.fromUnboxed sh . V.scanl1' (?) . R.toUnboxed $ a
  where sh = R.extent a

--

(>>>) :: (R.Shape sh, R.Source r e) => Array r sh e -> b -> b
(>>>) = R.deepSeqArray

-- | One level in sklansky network, fusing pairs of blocks of width 2^k
sklanskyA' :: (R.Source r e, R.Shape sh, Monad m, Unbox e)
          => (e -> e -> e)
          -> Array r (sh :. Int) e
          -> Int {- ^ level -}
          -> m (Array U (sh :. Int) e)
sklanskyA' (?) a k = R.computeUnboxedP $ R.traverse a id f
  where f get sh' @ (sh :. i)
          | i `testBit` k = get (sh :. clearBits i) ? get sh'
          | otherwise   = get sh'
        clearBits i = (i `clearBit` k) .|. (bit k - 1) -- `Bit hackery'

-- | Sloooooow
sklanskyA :: (R.Shape sh, Monad m, Unbox e)
         => (e -> e -> e)
         -> Array U (sh :. Int) e
         -> m (Array U (sh :. Int) e)
sklanskyA (?) a = foldM (sklanskyA' (?)) a [0 .. log2 n]
  where (_ :. n) = R.extent a

--

brentKungA :: (Unbox e, Monad m)
           => Int                        -- ^ Size threshold
           -> (e -> e -> e)
           -> Array U (Z :. Int) e
           -> m (Array U (Z :. Int) e)
brentKungA th (?) a = liftM R.delay (brentKungA' th (?) a) >>= R.computeUnboxedP

brentKungA' :: (Unbox e, Monad m)
            => Int                        -- ^ Size threshold
            -> (e -> e -> e)
            -> Array U (Z :. Int) e
            -> m (Array U (Z :. Int) e)
brentKungA' th (?) a
  | n <= th    = return $ scanS (?) a
  | otherwise = do
    b <- R.computeUnboxedP $ R.traverse a half pairwise
    c <- brentKungA' th (?) b
    let merge (sh :. n)
          | n == 0         = a R.! (sh :. 0)
          | n `mod` 2 == 0 = (c R.! (sh :. n `div` 2 - 1)) ? (a R.! (sh :. n))
          | otherwise      = c R.! (sh :. n `div` 2)
        d = R.fromFunction sh merge
    R.computeUnboxedP $ c >>> d
  where sh @ (_ :. n) = R.extent a
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
  :: (R.Shape sh, Unbox e)
  => Int
  -> Array U (sh :. Int) e
  -> Array V (sh :. Int) (Vector e)
vectorSplit k a = R.computeS . R.fromFunction (sh :. k) $ (\(s :. j) -> V.slice (R.toIndex ext (s :. j * m)) m aUx)
  where
    ext@(sh :. n) = R.extent a
    aUx = R.toUnboxed a
    m = n `div` (k + 1)

-- | Returns the remaining elements not covered by the previous array
-- (length @m + r@)
vectorLast
  :: (R.Shape sh, Unbox e)
  => Int
  -> Array U (sh :. Int) e
  -> Array V sh (Vector e)
{-# INLINE vectorLast #-}
vectorLast k a = R.computeS . R.fromFunction sh $ (\i -> V.slice (R.toIndex (sh :. n) (i :. (n - k'))) k' aUx)
  where
    (sh :. n) = R.extent a
    aUx = R.toUnboxed a
    k' = n `mod` (k + 1) + n `div` (k + 1)

vScanP
  :: (R.Shape sh, Unbox e, Monad m)
  => Int
  -> (e -> e -> e)
  -> Array V sh (Vector e)
  -> m (Array V sh (Vector e))
{-# INLINE vScanP #-}
vScanP k (?) a = R.computeP $ R.map (V.scanl1' (?)) a
--  where tail' = R.extract (Z :. 1) (Z :. k - 1)

fastScan :: (Unbox e, Monad m)
         => Int -- ^ Number of threads
         -> (e -> e -> e)
         -> Array U (Z :. Int) e
         -> m (Array U (Z :. Int) e)
{-# INLINE fastScan #-}
fastScan k (?) !a = do
  let ext@(sh :. n) = R.extent a
      m = n `div` (k + 1)
      split = vectorSplit k a
      vLast = vectorLast k a
  scanned1 <- vScanP k (?) split
  let factors = R.map V.last scanned1
      blockSums = let scan = R.fromListUnboxed (sh :. k) . scanl1 (?) . R.toList in scan factors
      tail' = R.extract (R.zeroDim :. 1) (sh :. k - 1)
      tl1 = tail' scanned1
      lastBlockSum = blockSums R.! (R.ix1 (k-1))
      scanFirst = R.fromUnboxed (Z :. m) (scanned1 R.! (R.ix1 0))
      scanBody = R.zipWith (V.map . (?)) blockSums tl1
      scanLast = R.fromFunction (R.ix1 1) . const $ V.postscanl' (?) lastBlockSum (vLast R.! Z)
  scanned2 <- computeVectorP $ scanBody R.++ scanLast
  let concat' hd = foldl' (R.++) (R.delay hd) . map (\v -> R.fromUnboxed (Z :. V.length v) v) . R.toList
      concated = concat' scanFirst scanned2
  R.computeUnboxedP concated

--

uSplit
  :: (R.Shape sh, Unbox e)
  => Int
  -> Array U (sh :. Int) e
  -> Array D (sh :. Int :. Int) e
{-# INLINE uSplit #-}
uSplit k a = R.reshape (sh :. k :. n') truncated
  where
    ext@(sh :. n) = R.extent a
    n' = n `div` (k + 1)
    truncated = R.extract (R.fromIndex ext 0) (sh :. n - n' - n `mod` (k + 1)) a

uLast
  :: (R.Shape sh, Unbox e)
  => Int
  -> Array U (sh :. Int) e
  -> Array D (sh :. Int) e
{-# INLINE uLast #-}
uLast k a = R.extract (R.fromIndex ext (n - n')) (sh :. n') a
  where
    ext@(sh :. n) = R.extent a
    n' = n `div` (k+1) + n `mod` (k+1)


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
