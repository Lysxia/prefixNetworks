{- |
   Sequences related to prefix networks
-}

module IntSeq (
  zsWidth,
  zWidth,
  sergeev
  ) where

import Data.Array

-- | Max width of a zero-deficiency slice with parameters (k, d)
zsWidth :: Int -> Int -> Integer
zsWidth = \d k -> zsWidthMem !! (d-1) !! (k-1)

-- Memoization strategy for the above
zsWidthMem = do
  d <- [1 ..]
  return $ do
    k <- [1 .. d]
    return $ zsW' d k

-- 
zsW' d k
  | k == 1    = 2
  | k == d    = d + 1
  | otherwise = zsWidth (fI d-1) (fI k-1) + zsWidth (fI d-2) (fI k-1) - 1
  where
    fI = fromIntegral

-- | Max width of a zero-deficiency network with max depth d
zWidth :: Int -> Integer
zWidth d = 1 - fromIntegral d + sum [zsWidth d k | k <- [1..d]]

--

xorZWidth :: Int -> Integer
xorZWidth d = 1 - fromIntegral d + sum [xorZsWidth (k-1) (k-1) k | k <- [1 .. d]]
  where
    xorZsWidth :: Int -> Int -> Int -> Integer
    xorZsWidth = \e l r -> xorZsWidthMem ! (e, l, r)
    
    xorZsWidthMem = array ((0,0,1), (d-1, d, d)) $ do
      e <- [0 .. d-1]
      l <- [0 .. d]
      r <- [1 .. d]
      return $ ((e,l,r), xorZsW' e l r)
    
    xorZsW' e l r
      | d == l && l == r = 2
      | e == 0 = 2
      | d == r || (d > l && l <= r)  = xorZsWidth (e-1) l (l+1)
                                     + xorZsWidth (e-1) (l+1) r - 1
      | d == l || (d > r && l > r) = xorZsWidth (e-1) l (r+1)
                                   + xorZsWidth (e-1) (r+1) r - 1

--

-- | Min size of minimum depth networks of width 2^n
sergeev :: Integer -> Integer
sergeev n = (7 * 2 ^ n - (17 + r) * 2 ^ (n `div` 2)) `div` 2 + n + 5
  where r = n `mod` 2 * 7

-- | Min size of Ladner-Fischer network of width 2^n and slack k
lf :: Int -> Int -> Int
lf n k = lfMem !! n !! k
  where
    lfMem = [[lf' n k | k <- [0 ..]] | n <- [0 ..]]
    lf' 0 _ = 0
    lf' n 0 = lf (n - 1) 0 + lf (n - 1) 1 + 2 ^ (n - 1)
    lf' n k = 2 ^ n - 1 + lf (n - 1) (k - 1)

-- | Min size of Fich network of width 2^n and slack k
fich :: Int -> Int -> Int
fich n k = fichMem !! n !! k
  where
    fichMem = [[fich' n k | k <- [0 ..]] | n <- [0 ..]]
    fich' 0 _ = 0
    fich' n 0 = fich (n - 1) 0 + fich (n - 1) 1 + 2 ^ (n - 1)
    fich' n 1
      | n <= 3 = lf n k
      | otherwise = fich (n - 4) 0 + fich (n - 2) 1 + 27 * 2 ^ (n - 4) - 1
    fich' n k = 2 ^ n - 1 + fich (n - 1) (k - 1)

-- | Min size of Fich network (with improved base case) of width 2^n and slack k
fich2 :: Int -> Int -> Int
fich2 n k = fich2Mem !! n !! k
  where
    fich2Mem = [[fich2' n k | k <- [0 ..]] | n <- [0 ..]]
    fich2' 0 _ = 0
    fich2' n 0 = fich2 (n - 1) 0 + fich2 (n - 1) 1 + 2 ^ (n - 1)
    fich2' n 1
      | n <= 8 = lf n 1
      | otherwise = fich2 (n - 4) 0 + fich2 (n - 2) 1 + 27 * 2 ^ (n - 4) - 1
    fich2' n k = 2 ^ n - 1 + fich2 (n - 1) (k - 1)
