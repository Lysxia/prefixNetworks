{- |
   Sequences related to prefix networks
-}

module IntSeq (
  zsWidth,
  zWidth,
  sergeev
  ) where

-- | Max width of a zero-deficiency slice with parameters (k, d)
zsWidth :: Int -> Int -> Int
zsWidth = \k d -> zsWidthMem !! (d-1) !! (k - 1)

-- Memoization strategy for the above
zsWidthMem = [[zsW' k d | k <- [1 .. d]] | d <- [1 ..]]

-- 
zsW' k d
  | k == 1    = 2
  | k == d    = d + 1
  | otherwise = zsWidth (k-1) (d-1) + zsWidth (k-1) (d-2) - 1

-- | Max width of a zero-deficiency network with max depth d
zWidth :: Int -> Int
zWidth d = 1 - d + sum [zsWidth k d | k <- [1..d]]

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
