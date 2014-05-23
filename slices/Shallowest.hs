module Shallowest where

import WSO (
  Net (..),
  Fan,
  singleWire,
  ($-),
  (|>),
  (|||),
  stack,
  partition )
import Data.List hiding ( partition )

-- | Network with a single fan
fanNet :: Int -> Net a
fanNet n = Net n id

halfOf :: Int -> Int
halfOf n = (n + 1) `div` 2
-- Try n `div` 2?
-- Greatest power of 2?

-- | Sklansky recursion pattern
-- 
-- > x(1) .. x(i)    x(i+1)    .. x(n)
-- > x(1) .. x[1..i] x(i+1)    .. x[i+1..n]
-- >                 x[1..i+1] .. x[1..n]
sklanskyRec :: Net a -> Net a -> Net a
sklanskyRec a b
  = a |> (stack (singleWire ||| b)
              $ fanNet (width b + 1))

-- | Shallow network.
--
-- The construction requires that for any compatible fan @f@ and list @l@,
-- for any @0 < i < j@, the value of @f l !! i@ does not depend on @f l !! j@.
make :: Int {- ^ Width -} -> Net a
make 1 = singleWire
make n = sklanskyRec (make1 half 1) $ make (n - half)
  where half = halfOf n

-- | Net with slack
make1 :: Int {- ^ Width -} -> Int {- ^ Slack -} -> Net a
make1 n 0 = make n -- Base case unused, but provided for a complete definition
make1 1 _ = singleWire
make1 n k = make1 half (k + 1) |> make2 (n - half + 1) k
  where half = halfOf n

-- | Net with slack,
-- whose first input is indented at depth @(d-slack-1)@,
-- where @d@ is the total depth of the network.
--
-- > d == 1 + (ceil $ log2 width)
make2 :: Int {- ^ Width -} -> Int {- ^ Slack -} -> Net a
make2 1 _ = singleWire
make2 n k = Net n net'
  where
    net' f xs = zs ++ [head ys]
      where
        (ts, zs) = slice (n - 1) (k + 1) k f (tail xs) (tail ys)
        ys = reverse . f $ head xs : reverse ts

slice
  :: Int {- ^ Width -} -> Int {- ^ Slack -} -> Int {- ^ Co-depth -}
  -> (Fan a -> [a] -> [a] -> ([a], [a]))
slice n k d' f xs ys
  | n == 1 = (xs, ys)
  | d' == 0 = (reverse $ make1 n (k - 1) $- f $ xs, reverse $ ys)
    -- equiv. to (reverse $ scanl1 (?) xs, reverse $ ys)
  | otherwise =
    let
      (t1, z1) = slice      half  (k + 1)      d'  f x1 $ tail ys
      (t2, z2) = slice (n - half) (k + 1) (d' - 1) f x2 y2
      y2 = reverse $ f $ head ys : (reverse $ tail t2)
      [t', t_] = f [head t1, head t2] 
      ts = t_ : t' : tail t1
      zs = z1 ++ z2
    in (ts, zs)
  where
    half = halfOf n
    (x1, x2) = splitAt half xs
    x ? y = let [_,v]=f[x,y]in v

{-
-- | Slice with slack
-- and co-depth of the special input
-- (@d - d'@, @d@: depth of the network, @d'@: depth of the first input).
-- 
-- > slice n k d' f t x xs = (y, zs, y')
-- > n == length xs
-- > zs == scanl1 (?) (t : xs)
-- > y == head . f $ x : _ -- Through one fan
-- > y' == foldl (?) (x : xs)
--
slice :: Int {- ^ Width -} -> Int {- ^ Slack -} -> Int {- ^ Co-depth -}
      -> (Fan a -> a -> a -> [a] -> (a, [a], a))
slice n k d' f t x xs
  | d' == 1 =
    let
      ys = make1 n (k + 1) $- f $ xs
      [y, y'] = f [x, last ys]
      zs = f $ t : init ys
    in (y, zs, y')
  | otherwise =
    let
      1 : hlv = halves n
      parts = partition hlv $ tail xs
      (ys, zss, x')
        = unzip3
        $ scanl4 slice'
            (t, [t'], head xs)
            hlv
            (reverse [k + 1 .. k + length hlv])
            ts
            parts
      t' : ts = f ys -- Yes, the definition is circular
      zs = concat zss
      [y, y'] = f [x, last x']
    in (y, zs, y')
  where
    slice' (_, _, y') m k t xs = slice m k (d' - 1) f t y' xs
    {- Test code
    slice' (_, _, y') m k t xs
      = (y', scanl1 (?) (t : init xs), foldl1 (?) (y':xs))
      where x ? y = let [u,v]=f[x,y]in v
    -}

scanl4
  :: (a -> b -> c -> d -> e -> a)
  -> a -> [b] -> [c] -> [d] -> [e] -> [a]
scanl4 u a (b : bs) (c : cs) ~(d : ds) (e : es)
  = {-seq e' $-} a : scanl4 u a' bs cs ds es
  where a' = u a b c d e
scanl4 _ a _ _ _ _ = [a]
-}

{-
sliceTop
  :: Int {- ^ Width -} -> Int {- ^ Slack -} -> Int {- ^ Co-depth -}
  -> (Fan a -> [a] -> [[a]])
sliceTop n k d' f xs =
  | d' == 1   = net $ make1 n (k + 1)
  | otherwise =
    let
      hlv = halves n
      parts = partition hlv xs
      tops -- Build top tree on every part
        = zipWith5 sliceTop
            hlv
            (reverse [k + 1 .. k + length hlv])
            (repeat $ d' - 1)
            (repeat f)
            parts
      tops'
        = onLasts (scanl1 (\x y -> let [x', y'] = f [x, y] in (x', y'))) tops
    in tops'

sliceBot
  :: Int {- ^ Width -} -> Int {- ^ Slack -} -> Int {- ^ Co-depth -}
  -> 
sliceBot n k d' f x xs =
  | d' == 1   = f xs
  | otherwise =
    let
      hlv = halves n
      parts = partition hlv xs
      onHeads = onHeads f
-}

onLasts :: ([a] -> [a]) -> [[a]] -> [[a]]
onLasts f xs = zipWith (++) (map init xs) ((: []) . f $ map last xs)

onHeads :: ([a] -> [a]) -> [[a]] -> [[a]]
onHeads f xs = zipWith (:) (f $ map head xs) $ map tail xs

lasts :: [[a]] -> [a]
lasts = map last

halves :: Int -> [Int]
halves n = halves' n []
  where
    halves' 0 acc = acc
    halves' 1 acc = 1 : acc
    halves' m acc = halves' half $ m - half : acc
      where half = halfOf m

shift :: [[a]] -> [[a]]
shift [] = []
shift [x] = [x]
shift (x : y : xs) = init x : (shift $ (last x : y) : xs)
