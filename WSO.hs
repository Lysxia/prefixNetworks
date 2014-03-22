{- |
   "A new approach to the design of optimal parallel prefix circuits" [1],
   Technical report, 2006, Mary Sheeran & Ian Parberry.

   Implementation of networks/circuits described in the paper [1],
   in a style inspired by:
   "Functional and dynamic programming in the
    design of parallel prefix networks" [2]
-}

module WSO (
  -- * Net type
    Fan
  , Net
  , singleWire
  , opFan

  -- ** Operations
  , width
  , ($-)
  , (|>)
  , stack
  , (|||)

  -- * Some statistics
  , printCheck
  , size
  , depth
  , fanout

  -- * \"Pretty\" printing
  , FanPos
  , printNet

  -- * Prefix networks
  -- ** Serial
  , serial

  -- ** Sklansky
  , sklansky
  , sklansky'

  -- ** Slices
  -- *** Fanout 2
  , slices2
  , brentKung

  , slice2
  , tTree
  , bTree

  -- *** Any fanout
  , slice00
  , t1Tree
  , b1Tree

  -- * Helper functions
  , partition'
  ) where

import Data.List


-- * Net type

type Fan a = [a] -> [a]

data Net a = Net
  { width :: Int
  , net   :: Fan a -> [a] -> [a]
  }

{- ^
   The presented construction use and produce /fixed-width/
   (width: number of inputs)
   networks, even though the @net@ member may be applied to other lengths.

   Variables of type @ Net a @ will use names @c@, @d@, ...

   As in [2], networks are parameterized by a \"fan\" component.
 -}

-- | The only width 1 prefix network
singleWire :: Net a
singleWire = Net { width = 1, net = const id }

-- | Fan associated to a binary operator
opFan :: (a -> a -> a) -> [a] -> [a]
opFan o (x : xs) = x : [x `o` x' | x' <- xs]


-- ** Operations

{- |
   To be combined with @($)@ to form a 'ternary' operator:
  
   @
       c $- f $ l
   @
  
   Enforce the width restriction.

   If the list argument does not have the right length, fail.

   Otherwise return @net c f l@, that is to say: substitute
   the argument fan @f@ in @c@ and evaluate the circuit with @l@ as input.
 -}
($-) :: Net a -> Fan a -> [a] -> [a]
($-) c f l = net c f $ l `with_length` width c
  where l `with_length` n =
          if length l == n then
            l
          else
            error $ "Expected length " ++ show n ++ ", got " ++ show (length l)

-- | Network composition, by plugging the last output of
--   the one network as the first input of the other network
(|>) :: Net a -> Net a -> Net a
c @ ( Net n _ ) |> d @ ( Net m _ ) = Net (n+m-1) e'
  where e' f l = let (a0, c0) = splitAt n l
                     (a, b') = splitAt (n-1) $ c $- f $ a0
                     b = d $- f $ (b' ++ c0)
                in a ++ b

-- | Juxtapose two networks
(|||) :: Net a -> Net a -> Net a
c @ ( Net n _ ) ||| d @ ( Net m _ ) = Net (n+m) e'
  where e' f l = let (a0, a1) = splitAt n l
                     b0       = c $- f $ a0
                     b1       = d $- f $ a1
                 in b0 ++ b1

-- | Plug the output of the first network into the input of the second one
--   They have to be of the same length !
stack :: Net a -> Net a -> Net a
stack c @ ( Net n _ ) d @ ( Net m _ ) =
    if n == m then Net n net'
              else error $ "Stacking " ++ show n ++ " on " ++ show m
  where net' f = (d $- f) . (c $- f)


-- * Some statistics

-- | Visual verification of networks using list concatenation
--   (helps debugging networks, prefix or not)
printCheck :: Net [Int] -> IO ()
printCheck = putStrLn . show' . check

check :: Net [Int] -> [[Int]]
check c = c $- opFan (++) $ [[x] | x <- [0 .. width c-1]]

show' :: [[Int]] -> String
show' = intercalate "\n" . map show

--

size :: Net Int -> Int
size c = sum $ c $- szFan $ replicate (width c) 0

szFan :: Fan Int
szFan (x : xs) = x : map (+1) xs

--

depth c = maximum $ c $- dpFan $ replicate (width c) 0

dpFan :: Fan Int
dpFan [x] = [x]
dpFan xs = replicate n (d+1)
  where n = length xs
        d = maximum xs

--

fanout c = maximum $ c $- foFan $ replicate (width c) 0

foFan :: Fan Int
foFan xs = replicate n fo
  where n = length xs
        fo = maximum (n : xs)


-- * \"Pretty\" printing

-- | The type used to collect fan positions
data FanPos = FanPos
  { wireNum  :: Int            -- ^ Wire identifier
  , curDepth :: Int            -- ^ Current depth of the wire
  , fans     :: [(Int, [Int])] -- ^ Fans and their depth
  }

-- | Print an ASCII drawing of the network
printNet :: Net FanPos -> IO ()
printNet = putStrLn . draw

dispFan :: [FanPos] -> [FanPos]
dispFan [w] = [w]
dispFan wires @ (FanPos i _ fs : fps) = fp' : fps'
  where d = maximum $ map curDepth $ wires
        f = map wireNum fps
        fp'  = FanPos i (d+1) ((d, f) : fs)
        fps' = [fp_ { curDepth = d + 1 } | fp_ <- fps]

dispZero :: Int -> [FanPos]
dispZero n = [FanPos i 0 [] | i <- [0..n-1]]

bottomUp :: [FanPos] -> [FanPos]
bottomUp = map (\fp -> fp { fans = reverse $ fans fp })

replicate' :: Int -> [a] -> [a]
replicate' = (concat .) . replicate

layout :: [FanPos] -> [[(Int, [Int])]]
layout fps = layout' 0 fps
  where layout' _  [] = []
        layout' d fps =
          if null ly
            then      layout' (d+1) fps'
            else ly : layout'     d fps'
          where (ly, fps') = scan d 0 fps
        scan _ _        [] = (         [], [])
        scan d i _fps @ (fp : fps)
          | null (fans fp) = scan d (i+1) fps
          |        d == d' = ((j, f), fp { fans = fs })
                         |:: scan d (last f + 1) fps
          |      otherwise = fp >: scan d (i+1) fps
          where FanPos j _ ((d', f) : fs) = fp
                (x, y) |:: (xs, ys) = (x : xs, y : ys)
                y       >: (xs, ys) = (    xs, y : ys)

drawLine :: Int -> [(Int, [Int])] -> String
drawLine n = drawLine' 0
  where drawLine' i            [] = replicate' (n-i) " |"
        drawLine' i ((j, f) : fs) = replicate' (j-i) " |"
                                 ++ " +"
                                 ++ drawFan (j+1) f
                                 ++ drawLine' (last f + 1) fs
        drawFan i       [] = ""
        drawFan i (j : js) = replicate' (j-i) "-|" ++ "-o" ++ drawFan (j+1) js

draw :: Net FanPos -> String
draw c = intercalate "\n" $ intersperse (replicate' n " |") lines
  where disp = bottomUp $ c $- dispFan $ dispZero n
        lines = map (drawLine n) $ layout disp
        n = width c

-- * Prefix networks

-- ** Serial

-- | Serial prefix network
serial :: Int {- ^ width @n@ -} -> Net a
serial n = Net
  { width = n
  , net   = net'
  }
  where net' f          [x] = [x]
        net' f (x : y : xs) = x' : (net' f $ y' : xs)
          where [x', y'] = f [x, y]

checkSerial = printCheck $ serial 10

-- ** Sklansky

-- | Sklansky construction
--
-- If the width @n@ is odd,
-- the middle wire is put to the left (yields a smaller circuit)
sklansky :: Int {- ^ width -} -> Net a
sklansky n = Net n (net' n)
  where net' 1 _ l = l
        net' n f l = let (l1, l2) = splitAt (n - (n `div` 2)) l
                         s1       = net' (n - (n `div` 2)) f l1
                         s2       = net'      (n `div` 2)  f l2
                         (t1, t2) = splitAt (n - (n `div` 2) - 1) s1
                     in t1 ++ f (t2 ++ s2)

-- | Sklansky construction (Alternative)
--
-- If the width @n@ is odd, the middle wire is put to the right.
sklansky' :: Int {- ^ width -} -> Net a
sklansky' n = Net n (net' n)
  where net' 1 _ l = l
        net' n f l = let (l1, l2) = splitAt (n `div` 2) l
                         s1       = net'      (n `div` 2)  f l1
                         s2       = net' (n - (n `div` 2)) f l2
                         (t1, t2) = splitAt ((n `div` 2) - 1) s1
                     in t1 ++ f (t2 ++ s2)

checkSklansky = printCheck $ sklansky 20

-- ** Slices

-- *** Fanout 2

-- | /DSO/ prefix network
slices2 :: Int {- ^ depth -} -> Net a
slices2 d = foldl1 (|>) [slice2 $ min k (d-k-1) | k <- [0..d-1]]

-- | Brent-Kung construction (using slices)
brentKung :: Int {- ^ depth -} -> Net a
brentKung d = foldl1 (|>) [slice2 $ min k (d-k-1) | k <- [0..d `div` 2]]

--

-- Combining /Top trees/ (Figure 6, left)
-- (x) denotes multiple wires
--
--   (a0)   ++  (a1)
--  | .. |     | .. |
--  | T  |     | T' |
--  | .. |     | .. |
--  (b0) b1'   (b2) b3'
--  | .. |     | .. |
--  | .. +-----|-..-o
--  | .. |     | .. |
--  | .. b1    | .. b3
--  | .. |     | .. |
--
combineT :: Net a -> Net a -> Net a
combineT t t' = Net (n+m) net'
  where n = width t
        m = width t'
        net' f l = let (a0, a1)   = splitAt n l
                       b0'        = t  $- f $ a0
                       b2'        = t' $- f $ a1
                       (b0, b1')  = splitAt (n-1) b0'
                       (b2, b3')  = splitAt (m-1) b2'
                       [b1, b3]   = f (b1' ++ b3') -- one element lists
                   in b0 ++ [b1] ++ b2 ++ [b3]

-- Combining /Bottom trees/
--
--  ( c0_)  ++  ( c1_)
--  c0'  (c1)   c2'  (c3)
--  | .. |      | .. |
--  +-..-|------o .. |
--  | .. |      | .. |
--  c0.. |      c2.. |
--  | .. |      | .. |
--  | B  |      | B' |
--  | .. |      | .. |
--   (d0)        (d1)
--  | .. |      | .. |
--
combineB :: Net a -> Net a -> Net a
combineB b b' = Net (n+m) net'
  where n = width b
        m = width b'
        net' f l = let (c0_, c2_) = splitAt n l
                       c0' : c1   = c0_
                       c2' : c3   = c2_
                       [c0, c2]   = f [c0', c2']
                       d0         = b  $- f $ (c0 : c1)
                       d1         = b' $- f $ (c2 : c3)
                   in d0 ++ d1

-- Combine T and B trees to create a /WSO1/ network (Figure 5)
stackWSO1 :: Net a -> Net a -> Net a
stackWSO1 tT bT = Net (n+1) net'
  where n = width bT -- == width tT
        net' f (a0 : a1) =
            let b1'      = tT $- f $ a1
                (b1, b2) = splitAt (n-1) b1'
                [c0, c2] = f (a0 : b2) -- b2 one element
                d0       = bT $- f $ (c0 : b1)
             -- d1       = c2
            in d0 ++ [c2]

-- | T tree
tTree :: Int {- ^ depth -} -> Net a
tTree 0 = singleWire
tTree k = combineT t t
  where t = tTree (k-1)

-- | B tree
bTree :: Int {- ^ depth -} -> Net a
bTree 0 = singleWire
bTree k = combineB b b
  where b = bTree (k-1)

-- | Slice with fanout 2 and depth @2*k+1@
slice2 :: Int {- ^ @k@: depth of B and T -} -> Net a
slice2 k = stackWSO1 (tTree k) (bTree k)

-- *** Any fanout

-- | Net whose first fan can be extended with new wires on the right
--   (so that they are combined with the first wire,
--   and returned as the second element.)
data OpenNet a = OpenNet
  { widthON :: Int
  ,   netON :: Fan a -> [a] -> ([a], [a])
  }

oSingleWire :: OpenNet a
oSingleWire = OpenNet 1 oNet
  where oNet f x = ([fx], fxs)
          where fx : fxs = f x

closeNet :: OpenNet a -> Net a
closeNet ( OpenNet n oNet ) = Net n $ (fst .) . oNet

-- | @ b1Tree t b @
--
--   B1 tree with depth @<= b@ matching @ t1Tree t b @
--
--   An additional last wire is added as well (as part of the first fan)
b1Tree :: Int -> Int -> Net a
b1Tree t b = Net (n+1) $ (uncurry (++) .) . net'
  where OpenNet n net' = b1Trees !! t !! b

-- Memoization strategy
b1Trees :: [[OpenNet a]]
b1Trees = [[b1Tree' t b | b <- [0..]] | t <- [0..]]

b1Tree' :: Int -> Int -> OpenNet a
b1Tree' 0 _ = oSingleWire
b1Tree' _ 0 = oSingleWire
b1Tree' t b = OpenNet (n+m) net'
  where treeLine = b1Trees !! (t-1) -- B1(t, b) is built from
        left  = treeLine !! b       -- B1(t-1, b  ) and
        right = treeLine !! (b-1)   -- B1(t-1, b-1)
        n = widthON left
        m = widthON right
        net' f l = let (l0, l1') = splitAt (n+1) l
                       (l1, l2)  = splitAt (m-1) l1'
                       (k0, k1' : k2) = netON left f (l0 ++ l2)
                       (k1,       []) = netON right f (k1' : l1)
                   in (k0 ++ k1, k2)

-- | @ t1Tree t b @
--
--   T1 tree with depth @<= t@ matching @ b1Tree t b @
t1Tree :: Int -> Int -> Net a
t1Tree t b = t1Trees !! t !! b

t1Trees :: [[Net a]]
t1Trees = [[t1Tree' t b | b <- [0..]] | t <- [0..]]

t1Tree' 0 _ = singleWire
t1Tree' _ 0 = singleWire
t1Tree' t b = Net (n+m) net'
  where treeLine = t1Trees !! (t-1) -- T1(t, b) is built from
        left  = treeLine !! b       -- T1(t-1, b  ) and
        right = treeLine !! (b-1)   -- T1(t-1, b-1)
        n = width  left
        m = width right
        net' f l = let (l0, l1) = splitAt n l
                       k0'      = net  left f l0
                       k2'      = net right f l1
                       (k0, k1) = splitAt (n-1) k0'
                       (k2, k3) = splitAt (m-1) k2'
                       [j1, j3] = f (k1 ++ k3)
                   in k0 ++ [j1] ++ k2 ++ [j3]

-- | @ slice00 t b @
--
--   The slice construction resulting from T1 and B1
--
--   An uneven choice of parameters @t@ and @b@ (@ t > b @) produces a wider
--   circuit than @ slice2 (t+b) `div` 2 @ by exploiting the absence of
--   restriction on fanout.
slice00 t b = stack (singleWire ||| t1Tree t b) (b1Tree t b)

--

-- | @ partition' [p1, ..., pn] l @
--
--   partitions the second argument into sublists of sizes @p1@, ..., @pn@.
partition' :: [Int] -> [a] -> [[a]]
partition'       [] l = [l]
partition' (p : ps) l = u : partition' ps v
  where (u, v) = splitAt p l

--

-- Add a "root" fan which covers
-- the first wire of every sub-network + a new wire

bRootTrees :: [Net a] -> Net a
bRootTrees nets = Net n net'
  where ns = map width nets
        n  = sum ns + 1
        net' f l = let s   = partition' ns l
                       fhs = f $ map head s
                       s'  = zipWith (\x y -> x : tail y) fhs s
                       out = zipWith (net `flip` f) nets s'
                   in concat out ++ last s

-- Add "output" fans which cover the last wire of every sub-network

tRootTrees :: [Net a] -> Net a
tRootTrees nets = Net n net'
  where ns = map width nets
        n  = 1 + sum ns
        net' f (h : t) =
            let s      = partition' ns t
                s'     = zipWith (net `flip` f) nets s
                x : xs = map last s'
                (acc, fxs) = mapAccumL
                               (\x y -> let [w, acc] = f [x, y] in (acc, w))
                               x xs
                out    = zipWith (\x y -> init x ++ [y]) s' (fxs ++ [acc])
            in h : concat out

