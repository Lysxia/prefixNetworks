-- Author: Li-yao Xia, github.com/Syrak
-- Haddock documentation

{- |
   \"A new approach to the design of optimal parallel prefix circuits\" [1],
   Technical report, 2006, Mary Sheeran & Ian Parberry.

   Implementation of networks/circuits described in the paper [1],
   in a style inspired by:
   \"Functional and dynamic programming in the
    design of parallel prefix networks\" [2]
-}

module WSO (
  -- * Net type
    Net(..)
  , Fan
  , singleWire
  , opFan

  -- ** Operations
  , ($-)
  , (|>)
  , stack
  , (|||)

  -- * Network information
  , size
  , depth
  , fanout

  , printCheck

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

  -- *** Any fanout
  , slice00
  , slices00

  -- *** Fanout @ f >= 3 @
  , slices
  , slice

  -- *** Matching B/T trees
  , tTree
  , bTree

  , t1Tree
  , b1Tree
  , b1TreeWaist

  , tTreef
  , bTreef

  -- * Helper functions
  , ($@)
  , partition'
  , multiMemo

  , treeMem
  , treefMem
  ) where

import Data.List
import Memo

-- * Net type

data Net a = Net
  { width :: Int
  , net   :: Fan a -> [a] -> [a]
  }

{- ^
   The presented construction use and produce /fixed-width/
   (width: number of inputs) networks, even though the @net@ member
   as a function may be applied to other lengths.

   Variables of type @ Net a @ will use names @c@, @d@, ...
   in the implementation.
 -}

-- | As in [2], networks are parameterized by a \"fan\" component.
type Fan a = [a] -> [a]

-- | The only width 1 prefix network, does nothing
singleWire :: Net a
singleWire = Net { width = 1, net = const id }

-- | One gate
twoWires :: Net a
twoWires = Net { width = 2, net = id }

-- | Fan associated to a binary operator
opFan :: (a -> a -> a) -> [a] -> [a]
opFan o (x : xs) = x : [x `o` x' | x' <- xs]


-- ** Operations

{- |
   To be combined with @($)@ to form a 'ternary' operator:

   @
       c $- f $ x
   @

   Enforce the width restriction.

   If the list argument does not have the right length, fail.

   Otherwise return @net c f x@, that is to say: substitute
   the argument fan @f@ in @c@ and evaluate the circuit with @x@ as input.
 -}
($-) :: Net a -> Fan a -> [a] -> [a]
($-) c f x = net c f $ x `with_length` width c
  where x `with_length` n =
          if length x == n
            then x
            else error $ "Expected length " ++ show n
                      ++ ", got " ++ show (length x)

-- | Network composition, by plugging the last output of
--   one network as the first input of the other network
(|>) :: Net a -> Net a -> Net a
c @ ( Net n _ ) |> d @ ( Net m _ ) = Net (n+m-1) e'
  where e' f x = let (x0, x1) = splitAt n x
                     (y0, y1) = splitAt (n-1) $ c $- f $ x0
                     z        = d $- f $ (y1 ++ x1)
                in y0 ++ z

-- | Juxtapose two networks
(|||) :: Net a -> Net a -> Net a
c @ ( Net n _ ) ||| d @ ( Net m _ ) = Net (n+m) e'
  where e' f x = (c $- f $ x0) ++ (d $- f $ x1)
          where (x0, x1) = splitAt n x

-- | Plug the output of the first network into the input of the second one
--   They must have the same length !
stack :: Net a -> Net a -> Net a
stack c @ ( Net n _ ) d @ ( Net m _ ) =
    if n == m then Net n net'
              else error $ "Stacking " ++ show n ++ " on " ++ show m
  where net' f = (d $- f) . (c $- f)


-- * Network information

-- | Visual verification of networks using list concatenation
--   (helps debugging networks, prefix or not)
printCheck :: Net [Int] -> IO ()
printCheck = putStrLn . show' . check

check :: Net [Int] -> [[Int]]
check c = c $- opFan (++) $ [[x] | x <- [0 .. width c-1]]

show' :: [[Int]] -> String
show' = intercalate "\n" . map show

--

-- | Size of a circuit
size :: Net Int -> Int
size c = sum $ c $- szFan $ replicate (width c) 0

szFan :: Fan Int
szFan (x : xs) = x : map (+1) xs

--

-- | Depth of a circuit
depth c = maximum $ c $- dpFan $ replicate (width c) 0

dpFan :: Fan Int
dpFan [x] = [x]
dpFan xs = replicate n (d+1)
  where n = length xs
        d = maximum xs

--

-- | Maximum fanout
fanout c = maximum $ c $- foFan $ replicate (width c) 0

foFan :: Fan Int
foFan xs = replicate n fo
  where n = length xs
        fo = maximum (n : xs)

-- | Left associative application to help reduce the number of brackets
--
--   A syntactic trick dependent on personal taste
infixl 0 $@
($@) = ($)

--
 
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

-- ** Sklansky

-- | Sklansky construction
--
-- If the width @n@ is odd,
-- the middle wire is put to the left
-- (yields a smaller circuit compared to the following one)
sklansky :: Int {- ^ width -} -> Net a
sklansky n = Net n (net' n)
  where net' 1 _ x = x
        net' n f x = let (x1, x2) = splitAt (n - (n `div` 2)) x
                         s1       = net' (n - (n `div` 2)) f x1
                         s2       = net'      (n `div` 2)  f x2
                         (t1, t2) = splitAt (n - (n `div` 2) - 1) s1
                     in t1 ++ f (t2 ++ s2)

-- | Sklansky construction (alternative)
--
-- If the width @n@ is odd, the middle wire is put to the right.
sklansky' :: Int {- ^ width -} -> Net a
sklansky' n = Net n (net' n)
  where net' 1 _ x = x
        net' n f x = let (x1, x2) = splitAt (n `div` 2) x
                         s1       = net'      (n `div` 2)  f x1
                         s2       = net' (n - (n `div` 2)) f x2
                         (t1, t2) = splitAt ((n `div` 2) - 1) s1
                     in t1 ++ f (t2 ++ s2)

checkSklansky = printCheck $ sklansky 20

-- ** Slices

-- *** Fanout 2

-- | /DSO/ prefix network
slices2 :: Int {- ^ depth -} -> Net a
slices2 d = foldl1 (|>) [slice2 $ min k (d-k-1) | k <- [0 .. d-1]]

-- | Brent-Kung construction (using slices)
brentKung :: Int {- ^ depth -} -> Net a
brentKung d = foldl1 (|>) [slice2 $ min k (d-k-1) | k <- [0 .. d `div` 2]]

-- | Slice with fanout 2 and depth @2*k+1@
slice2 :: Int {- ^ @k@: depth of B and T -} -> Net a
slice2 k = stackWSO1 $@ tTree k
                     $@ bTree k

--

{- Combining /Top trees/ (Figure 6, left)
   (x) denotes multiple wires

     (x0)   ++  (x1)
    | .. |     | .. |
    | T  |     | T' |
    | .. |     | .. |
    (y0) y1'   (y2) y3'
    | .. |     | .. |
    | .. +-----|-..-o
    | .. |     | .. |
    | .. y1    | .. y3
    | .. |     | .. |
 -}
combineT :: Net a -> Net a -> Net a
combineT t t' = Net (n+m) net'
  where n = width t
        m = width t'
        net' f x = let (x0, x1)   = splitAt n x
                       y0'        = t  $- f $ x0
                       y2'        = t' $- f $ x1
                       (y0, y1')  = splitAt (n-1) y0'
                       (y2, y3')  = splitAt (m-1) y2'
                       [y1, y3]   = f (y1' ++ y3') -- one element lists
                   in y0 ++ [y1] ++ y2 ++ [y3]

{- Combining /Bottom trees/

    ( z0_)  ++  ( z1_)
    z0'  (z1)   z2'  (z3)
    | .. |      | .. |
    +-..-|------o .. |
    | .. |      | .. |
    z0.. |      z2.. |
    | .. |      | .. |
    | B  |      | B' |
    | .. |      | .. |
     (w0)        (w1)
    | .. |      | .. |
 -}
combineB :: Net a -> Net a -> Net a
combineB b b' = Net (n+m) net'
  where n = width b
        m = width b'
        net' f x = let (z0_, z2_) = splitAt n x
                       z0' : z1   = z0_
                       z2' : z3   = z2_
                       [z0, z2]   = f [z0', z2']
                    in (b  $- f $ (z0 : z1)) ++ (b' $- f $ (z2 : z3))

-- Combine T and B trees to create a /WSO1/ network (Figure 5)
stackWSO1 :: Net a -> Net a -> Net a
stackWSO1 tT bT = Net (n+1) net'
  where n = width bT -- == width tT
        net' f (x0 : x1) = let y1'      = tT $- f $ x1
                               (y1, y2) = splitAt (n-1) y1'
                               [z0, z2] = f (x0 : y2) -- y2 one element
                           in (bT $- f $ (z0 : y1)) ++ [z2]

-- | T tree with fanout 2 and depth @k@ matching @ bTree k @
tTree :: Int {- ^ depth -} -> Net a
tTree 0 = singleWire
tTree k = combineT t t
  where t = tTree (k-1)

-- | B tree with fanout 2 and depth @k@ matching @ tTree k @
bTree :: Int {- ^ depth -} -> Net a
bTree 0 = singleWire
bTree k = combineB b b
  where b = bTree (k-1)

-- *** Any fanout

-- | A more general representation of B1 and Bf' trees
--
--   Net whose first fan can be extended with new wires on the right
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

-- | Convert to a proper @Net@
closeNet :: OpenNet a -> Net a
closeNet ( OpenNet n net' ) = Net n net''
  where net'' = (fst .) . net'

-- | An additional last wire is added as well (as part of the first fan),
--   so that the waist is included in the returned tree.
withWaist :: OpenNet a -> Net a
withWaist ( OpenNet n net' ) = Net (n+1) net''
  where net'' = (uncurry (++) .) . net'

-- | @ b1Tree t b @
--
--   B1 tree with depth @ <= b @ matching @ t1Tree t b @.
--   No fanout restriction
b1Tree :: Int -> Int -> Net a
b1Tree t b = closeNet $ b1Tree' t b

-- | @ b1TreeWaist t b @
--
--   The first fan is extended to include the waist.
b1TreeWaist :: Int -> Int -> Net a
b1TreeWaist t b = withWaist $ b1Tree' t b

b1Tree' :: Int -> Int -> OpenNet a
b1Tree' = treeMem oSingleWire combine
  where left @ ( OpenNet n oc ) `combine` right @ ( OpenNet m od ) =
          OpenNet (n+m) onet'
          where onet' f x =
                  let (x0, x1') = splitAt (n+1) x
                      (x1, x2)  = splitAt (m-1) x1'
                      (y0, y1' : y2) = netON left f (x0 ++ x2)
                      (y1,       []) = netON right f (y1' : x1)
                  in (y0 ++ y1, y2)

-- | @ t1Tree t b @
--
--   T1 tree with depth @ <= t @ matching @ b1Tree t b @.
--   No fanout restriction
t1Tree :: Int -> Int -> Net a
t1Tree = treeMem singleWire combine
  where left @ ( Net n _ ) `combine` right @ ( Net m _ ) = Net (n+m) net'
          where net' f x =
                  let (x0, x1) = splitAt n x
                      y0'      =  left $- f $ x0
                      y2'      = right $- f $ x1
                      (y0, y1) = splitAt (n-1) y0'
                      (y2, y3) = splitAt (m-1) y2'
                      [z1, z3] = f (y1 ++ y3)
                  in y0 ++ [z1] ++ y2 ++ [z3]

-- | Memoization structure for fanout 2
treeMem :: a                 -- ^ Base case
        -> (a -> a -> a)     -- ^ Combine
        -> (Int -> Int -> a) -- ^ Memoized function
treeMem base combine = treeMem'
  where treeMem' = multiMemo tree
        tree 0 _ = base
        tree _ 0 = base
        tree t b = treeMem' (t-1) b `combine` treeMem' (t-1) (b-1)

-- | @ slice00 t b @
--
--   The slice construction resulting from T1 and B1
--
--   An uneven choice of parameters @t@ and @b@ (@ t > b @) produces a wider
--   circuit than @ slice2 $ (t+b) \`div\` 2 @ by exploiting the absence of
--   restriction on fanout.
slice00 t b = stack $@ singleWire ||| t1Tree t b
                    $@ b1TreeWaist t b

-- | The resulting slices00 construction

slices00 d = foldl1 (|>) [slice00 k (d-k) | k <- [0 .. d-1]]

--

-- | @ partition' [p1, ..., pn] l @
--
--   Partition the second argument into sublists of sizes @p1@, ..., @pn@,
--   discards the remaining elements.
partition' :: [Int] -> [a] -> [[a]]
partition'       [] _ = []
partition' (p : ps) l = u : partition' ps v
  where (u, v) = splitAt p l

-- *** Fanout @ f >= 3 @

-- | slices(f)
--
--   Uses slices2 if @ f == 2 @
slices :: Int -> Int -> Net a
slices 2 d = slices2 d
slices f d = foldl1 (|>) [slice f k (d-k) | k <- [0 .. d-1]]

-- | @ slice f t b @
--
--   Defined for @ f >= 3 @
--
--   slice(f)
slice :: Int -> Int -> Int -> Net a
slice f t b = stack $@ singleWire ||| tTreef f t b
                    $@ bTreef f t b -- Includes the waist

-- | @ tTreef f t b @
--
--   T1 tree with max fanout @f@ and depth @ <= t @ matching @ bTree f t b @
tTreef :: Int -> Int -> Int -> Net a
tTreef _ 0 _ = singleWire
tTreef _ _ 0 = singleWire
tTreef f t b | f > t+1   = t1Tree t b
             | otherwise = tRootTrees $ tT : tTs
  where tTs @ (tT : _) = tTreef' f (b-1) `map` [t-f+2 .. t-1]

-- | @ bTreef f t b @
--
--   B1 tree with max fanout @f@ and depth @ <= b @ matching @ tTree f t b @
--
--   The first fan is extended to include the waist
bTreef :: Int -> Int -> Int -> Net a
bTreef _ 0 _ = twoWires
bTreef _ _ 0 = twoWires
bTreef f t b | f > t+2   = b1TreeWaist t b
             | otherwise = bRootTrees $ bT : bTs ++ [singleWire]
  where bTs @ (bT : _) = bTreef' f (b-1) `map` [t-f+2 .. t-1]

-- Add a "root" fan which covers
-- the first wire of every sub-network

bRootTrees :: [Net a] -> Net a
bRootTrees nets = Net n net'
  where ns = map width nets
        n  = sum ns
        net' f x = let s   = partition' ns x
                       fhs = f $ map head s
                       s'  = zipWith as_head_of fhs s
                       out = zipWith ($- f) nets s'
                   in concat out
        x `as_head_of` (_ : ys) = x : ys

-- Add "output" fans which cover the last wire of every sub-network

tRootTrees :: [Net a] -> Net a
tRootTrees nets = Net n net'
  where ns = map width nets
        n  = sum ns
        net' f x =
            let s      = partition' ns x
                s'     = zipWith ($- f) nets s
                y : ys = map last s'
                (acc, fys) = mapAccumL
                               (\ac v -> let [w, acc] = f [ac, v] in (acc, w))
                               y ys
                out    = zipWith with_last s' (fys ++ [acc])
            in concat out
        yi `with_last` y = init yi ++ [y]

-- B tree with max fanout @f@ and depth @ <= b @ matching @ tTree f t b @
bTreef' :: Int -> Int -> Int -> Net a
bTreef' = memo $ treefMem b1Tree bRootTrees

-- T tree with max fanout @f@ and depth @ <= t @ matching @ bTree f t b @
tTreef' :: Int -> Int -> Int -> Net a
tTreef' = memo $ treefMem t1Tree tRootTrees

--   Different order of b and t in @treefMem@ as opposed to @treeMem@!
-- | Memo. for parametric fanout @ >= 3 @
treefMem :: (Int -> Int -> a) -- ^ Base case (@ f > t+1 @ or @ b == 0 @)
         -> ([a] -> a)        -- ^ Combine
         -> Int               -- ^ Max fanout
         -> (Int -> Int -> a) -- ^ Memoized function

treefMem base combine f = treefMem
  where treefMem = multiMemo treef
        treef b t | f > t+1 || b == 0 = base t b
                  | otherwise         = combine $ c : cs
          where cs @ (c : _) = treefMem (b-1) `map` [t-f+1 .. t-1]

-- | Memoize function with multiple arguments.
--
--   This implementation applies memoization parameter-wise.
--
--   Hence it may be more efficient for higher order implementations
multiMemo :: (Memo a, Memo b) => (a -> b -> c) -> a -> b -> c
multiMemo = memo . (memo .)
