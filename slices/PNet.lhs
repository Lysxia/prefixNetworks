\section{Building blocks}

\begin{code}
module PNet (
  -- * Net type
    Net(..)
  , Fan
  , singleWire
  , twoWires
  , fanNet
  , opFan

  -- ** Operations
  , ($-)
  , (|>)
  , stack
  , (|||)

  , partition
  , (.+)
\end{code}

\ignore{
\begin{code}
  -- * Network information
  , size
  , depth
  , fanout

  , printCheck

  -- * Prefix networks
  -- ** Serial
  , serial

  -- ** Sklansky
  , sklanskyRec
  , sklansky
  , sklansky'
  , sklansky''

  -- ** Ladner-Fischer
  , lafi

  -- * Auxiliary functions
  , ($@)
  , multiMemo
  ) where

import Memo
import Data.List hiding ( partition )
\end{code}
}

A network is represented by the following type:

\begin{code}
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
\end{code}

\begin{code}

-- | As in [2], networks are parameterized by a \"fan\" component.
type Fan a = [a] -> [a]

-- | The only width 1 prefix network, does nothing
singleWire :: Net a
singleWire = Net { width = 1, net = const id }

-- | One gate
twoWires :: Net a
twoWires = Net { width = 2, net = id }

-- | Network with a single fan
fanNet :: Int -> Net a
fanNet n = Net n id

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

--

-- | @ partition [p1, ..., pn] l @
--
--   Partition the second argument into sublists of sizes @p1@, ..., @pn@,
--   discards the remaining elements.
partition :: [Int] -> [a] -> [[a]]
partition       [] _ = []
partition (p : ps) l = u : partition ps v
  where (u, v) = splitAt p l

\end{code}

Sklansky recursion pattern. Fig.~\ref{fig:sklanskyrec}.

\begin{code}
sklanskyRec :: Net a -> Net a -> Net a
sklanskyRec a b
  = a |> (stack (singleWire ||| b)
              $ fanNet (width b + 1))
\end{code}

\begin{figure}
\input{sklanskyrec}
\caption{\label{fig:sklanskyrec}}
\end{figure}

\ignore{
\begin{code}
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
\end{code}
}

\ignore{
\begin{code}
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
sklansky' :: Int -> Net a
sklansky' n = Net n (net' n)
  where net' 1 _ x = x
        net' n f x = let (x1, x2) = splitAt (n `div` 2) x
                         s1       = net'      (n `div` 2)  f x1
                         s2       = net' (n - (n `div` 2)) f x2
                         (t1, t2) = splitAt ((n `div` 2) - 1) s1
                     in t1 ++ f (t2 ++ s2)

-- | Sklansky construction (ter)
--
-- Obtained by splitting at powers of 2
sklansky'' :: Int -> Net a
sklansky'' n = Net n $ divide $ prevPow2 n
  where prevPow2 0 = 0
        prevPow2 1 = 1
        prevPow2 n = 2 * prevPow2 (n `div` 2)
        divide _ _ [] = []
        divide _ _ [x] = [x]
        divide n f xs | null x2   = divide (n `div` 2) f xs
                      | otherwise = y1 ++ z
          where (x1, x2) = splitAt n xs
                y1'      = divide (n `div` 2) f x1
                y3       = divide (n `div` 2) f x2
                (y1, y2) = splitAt (n-1) y1'
                z        = f $ y2 ++ y3

checkSklansky = printCheck $ sklansky 20

-- | Ladner-Fischer, depth @d@, slack @k@
lafi :: Int {-^ Width -} -> Int {-^ Slack -} -> Net a
lafi n k = Net (2 ^ n) $ net' n k
  where
    net' 0 _ _ (xs@[_]) = xs
    net' n 0 f xs =
      let
        (x1, x2) = splitAt (2 ^ (n - 1)) xs
        y1 = net' (n - 1) 1 f x1
        y2' = net' (n - 1) 0 f x2
        y2 = f $ last y1 : y2'
      in init y1 ++ y2
    net' n k f xs =
      let
        xs' = map f $ pair xs
        x1 = map head xs'
        x2 = xs' >>= tail
        y2 = net' (n - 1) (k - 1) f x2
        z = concat . zipWith (\a b -> f [a, b]) y2 $ tail x1
      in head x1 : z ++ [last y2]
    pair []           = []
    pair (x : y : xs) = [x, y] : pair xs

-- | Memoize function with multiple arguments.
--
--   This implementation applies memoization parameter-wise.
--
--   Hence it may be more efficient for higher order implementations
multiMemo :: (Memo a, Memo b) => (a -> b -> c) -> a -> b -> c
multiMemo = memo . (memo .)

infixr 9 .+
(.+) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.+) = (.) . (.)
\end{code}
}
