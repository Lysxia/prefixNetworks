This module implements the construction of a prefix network with minimal depth.

\section{Building blocks}

\begin{code}
module Shallowest.Construct where

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
\end{code}

A network is represented by the following type:
\begin{code}% Not part of the module
data Net a = { width :: Int, net :: Fan a -> [a] -> [a] }
\end{code}

etcetc... (TODO: separate common definitions from WSO specific ones,
and put the former into lhs format)

\begin{code}
-- | Network with a single fan
fanNet :: Int -> Net a
fanNet n = Net n id
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

Our construction should produce networks of all widths $n$.
Furthermore, it consists in several ways to
compose two networks of approximately the same width.
When the width a power of two $n=2^d$,
the division in halves is straightforward.

Different solutions can be proposed:

\begin{itemize}
  \item Construct a network of width $2^{\ceil{\log_2 n}}$, and truncate it.
  \item Divide the width by 2 and round:
    $\ceil{\frac{n}2}$ and $\floor{\frac{n}2}$.
  \item Make one of the parts a power of two:
    $2^{\ceil{\log_2 n}-1}$ and the rest.
\end{itemize}

Here we consider the second option,
using the ceiling as the width of the left half.

\begin{code}
halfOf :: Int -> Int
halfOf n = (n + 1) `div` 2
-- Try n `div` 2?
-- Greatest power of 2?
\end{code}


\section{Construction}

The prefix network \lstinline$make n$
has width $n$ and depth $\ceil{\log_2 n}$.
Its right half is defined recursively using the same function.%
\footnote{\lst$make n$ may be \lst$make1 n 0$?}

\begin{code}
make :: Int {- ^ Width -} -> Net a
make 1 = singleWire
make n = sklanskyRec left right
  where
    half = halfOf n
    left = make1 half 1
    right = make (n - half)
\end{code}

The left subnetwork is a particular case of \lst$make1 n k$,
which is a \lst$n$-input prefix network with slack \lst$k$.
Again, the network is made of two halves,
the left one is defined with the same function.

\begin{code}
make1 :: Int {- ^ Width -} -> Int {- ^ Slack -} -> Net a
make1 1 _ = singleWire
make1 n k = left |> right
  where
    half = halfOf n
    left = make1 half (k + 1)
    right = slice (n - half + 1) k
\end{code}

The right subsubnetwork, \lst$slice n k$,
is an \lst$n$-input prefix network,
whose first input is indented at depth $\ceil{\log_2(\text{\lst$n$} - 1)}$
and must be the left parent of the last output restricted at depth
$\ceil{\log_2(\text{\lst$n$}-1)} + 1$.
The total depth of \lst$slice n k$ is $\ceil{\log_2(\text{\lst$n$}-1)}+1+k$.
The difference with the depth of the last output,
\lst$k$ is called the \emph{slack} of the network.

\begin{code}
slice :: Int {- ^ Width -} -> Int {- ^ Slack -} -> Net a
slice 1 _ = singleWire
slice n k = Net n net'
  where
    net' f (x0 : xs) = zs ++ [y0]
      where
        (ts, zs) = openSlice (n - 1) (k + 1) k f xs ys
        y0 : ys = reverse . f $ x0 : reverse ts
\end{code}

The following is a peculiar ``slice'', \lst$openSlice n k d'$.

\begin{code}
openSlice
  :: Int {- ^ Width -} -> Int {- ^ Slack -} -> Int {- ^ Co-depth -}
  -> (Fan a -> [a] -> [a] -> ([a], [a]))
\end{code}

\paragraph{An interpretation}
It can be seen as an \emph{incomplete} prefix network with $n+1$ inputs,
$x_0,\dots,x_n$, computing ${x_0\circ\dots\circ x_i}_{0\leq i\leq n}$.%
\footnote{This is the interpretation of its usage in \lst$slice$,
but \lst$openSlice$ is a generalization which also includes
a structure used in its own definition.
This factors two pieces of code which looked very similar.}

It takes two sets of inputs
\lst$xs$ (\emph{primary}) and \lst$ys$ (\emph{secondary}),
and produces two sets of outputs
\lst$ts$ (\emph{secondary}) and \lst$zs$ (\emph{primary}).

The network is pictured with a ``hole'' in the middle,
separating a \emph{top} subnetwork from a \emph{bottom} one;
see Fig.~\ref{fig:openSlice}.
In the top subnetwork,
the primary inputs \lst$xs$,
corresponding to $x_1,\dots,x_n$ (note the absence of $x_0$),
are first combined to produce
their sum $y'_n\define x_1\circ\dots\circ x_n$,
yielding a number of intermediate prefixes
$y'_i\define x_1\circ\dots\circ x_i$ in the process.
The total sum and some of these intermediate prefixes $y'_i$
are output as \lst$ts$,
\emph{from right to left},
while the other intermediate values ``cross the hole'' unaffected.

The hole is filled---from outside this function---with a fan%
\footnote{in the context of this interpretation.
The function definition exploits the generality of this hole
in more elaborate ways.}
adding $x_0$ to every $y'_i$,
resulting in prefixes $y_i\define x_0\circ\dots\circ x_i$,
present in the secondary input \lst$ys$, also from right to left.
The rightmost value $y_n$ is extracted from the hole
and it is not fed back in \lst$ys$,
whereas the leftmost value $x_0$ is introduced at the end of \lst$ys$.

These new inputs are used in the bottom subnetwork
to finally obtain a prefix network on $x_0,\dots,x_n$.

\begin{figure}
  \input{openSlice}
  \caption{\label{fig:openSlice}}
\end{figure}

The corresponding usage is the following:

\begin{code}%skipped
fan :: [a] -> [a]
input :: [a] -- length : n + 1

x0 : xs = input
(ts, zs) = openSlice n k d xs ys
y0 : ys = reverse (fan (x0 : reverse ts))
output = x0 : zs
\end{code}

Although not apparent in this usage,
the order of dependencies is as such:
\lst$xs$, \lst$ts$, \lst$ys$, \lst$zs$.

\begin{code}
openSlice n k d' f xs ys
  | n == 1 = (xs, ys)
  | d' == 0 = (reverse $ make1 n (k - 1) $- f $ xs, reverse ys)
    -- equiv. to (reverse $ scanl1 (?) xs, reverse $ ys)
  | otherwise =
    let
      (t1, z1) = openSlice      half  (k + 1)      d'  f x1 $ tail ys
      (t2, z2) = openSlice (n - half) (k + 1) (d' - 1) f x2 y2
      y2 = reverse . f $ head ys : (reverse $ tail t2)
      [t', t_] = f [head t1, head t2] 
      ts = t_ : t' : tail t1
      zs = z1 ++ z2
    in (ts, zs)
  where
    half = halfOf n
    (x1, x2) = splitAt half xs
\end{code}

\ignore{
\begin{code}%ignored
{-
-- | Slice with slack
-- and co-depth of the special input
-- (@d - d'@, @d@: depth of the network, @d'@: depth of the first input).
-- 
-- > openSlice n k d' f t x xs = (y, zs, y')
-- > n == length xs
-- > zs == scanl1 (?) (t : xs)
-- > y == head . f $ x : _ -- Through one fan
-- > y' == foldl (?) (x : xs)
--
openSlice :: Int {- ^ Width -} -> Int {- ^ Slack -} -> Int {- ^ Co-depth -}
      -> (Fan a -> a -> a -> [a] -> (a, [a], a))
openSlice n k d' f t x xs
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
        $ scanl4 openSlice'
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
    openSlice' (_, _, y') m k t xs = openSlice m k (d' - 1) f t y' xs
    {- Test code
    openSlice' (_, _, y') m k t xs
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
openSliceTop
  :: Int {- ^ Width -} -> Int {- ^ Slack -} -> Int {- ^ Co-depth -}
  -> (Fan a -> [a] -> [[a]])
openSliceTop n k d' f xs =
  | d' == 1   = net $ make1 n (k + 1)
  | otherwise =
    let
      hlv = halves n
      parts = partition hlv xs
      tops -- Build top tree on every part
        = zipWith5 openSliceTop
            hlv
            (reverse [k + 1 .. k + length hlv])
            (repeat $ d' - 1)
            (repeat f)
            parts
      tops'
        = onLasts (scanl1 (\x y -> let [x', y'] = f [x, y] in (x', y'))) tops
    in tops'

openSliceBot
  :: Int {- ^ Width -} -> Int {- ^ Slack -} -> Int {- ^ Co-depth -}
  -> 
openSliceBot n k d' f x xs =
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
\end{code}
}
