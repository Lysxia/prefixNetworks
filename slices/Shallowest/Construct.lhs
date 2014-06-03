This module implements the construction of a prefix network with minimal depth.

\section{Half function}

\begin{code}
module Shallowest.Construct where

import PNet
import Data.List hiding ( partition )
\end{code}

Our construction should produce networks of all widths $n$.
Furthermore, it consists in several ways to
compose two networks of approximately the same width (a ``half'').
When the width a power of two $n=2^d$,
the division in halves is straightforward.

Different solutions can be proposed
in the general case to determine a ``half'':

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
\footnote{\lst$make n$ may be \lst$make1 n 0$?\ldots}

\begin{code}
make
  :: Int {- ^ Width -}
  -> Net a
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
make1
  :: Int {- ^ Width -}
  -> Int {- ^ Slack -}
  -> Net a
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
slice
  :: Int {- ^ Width -}
  -> Int {- ^ Slack -}
  -> Net a
slice 1 _ = singleWire
slice n k = Net n net'
  where
    net' f input = output
      where
        -- (...)
\end{code}

We will explain the definition of \lst$slice$.
Note the peculiar usage of \lst$openSlice$,
containing an apparently circular definition.

\begin{code}
  --  where
        x1 : xs = input
        (ts, ys) = openSlice (n - 1) (k + 1) k f xs us
        yn : us = reverse . f $ x1 : reverse ts
        output = ys ++ [yn]
\end{code}

The function \lst$openSlice$ has type:

\begin{code}
openSlice
  :: Int {- ^ Width -}
  -> Int {- ^ Slack -}
  -> Int {- ^ Co-depth -}
  -> (Fan a -> [a] -> [a] -> ([a], [a]))
\end{code}

\lst{openSlice (n-1) k d'} can be seen%
\footnote{This is the interpretation of its usage in \lst$slice$,
but \lst$openSlice$ is a generalization which also comprises
a structure used in its own definition.
This factors two pieces of code which looked very similar.}
as an \emph{incomplete} prefix network with $n$ inputs ($n\geq 2$),
$x_1,\dots,x_n$, computing $\{y_i\}_{i=1,\dots,n}$.

\begin{equation}
  y_i \define x_1\circ\dots\circ x_i,\,i=1,\dots,n.
\end{equation}

\lst$openSlice$ takes two sets of inputs
\lst$xs$ (\emph{primary}) and \lst$us$ (\emph{secondary}),
and produces two sets of outputs
\lst$ts$ (\emph{secondary}) and \lst$ys$ (\emph{primary}).

Contrary to what the type signature of \lst$openSlice$ may suggest,
the order of dependencies is:
\lst$xs$, \lst$ts$, \lst$us$, \lst$ys$.
Hence, this implementation makes extensive use of laziness in Haskell.

The network is pictured with a ``hole'' in the middle,
separating a \emph{top} subnetwork from a \emph{bottom} one;
see Fig.~\ref{fig:openSlice}.
In the top subnetwork,
the primary inputs \lst$xs$,
corresponding to $x_2,\dots,x_n$ (note the absence of $x_1$),
are first combined to produce their sum $t_n$,
yielding a couple of intermediate prefixes $\{t_i\}_{i\in S}$
in the process.

\begin{equation}
  t_i \define x_2\circ\dots\circ x_i,\, i\in S\subset \{2,\dots,n\}.
\end{equation}

The total sum and some of these prefixes
$\{t_i\}_{i\in T}$, with $T\subset S$,
are output as \lst$ts$,
\emph{from right to left},
while other values ``cross the hole'' unaffected.
In the present implementation, $S=T$,
and they correspond exactly to the right ends of spine nodes
in the top subnetwork.
But the optimal choice of $S$ and $T$ is the object of current work.

The hole is filled using the following line from the clause above%
\footnote{in the context of this interpretation.
The definition of \lst$openSlice$ exploits the generality of this hole
to insert more complex combinations of fans.}:

\begin{code}%
        yn : us = reverse . f $ x1 : reverse ts
\end{code}
\ignore{$}%Syntactic coloration is messed up in my editor...

It adds $x_1$ to every $t_i$,
resulting in prefixes $\{y_i\}_{i\in T}$
present in the secondary inputs \lst$us$, also from right to left.

The rightmost value $y_n$ is extracted from the hole
and it is not fed back in \lst$us$,
whereas the leftmost value $y_1=x_1$ is introduced at the end of \lst$us$.

\begin{equation}
  y_i = x_1\circ t_i
\end{equation}

These new inputs are used in the bottom subnetwork
to complete the prefix sum.
The values $\{y_i\}_{i=1,\dots,n-1}$ are output in \lst$ys$.
(Recall that the last output, $y_n$, was removed in the hole,
so \lst$openSlice$ doesn't ``see'' it.)

As its first argument, \lst$openSlice$ expects \lst$length xs$,
which is one less than $n$.
The \emph{slack} \lst$k$ is the number of levels
between the secondary output \lst$ts$ and the bottom of the network.
The \emph{codepth} \lst$d'$ is the number of levels
between the secondary input \lst$us$ and the bottom of the network.
See Fig.~\ref{fig:openSlice}.

\begin{figure}
  \input{openSlice}
  \caption{\label{fig:openSlice}
  Representation of \lst$openSlice$}
\end{figure}

\begin{code}
openSlice m k d' f xs us
  | m == 1 = (xs, us)
  | d' == 0 = (reverse $ make1 m (k - 1) $- f $ xs, reverse us)
  | otherwise =
    let
      (t1, y1) = openSlice      half  (k + 1)      d'  f x1 $ tail us
      (t2, y2) = openSlice (m - half) (k + 1) (d' - 1) f x2 u2
      u2 = reverse . f $ head us : (reverse $ tail t2)
      [t', t_] = f [head t1, head t2] 
      ts = t_ : t' : tail t1
      ys = y1 ++ y2
    in (ts, ys)
  where
    half = halfOf m
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
