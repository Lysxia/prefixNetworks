The following construction demonstrates that
with an additional assumption of invertibility,
zero-deficiency%
\footnote{$d+s=2n-2$, i.e., no extra gate, spine length = depth}
can be achieved at depths smaller than $\log_\phi n$,
resulting in smaller sizes compared to more general solutions
(since $d+s>2n-2$).

\begin{code}

module Xor.Construct where

import Data.List

\end{code}

\section{Representation of networks}

Networks are parameterized by
pairs of operations: ($+$, $-$),
such that $+$ is associative and

\[(x_0+\dots+x_i+\dots+x_n)-(x_{i+1}+\dots+x_n) = x_0+\dots+x_i\]

An example is the \emph{exclusive or} operation
(both as addition and subtraction),
$+=-=\oplus$.

That parametrization is abbreviated with the type synonym \lst$AddSub$.
We will omit these parameters in our descriptions,
so that \lst$foo :: AddSub a t$ is identified with \lst$foo (+) (-) :: t$
for some appropriate \lst$(+)$ and \lst$(-)$.

\begin{code}

type Operation a = a -> a -> a

-- | Addition -> Subtraction -> u
type AddSub a u =  Operation a -> Operation a -> u

data XNet a = XNet {
  xWidth :: Int,
  xNet :: AddSub a ([a] -> [a])
  }

-- | Simplest network
singleWire :: XNet a
singleWire = XNet 1 (\_ _ -> id)

-- | Network composition
(XNet n u) |> (XNet m v) = XNet (n + m - 1) w
  where
    w (+) (-) xs
      = let (x1, x2) = splitAt n xs
            y1 = u (+) (-) x1
            y2 = v (+) (-) (last y1 : x2)
        in init y1 ++ y2

\end{code}

\section{Zero-deficiency network}

A zero-deficiency network is a composition of slices.
\lst$slice$ wraps a more general structure, \lst$slice'$.

\begin{code}

zdNet
  :: Int -- ^ Max depth
  -> XNet a
zdNet d = foldl' (|>) singleWire [slice d k | k <- [1 .. d]]

slice :: Int -> Int -> XNet a
slice d k = XNet (n + 1) u
  where
    (n, u') = slice' d (k-1) k (k-1)
    u (+) (-) (x : xs)
      = let (sum_xs, continue) = u' (+) (-) xs
        in x : continue x (x + sum_xs)

\end{code}

\section{Slices}

Explanation of \lst$slice'$, for $\ell,r\in [e,d]$:

\begin{code}% Not compiled

-- Usage of slice'
n :: Int
u :: AddSub a ([a] -> (a, a -> a -> [a]))
(n, u) = slice' d l r e
\end{code}

\lst$u$ implements a prefix sum on $n+1$ inputs (as wide as possible)
in the following way:

\begin{itemize}
  \item We separate the head and tail of the input,
    \lst$y : xs$ (\lst$xs$ has length $n$).
  \item Precondition:
    We assume that \lst$y$ has depth $\ell$,
    and that the sum \lst$sum_y_xs$
    (i.e., last output in the prefix sum) of \lst$(y : xs)$
    has depth $r$ and is obtained outside of this function.
  \item \lst$(sum_xs, continue) = u xs$ produces
    the sum of \lst$xs$ at depth $e$, and a continuation.
    \lst$sum_xs$ might be used to obtain \lst$sum_y_xs$, but not necessarily,
    although we still require that this equation holds:

    \lst$sum_y_xs == y + sum_xs$.
  \item \lst$continue y sum_y_xs$ produces
    the tail of the prefix sum of \lst$(y : xs)$,
    (i.e., it includes \lst$sum_y_xs$ but excludes \lst$x$)
    with maximum depth $d$ overall.
\end{itemize}

It is possible to truncate the network (with minor fixes)
to obtain narrower ones.

\begin{code}

slice'
  :: Int -> Int -> Int -> Int
  -> (Int, AddSub a ([a] -> (a, a -> a -> [a])))
slice' d l r e
  | (d == l && l == r) || e == 0 = (1, \_ _ ls@[x] -> (x, \_ x' -> [x']))
  | d == r || (d > l && l <= r) = sliceLeft d l r e
  | d == l || (d > r && l > r) = sliceRight d l r e
\end{code}

\section{Slice construction}

This is a construction based on the general case without subtraction,
by adding another way to combine slices with smaller parameters.

Let $x[1;n]\define x_1+\dots+x_n$.

\lst$sliceLeft$ and \lst$sliceRight$ are almost identical.
They could be factored.

\lst$y_x1$ corresponds to the sum $(y+x[1;n])$ and
is used as the last output of the left half (\lst$u$),
and the first input of the right half (\lst$v$).

\lst$sliceLeft$ and \lst$sliceRight$ differ in that $(y+x[1;n])$
can be computed:

\begin{itemize}
  \item (left) Either at depth $\ell+1$,
    from the first input $y$ (depth $\ell$)
    and a value from the skeleton
    $x[1;n]$ (\lst$sum_x1$ from \lst$u x1$, depth $e-1 < \ell$).
    That correponds to the original recursion pattern of slices
    (Zhu et al.).
  \item (right) Or at depth $r+1$,
    from the last output $y+x[1;n+m]$ (depth $r$)
    and another value from the skeleton
    $x[n+1;m]$ (\lst$sum_x2$ from \lst$v x2$, depth $e-1 < r$).
\end{itemize}
We choose the option resulting in the shallowest output,
while satisfying the depth constraints.

\begin{code}

sliceLeft
  :: Int -> Int -> Int -> Int
  -> (Int, AddSub a ([a] -> (a, a -> a -> [a])))
sliceLeft d l r e = (n + m, w)
  where
    (n, u) = slice' d l (l+1) (e-1) -- Different from sliceRight
    (m, v) = slice' d (l+1) r (e-1) -- 
    w (+) (-) xs
      = let (x1, x2) = splitAt n xs
            (sum_x1, continue_u) = u (+) (-) x1
            (sum_x2, continue_v) = v (+) (-) x2
            continue y y_xs = continue_u y y_x1 ++ continue_v y_x1 y_xs
              where y_x1 = y + sum_x1 -- Different from sliceRight
        in (sum_x1 + sum_x2, continue)

sliceRight
  :: Int -> Int -> Int -> Int
  -> (Int, AddSub a ([a] -> (a, a -> a -> [a])))
sliceRight d l r e = (n + m, w)
  where
    (n, u) = slice' d l (r+1) (e-1) -- Different from sliceLeft
    (m, v) = slice' d (r+1) r (e-1) --
    w (+) (-) xs
      = let (x1, x2) = splitAt n xs
            (sum_x1, continue_u) = u (+) (-) x1
            (sum_x2, continue_v) = v (+) (-) x2
            continue y y_xs = continue_u y y_x1 ++ continue_v y_x1 y_xs
              where y_x1 = y_xs - sum_x2 -- Different from sliceLeft
        in (sum_x1 + sum_x2, continue)
\end{code}

\section{Auxiliary functions}

\begin{code}
-- | Checks that a net implements a prefix network. 
-- (Probably) correct when the net is polymorphic.
check :: XNet (Int, Int) -> Bool
check xn@(XNet n _) = check' xn == [(1,x) | x <- [1 .. n]]

check' :: XNet (Int, Int) -> [(Int,Int)]
check' (XNet n u) = u (.+) (.-) [(x,x) | x <- [1 .. n]]
  where
    (.+) (i,j) (j',k) | j+1 == j' = (i,k)
    (.-) (i,k) (j,k') | k == k' && i < j = (i,j-1)

check20 = check $ zdNet 20 :: Bool

-- | Depth of a net
xDepth = maximum . xDepths

xDepths :: XNet Int -> [Int]
xDepths (XNet n u) = u (.+) (.-) (replicate n 0)
  where (.+) x y = 1 + max x y ; (.-) = (.+)
\end{code}
