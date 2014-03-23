{--
Copyright (c) 2009, Koen Claessen
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Chalmers University of Technology nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY Koen Claessen ''AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL Mary Sheeran BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--}



{-# LANGUAGE DeriveGeneric, DefaultSignatures, FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Memo    
  ( Memo(..)
  , memoConv -- :: Memo a => (a -> a') -> (a' -> a) -> (a' -> b) -> (a' -> b)
  , memoShow -- :: (Show a, Read a) => (a -> b) -> (a -> b)
  )
 where

import GHC.Generics
import Data.Char

--------------------------------------------------------------------------

{-
Given a recursive function definition:

  f x = ... f ...

To memoize f, instead of the above, write this:

  f = memo f'
   where
    f' x = ... f ...

for some new function symbol f'.

Note:

(1) f (not f') is still used on the right hand side in the definition
    of f',

(2) do not add extra arguments in the definition of f,
    otherwise memoization does not work.

Now, f is a memoized function!

If f has two arguments:

  f x y = ... f ...

then memoize f like this:

  f = memo (\x -> memo (\y -> f' x y))
   where
    f' x y = ... f ...

(or this:

  f = memo (memo . f')
   where
    f' x y = ... f ...

:-)
-}

--------------------------------------------------------------------------
-- an example

-- unmemoized version of fibonacci
fib0 :: Integer -> Integer
fib0 0 = 1
fib0 1 = 1
fib0 n = fib0 (n-1) + fib0 (n-2)

-- memoized version of fibonacci
fib1 :: Integer -> Integer
fib1 = memo fib1'
 where
  fib1' 0 = 1
  fib1' 1 = 1
  fib1' n = fib1 (n-1) + fib1 (n-2)

--------------------------------------------------------------------------

class GMemo f where
  gmemo :: (f a -> c) -> f a -> c

instance GMemo U1 where
  gmemo f = \_ -> fU1
   where
    {-# NOINLINE fU1 #-}
    fU1 = f U1

instance (GMemo a, GMemo b) => GMemo (a :+: b) where
  gmemo f = \x -> case x of
                      L1 a -> fL1 a
                      R1 b -> fR1 b
   where
    {-# NOINLINE fL1 #-}
    fL1 = gmemo (f . L1)
    {-# NOINLINE fR1 #-}
    fR1 = gmemo (f . R1)

instance (GMemo a, GMemo b) => GMemo (a :*: b) where
  gmemo f = \(a :*: b) -> fProd a b
   where
    {-# NOINLINE fProd #-}
    fProd = gmemo (\a -> gmemo (\b -> f (a :*: b)))

instance GMemo f => GMemo (M1 i c f) where
  gmemo f = \(M1 x) -> fM1 x
    where
      {-# NOINLINE fM1 #-}
      fM1 = gmemo (f . M1)

instance Memo c => GMemo (K1 i c) where
  gmemo f = \(K1 c) -> fK1 c
    where
      {-# NOINLINE fK1 #-}
      fK1 = memo (f . K1)

class Memo a where
  memo :: (a -> c) -> a -> c

  default memo :: (Generic a, GMemo (Rep a)) => (a -> c) -> a -> c
  memo = (. from) . gmemo . (. to)

--------------------------------------------------------------------------

instance Memo ()
instance Memo Bool

instance Memo a => Memo (Maybe a)
instance Memo a => Memo [a]
instance (Memo a, Memo b) => Memo (Either a b)

instance (Memo a, Memo b) => Memo (a,b)
instance (Memo a, Memo b, Memo c) => Memo (a,b,c)
instance (Memo a, Memo b, Memo c, Memo d) => Memo (a,b,c,d)

--------------------------------------------------------------------------

memoConv :: Memo a => (a -> a') -> (a' -> a) -> (a' -> b) -> (a' -> b)
memoConv back forth f = memo (f . back) . forth

memoShow :: (Show a, Read a) => (a -> b) -> (a -> b)
memoShow = memoConv read show

--------------------------------------------------------------------------

data N a
  = Zero
  | Minus1
  | Bool :+ a
  deriving Generic

instance Memo a => Memo (N a)

memoNum :: (Integral a, Memo a) => (a -> b) -> (a -> b)
memoNum = memoConv back forth
 where
  forth 0    = Zero
  forth (-1) = Minus1
  forth n    = odd n :+ (n `div` 2)

  back Zero     = 0
  back Minus1   = (-1)
  back (b :+ n) = (if b then 1 else 0) + 2*n

instance Memo Int where
  memo = memoNum

instance Memo Char where
  memo = memoConv chr ord

instance Memo Integer where
  memo = memoNum

instance Memo Float where
  memo = memoConv (uncurry encodeFloat) decodeFloat

instance Memo Double where
  memo = memoConv (uncurry encodeFloat) decodeFloat

--------------------------------------------------------------------------


