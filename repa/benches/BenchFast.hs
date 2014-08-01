module Main where

import Scans

import Control.Exception
import Control.Monad

import Criterion.Main

import Data.Array.Repa
import Data.Array.Repa.Index (ix1)
import Data.Array.Repa.Repr.Vector (V)
import Data.Functor.Identity

--import Data.List

import System.Random
import System.Environment (getArgs)

main = do
  s <- getArgs
--  unless (compute ua `equalsS` fromListUnboxed (ix1 n) (scanl1 op rand))
--    (throwIO $ AssertionFailed "Incorrect implementation")
  defaultMain
    [ bench name $ whnf compute ua ]
  where
    compute = runIdentity . scan op
    scan :: (Int -> Int -> Int) -> Array U DIM1 Int -> Identity (Array U DIM1 Int)
    scan = fastScan 2
    name = "FastScan"
    op = (*)
    n = 1000000
    rand = randomInts n
    ua = fromListUnboxed (ix1 n) $ rand

randomInts :: Int -> [Int]
randomInts n = take n $ randoms $ mkStdGen 1337
