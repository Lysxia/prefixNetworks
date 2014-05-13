module Main where

import Scans
import Data.Array.Repa
import Data.Array.Repa.Index (ix1)

--import Data.List

import Criterion.Main
import System.Random
import System.Environment (getArgs)

import Data.Functor.Identity

import Control.Exception

main = do
  s <- getArgs
  if compute ua `equalsS` fromListUnboxed (ix1 n) (scanl1 op rand)
    then return ()
    else throwIO $ AssertionFailed "Incorrect implementation"
  defaultMain
    [ bench name $ whnf compute ua ]
  where
    compute = runIdentity . scan op
    scan :: Monad m
         => (Int -> Int -> Int) -> Array U DIM1 Int -> m (Array U DIM1 Int)
    scan = fastScan 4
    name = "FastScan"
    op = (+)
    n = 1000000
    rand = randomInts n
    ua = fromListUnboxed (ix1 n) $ rand

randomInts :: Int -> [Int]
randomInts n = take n $ randoms $ mkStdGen 1337
