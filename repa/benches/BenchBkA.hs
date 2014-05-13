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
    [ bench "BrentKungA" $ whnf compute ua
    , bench "List"     $ nf (scanl1 op) rand
    ]
  where compute = runIdentity . brentKungA th op
        op = (+)
        n = 1000000
        th = 1024 -- Threshold
        rand = randomInts n
        ua = fromListUnboxed (ix1 n) $ rand

randomInts :: Int -> [Int]
randomInts n = take n $ randoms $ mkStdGen 1337
