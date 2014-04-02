module Main where

import Scans
import Data.Array.Repa (fromListUnboxed, equalsS)
import Data.Array.Repa.Index (ix1)
import qualified Data.Vector.Unboxed as Unboxed (scanl1', fromList, sum)

import Criterion.Main
import System.Random

import Data.Functor.Identity

import Control.Exception
import Control.DeepSeq

main = do
  check
  defaultMain
    [ bench "List"     $ nf (last . scanl1 (+)) rand
    , bench "Vector"   $ whnf (Unboxed.sum) va
    , bench "ScanS"    $ whnf compute ua
    ]
  where compute = scanS op
        check | correct   = return ()
              | otherwise = throwIO $ AssertionFailed "Incorrect implementation"
        correct = compute ua `equalsS` fromListUnboxed (ix1 n) (scanl1 op rand)
        op = (+)
        n = 100000 :: Int
        ua = fromListUnboxed (ix1 n) $ rand
        va = Unboxed.fromList rand
        rand = randomInts n

randomInts :: Int -> [Int]
randomInts n = take n $ randoms $ mkStdGen 1337
