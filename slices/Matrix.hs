module Matrix where

import PNet (
  Net (..),
  opFan )
import Data.List ( insert, foldl' )

type Trace = ([(Int,Int)], (Int, Int))
type Matrix = [[Bool]]

(!&) :: Matrix -> Int -> Int -> Bool
(!&) m i j = m !! j !! i

matrix
  :: Net Trace -> Matrix
matrix (Net n net)
  = [ [ (i,j) `elem` tr | i <- [0 .. j] ] | j <- [0 .. n - 1] ]
  where tr = concat . map fst
           $ net (opFan traceBinOp)
                 [ ([c],c) | x <- [0 .. n - 1], let c = (x,x) ]

traceBinOp :: Trace -> Trace -> Trace
traceBinOp (is, (i,j)) (ks, (j',k))
  | j + 1 == j' = (c : is ++ ks, c)
  | otherwise = error $ show i ++ " " ++ show j ++ " " ++ show j' ++ " "++ show k
  where c = (i, k)

{-
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs'@(x : xs) ys'@(y : ys)
  = case compare x y of
      EQ -> x : merge xs ys
      LT -> x : merge xs ys'
      GT -> y : merge xs' ys
-}
