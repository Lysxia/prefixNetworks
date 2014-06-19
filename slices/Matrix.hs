module Matrix where

import PNet (
  Net (..),
  opFan,
  ($-),
  (.+)
  )
import Tikz

import Text.Printf
import qualified Data.Map as M
import Data.Function
import Data.Maybe

type Map = M.Map
data Trace = Trace {
  edges :: Map (Int,Int) Int,
  current :: (Int, Int)
  }
type Matrix = [[Maybe Int]]

(!&) :: Matrix -> Int -> Int -> Maybe Int
(!&) m i j = m !! j !! i

clamp :: Int -> Int -> [a] -> [a]
clamp i n = take n . drop i

clampMatrix :: Int -> Int -> Int -> Int -> [[a]] -> [[a]]
clampMatrix i m j n = map (clamp i m) . clamp j n

matrix
  :: Net Trace -> Matrix
matrix net@(Net n _)
  = [ [ M.lookup (i,j) tr | i <- [0 .. j] ] | j <- [0 .. n - 1] ]
  where
    tr = M.unions . map edges
       $ net $- opFan traceBinOp $ input
    input = [ Trace (M.singleton c x) c | x <- [0 .. n - 1], let c = (x,x) ]

traceBinOp :: Trace -> Trace -> Trace
traceBinOp (Trace is (i,j)) (Trace ks (j',k))
  | j + 1 == j' = Trace (M.insert c j $ is `M.union` ks) c
  | otherwise = error $
                  show i ++ " " ++ show j ++ " " ++ show j' ++ " "++ show k
  where c = (i, k)

defaultRadius = 0.1 :: Double
defaultOut = -70 :: Double
defaultIn = -170 :: Double

tikzMatrix :: Double -> Matrix -> String
tikzMatrix scale = unlines . map coordToTikz . pointList
  where
    pointList = catMaybes . concat . zipWith (zipWith (fmap . const)) ijs
    ijs = [ [ (i, j) | i <- [0 ..] ] | j <- [0 ..] ]
    coordToTikz = uncurry (flip tikzCircle `on` ((* scale) . fromIntegral))

tikzHLines :: Double -> Matrix -> String
tikzHLines scale = unlines . map coordsToTikz . pointsList
  where
    pointsList = catMaybes . concat . zipWith (zipWith (fmap . leftP)) ijs
    leftP (i,k) j = ((i, j),(i, k))
    ijs = [ [ (i, j) | i <- [0 ..] ] | j <- [0 ..] ]
    coordsToTikz
      = uncurry (tikzCurve defaultOut defaultIn `on` flippedFromIntegral)
    flippedFromIntegral (i,j) = (scale * fromIntegral j, scale * fromIntegral i)

tikzPicMatrix :: Double -> Matrix -> String
tikzPicMatrix scale
  = tikzEnvironment . tikzScope "" . (tikzDefineDouble "r" (scale * 0.1) ++)
  . tikzMatrix scale

tikzVLines :: Double -> Int -> String
tikzVLines scale n
  = printf "\\foreach \\i in {1,...,%d}\n\
           \  \\draw[densely dotted] (\\i*%0.3f,\\i*%0.3f) -- (\\i*%0.3f,0);\n"
           (n-1) scale scale scale

tikzPicLines :: Double -> Matrix -> String
tikzPicLines scale m
  = tikzEnvironment $ defRad ++ defMat m ++ matScope ++ linScope
  where
    defMat = tikzDefine "matr" . tikzMatrix scale
    defRad = tikzDefineDouble "r" (scale * 0.1)
    matScope = tikzScope "" "\\matr\n"
    linScope = tikzScope (printf "xshift=%0.3fcm" (scale * fromIntegral (n+5)))
             $ vLines ++ tikzHLines scale m ++ "\\matr\n"
    vLines = tikzVLines scale n
    n = length m

