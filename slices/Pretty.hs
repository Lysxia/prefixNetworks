module Pretty (
    Net

  -- * \"Pretty\" printing
  , FanPos
  , printNet
  , printNetV

  , tikzOfNet
  , tikzOfFan

  , renderNetFile
  ) where

import WSO
import Data.List
import Data.Char

import Text.LaTeX.Packages.TikZ
import Text.LaTeX.Packages.TikZ.Syntax
import Text.LaTeX.Packages.TikZ.PathBuilder
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Syntax

-- * \"Pretty\" printing

-- | The type used to collect fan positions
data FanPos = FanPos
  { wireNum  :: Int            -- ^ Wire identifier
  , curDepth :: Int            -- ^ Current depth of the wire
  , fans     :: [(Int, [Int])] -- ^ Fans and their depth
  }
  deriving Show

-- | Print an ASCII drawing of the network
printNet :: Net FanPos -> IO ()
printNet = printLines . drawNet

-- | Print circuit vertically
--   (wide circuits get messed up by terminal line wrap horizontally)
printNetV :: Net FanPos -> IO ()
printNetV = printLines . reverse . map (map swapChar) . transpose . drawNet2
  where swapChar '-' = '|'
        swapChar '|' = '-'
        swapChar  x  = x

--

fanPos :: Net FanPos -> [FanPos]
fanPos c = bottomUp $ c $- dispFan $ dispZero $ width c

updateFans :: ([(Int, [Int])] -> [(Int, [Int])]) -> FanPos -> FanPos
updateFans f fp = fp { fans = f $ fans fp }

dispFan :: [FanPos] -> [FanPos]
dispFan   [w]                         = [w]
dispFan wires @ (FanPos i _ fs : fps) = fp' : fps'
  where d = maximum $ map curDepth $ wires
        f = map wireNum fps
        fp'  = FanPos i (d+1) ((d, f) : fs)
        fps' = [fp_ { curDepth = d + 1 } | fp_ <- fps]

dispZero :: Int -> [FanPos]
dispZero n = [FanPos i 0 [] | i <- [0 .. n-1]]

bottomUp :: [FanPos] -> [FanPos]
bottomUp = map $ updateFans reverse

replicate' :: Int -> [a] -> [a]
replicate' = (concat .) . replicate

{- Gather fans in lines a fan is given by (firstWire, [remaining])
  
   Split overlapping fans over several lines

   Greedy algorithm:
     Take the first fan that fits,
     repeat until end of line (adding fans to one line),
     repeat until no more fans (creating new lines).

   Is that optimal (in the number of lines)?
 -}
layout :: [FanPos] -> [[[(Int, [Int])]]]
layout fps = layout' 0 fps [] []
  where layout' _  []      [] lines = reverse                     lines
        layout' _  [] curLine lines = reverse $ reverse curLine : lines
        layout' d fps curLine lines =
          if null lo
            then layout' (d+1) fps'             [] $ reverse curLine : lines
            else layout'     d fps' (lo : curLine)                     lines
          where (lo, fps') = scan d 0 fps
        -- Take a list of fans @lo@ at depth d from @fps@, leaving @fps'@,
        -- s.t. the fans in @lo@ don't overlap
        scan _ _   [] = ([], []) -- @ (lo, fps') @
        scan d i _fps @ (fp : fps)
          | null (fans fp) = scan d i fps
          | d == d' && i <= j = consPair ((j, f), updateFans tail fp)
                                    $ scan d (last f + 1) fps
          |      otherwise = consSnd fp $ scan d i fps
          where FanPos j _ ((d', f) : _) = fp
                consPair (x, y) (xs, ys) = (x : xs, y : ys)
                consSnd      y  (xs, ys) = (    xs, y : ys)

drawLine :: Int -> [(Int, [Int])] -> String
drawLine n = tail . drawLine' 0
  where drawLine' i            [] = replicate' (n-i) " |"
        drawLine' i ((j, f) : fs) = replicate' (j-i) " |"
                                 ++ " +"
                                 ++ drawFan (j+1) f
                                 ++ drawLine' (last f + 1) fs
        drawFan i       [] = ""
        drawFan i (j : js) = replicate' (j-i) "-|"
                          ++ "-o"
                          ++ drawFan (j+1) js

drawLine2 :: Int -> [(Int, [Int])] -> String
drawLine2 n = drawLine' 0
  where drawLine' i            [] = replicate (n-i) '|'
        drawLine' i ((j, f) : fs) = replicate (j-i) '|'
                                 ++ "+"
                                 ++ drawFan (j+1) f
                                 ++ drawLine' (last f + 1) fs
        drawFan i       [] = ""
        drawFan i (j : js) = replicate (j-i) '-'
                          ++ "o"
                          ++ drawFan (j+1) js

-- The @Int@ type parameter is the width of the circuit
drawWith :: (Int -> [(Int, [Int])] -> String)
                              {- ^ draw a line of fans -}
         -> (Int -> String)   {- ^ empty line -}
         -> (Int -> [String]) {- ^ header (e.g. with wire indices) -}
         -> Net FanPos -> [String]
drawWith drawLine _space header c = header n ++ [space] ++ lines
  where lo    = layout $ fanPos c
        lines = (++ [space]) . map (drawLine n) =<< lo
        space = _space n
        n     = width c

--

drawNet :: Net FanPos -> [String]
drawNet = drawWith $@ drawLine
                   $@ intersperse ' ' . flip replicate '|'
                   $@ map (intersperse ' ') . indices

drawNet2 :: Net FanPos -> [String]
drawNet2 = drawWith $@ drawLine2
                    $@ flip replicate '|'
                    $@ indices

indices n = indices' n [] $ map (intToDigit . (`mod` 10)) [0 ..]
  where indices' 0 acc _ = acc
        indices' m acc l = indices' $@ m `div` 10
                                    $@ take n l : acc
                                    $@ concat (map (replicate 10) l)

printLines :: [String] -> IO ()
printLines = foldr $@ (>>) . putStrLn
                   $@ return ()

-- Tikz rendering

unit = 1 :: Double
radius = 0.1 :: Double

foldTikz :: [TikZ] -> TikZ
foldTikz = foldl' (->>) emptytikz

tikzOfNet :: Net FanPos -> TikZ
tikzOfNet c = (->>) wires . foldTikz . zipWith tikzOfFans [unit ..]
            $ reverse levels
  where levels = concat . layout . fanPos $ c
        wires = foldTikz [draw
                             $ Start (pointAtXY x 0)
                        `Line` pointAtXY x d | x <- [0 .. fromIntegral n - 1]]
        n = width c
        d = fromIntegral (length levels)

tikzOfFans :: Double -> [(Int, [Int])] -> TikZ
tikzOfFans yPos fs = foldTikz $ map (tikzOfFan yPos) fs

-- f is not empty
tikzOfFan :: Double -> (Int, [Int]) -> TikZ
tikzOfFan yPos (i, f) = foldTikz
                   $ (map draw $ start : middle ++ end) ++ map fill circles
  where start = Start (pointAtXY (fromIntegral i) yPos) `Line` secondPt
        middle | i + 1 == last f = []
               | otherwise       = [middle']
        middle' = Start secondPt
           `Line` pointAtXY (fromIntegral (last f) - unit / 2) yPos'
        (end, circles) = unzip $ map end' f
        end' j = (Start (pointAtXY (fromIntegral j - unit / 2) yPos') `Line` p
               , Start p `Circle` radius)
          where p = pointAtXY (fromIntegral j) yPos''
        secondPt = pointAtXY (fromIntegral i + unit / 2) yPos'
        yPos'  = yPos - unit / 2
        yPos'' = yPos - unit

renderNetFile :: FilePath
              -> Double   {- ^ scale factor -}
              -> Net FanPos
              -> IO ()
renderNetFile f s = renderFile f . tp' . scope [TScale s] . tikzOfNet
  where tp' :: TikZ -> LaTeX
        tp' = tikzpicture
