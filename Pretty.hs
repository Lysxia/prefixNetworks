module Pretty (
    Net

  -- * \"Pretty\" printing
  , FanPos
  , printNet
  , printNetV
  ) where

import WSO
import Data.List
import Data.Char

-- * \"Pretty\" printing

-- | The type used to collect fan positions
data FanPos = FanPos
  { wireNum  :: Int            -- ^ Wire identifier
  , curDepth :: Int            -- ^ Current depth of the wire
  , fans     :: [(Int, [Int])] -- ^ Fans and their depth
  }

-- | Print an ASCII drawing of the network
printNet :: Net FanPos -> IO ()
printNet = printLines . draw

-- | Print circuit vertically
--   (wide circuits get messed up by terminal line wrap horizontally)
printNetV :: Net FanPos -> IO ()
printNetV = printLines . reverse . map (map swapChar) . transpose . draw2
  where swapChar '-' = '|'
        swapChar '|' = '-'
        swapChar  x  = x

--

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
  where layout' _  [] curLine lines = reverse
                                    $ if null curLine then lines
                                                      else curLine : lines
        layout' d fps curLine lines =
          if null lo
            then layout' (d+1) fps'             [] $ reverse curLine : lines
            else layout'     d fps' (lo : curLine)                     lines
          where (lo, fps') = scan d 0 fps
        -- Take a list of fans @lo@ at depth d from @fps@, leaving @fps'@,
        -- s.t. the fans in @lo@ don't overlap
        scan _ _   [] = ([], []) -- @ (lo, fps') @
        scan d i _fps @ (fp : fps)
          | null (fans fp) = scan d (i+1) fps
          |        d == d' = consPair ((j, f), updateFans tail fp)
                                    $ scan d (last f + 1) fps
          |      otherwise = consSnd fp $ scan d (i+1) fps
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
  where lo    = layout $ bottomUp $ c $- dispFan $ dispZero n
        lines = (++ [space]) . map (drawLine n) =<< lo
        space = _space n
        n     = width c

--

draw :: Net FanPos -> [String]
draw = drawWith $@ drawLine
                $@ intersperse ' ' . flip replicate '|'
                $@ map (intersperse ' ') . indices

draw2 :: Net FanPos -> [String]
draw2 = drawWith $@ drawLine2
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

