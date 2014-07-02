module Tikz where

import Text.Printf

type Tikz = String
type Scope = String
type Point = (Double, Double)
type Angle = Double

tikzEnvironment :: Tikz -> Tikz
tikzEnvironment = printf "\\begin{tikzpicture}\n%s\\end{tikzpicture}\n"

tikzScope :: Scope -> Tikz -> Tikz
tikzScope = printf "\\begin{scope}[%s]\n%s\\end{scope}\n"

-- | Precision 10^-2
tikzDefineDouble :: String -> Double -> String
tikzDefineDouble = printf "\\def\\%s{%0.3f}\n"

tikzDefine :: String -> String -> String
tikzDefine = printf "\\def\\%s{\n%s\n}\n"

tikzCircle :: Double -> Double -> String
tikzCircle i j = printf "\\fill (%0.3f, %0.3f) circle (\\r);" i j

tikzCurve :: Angle -> Angle -> Point -> Point -> String
tikzCurve a b (x,y) (x',y')
  = printf "\\draw (%0.3f,%0.3f) to[out=%0.3f,in=%0.3f] (%0.3f,%0.3f);"
      x y a b x' y'
