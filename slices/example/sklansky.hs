{- Draw an example network in tikz -}

module Main where

import WSO
import Pretty

main = renderNetFile "sklansky.tex" scale net
  where scale = 0.35
        net = sklansky 32
