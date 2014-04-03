{- Draw an example network in tikz -}

module Main where

import WSO
import Pretty

main = renderNetFile "fan.tex" scale net
  where scale = 1
        net = Net 5 $ id
