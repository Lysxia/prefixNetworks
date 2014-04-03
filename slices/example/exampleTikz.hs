{- Draw an example network in tikz -}

module Main where

import WSO
import Pretty

main = renderNetFile "exampleTikz.tex" scale net
  where scale = 0.5
        net = slices 3 6
