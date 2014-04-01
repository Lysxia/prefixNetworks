{-# LANGUAGE Rank2Types #-}
module Main where

import WSO
import Pretty

main = do
  let ser = serial n
  putStrLn "Net:"
  netInfo "Serial" ser

  putStrLn "Check:"
  printCheck ser

  newline

  netInfo "Sklansky" $ sklansky n
  netInfo "Sklansky'" $ sklansky' n
  netInfo "slices(2)" $ slices2 m

  netInfo "slice(2)" $ slice2 k
  putStrLn "Much wider for the same depth:"
  netInfo "slice00" $ slice00 (k+1) k
  putStrLn "Even more:"
  printNetV $ slice00 (k+2) (k-1)

  where n = 7
        m = 9
        k = 5

newline :: IO ()
newline = putStrLn ""

netInfo :: String -> (forall a. Net a) -> IO ()
netInfo title c = do
  putStrLn title
  putStrLn $ "width " ++ show (width c)
  putStrLn $ "depth " ++ show (depth c)
  putStrLn $ "size  " ++ show ( size c)
  printNet c
  newline

