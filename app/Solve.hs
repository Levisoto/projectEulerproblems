module Main where

import Lib

main :: IO ()
main = do
  putStrLn "Prob 1"
  print $ m3and5 1000
  putStrLn "==================\n"
  putStrLn "Prob 2"
  print $ foundfibo 400 
  putStrLn "==================\n"
  putStrLn "Prob 3"
  print $ maximumPrime 600851475143
  putStrLn "==================\n"
  putStrLn "Prob 4"
  print $ findLargestPalindrome
  putStrLn "==================\n"

