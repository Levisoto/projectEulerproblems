module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--Problem 1 "Multiples of 3 and 5"
m3and5 :: Integer -> Integer
m3and5 num = sum [x | x <- [1..num-1], mod x 3 == 0 || mod x 5 == 0]

--Problem 2 "sum of even-values from fibonacci's serie "
fibonacci :: Integer -> Integer
fibonacci num = sum [x | x <- (map fibo [1..num]), mod x 2 == 0]
  where
    fibo 1 = 1
    fibo 2 = 2
    fibo n = fibo (n - 2) + fibo (n - 1)
--The last term of fibonacci is the 32th element == 3524578

