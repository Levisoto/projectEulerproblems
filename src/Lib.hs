module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--Problem 1 "Multiples of 3 and 5"
m3and5 :: Integer -> Integer
m3and5 num = sum [x | x <- [1..num-1], mod x 3 == 0 || mod x 5 == 0]

--Problem 2 "sum of even-values from foundfibo's serie "
foundfibo :: Integer
foundfibo = sum $ filter (even) $ filter (<4000000) $ fibo2 100
  --where
    --fibo 1 = 1
    --fibo 2 = 2
    --fibo n = fibo (n - 2) + fibo (n - 1)
--The last term of foundfibo is the 32th element == 3524578

fibo2 :: Integer -> [Integer]
fibo2 0 = [0]
fibo2 1 = [1]
fibo2 2 = [1,1]
fibo2 n
  | n > 2 = addnext $ fibo2 (n-1)
    where
      addnext list = list++[(list !! ((length list)-2))+(list !! ((length list)-1))]
