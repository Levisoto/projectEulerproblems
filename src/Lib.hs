module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--Problem 1 "Multiples of 3 and 5"
m3and5 :: Integer -> Integer
m3and5 num = sum [x | x <- [1..num-1], mod x 3 == 0 || mod x 5 == 0]
---------------------------------------------------------------------
---------------------------------------------------------------------

--'******************************************************************
--Problem 2 "sum of even-values from foundfibo's serie "
foundfibo :: Integer -> Integer
foundfibo m = sum $ filter (even) $ filter (<4000000) $ fibo2 m
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
---------------------------------------------------------------------
---------------------------------------------------------------------

--'******************************************************************
--Problem 3 "Finding the maximum a number's factor prime"
factorPrime:: Integer -> [Integer]
factorPrime m= take 2 [x | x <- consPrime xnum, mod m x == 0]
--factorPrime m= take 2 [x | x <- factors, isPrime $ ceiling ((/)  (fromIntegral m) (fromIntegral x))]
  where
    xnum = ceiling $ logBase 2 $ sqrt $ fromIntegral m
    --ynum = (ceiling.fromIntegral) m/x

maximumPrime :: Integer -> Integer
maximumPrime m = maximum $ factors m

factors :: Integer -> [Integer]
factors m
  | isPrime m = [m]
  | otherwise = factorPrime m ++ factorPrime (ceiling $ foldl (/) (fromIntegral m) (map fromIntegral (factorPrime m)))

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = if (elem False $ map ((/=0).(mod  n)) posibility) then False else True
--isPrime n = if (elem False $ map ((/=0).( `mod` n)) posibility n) then False else True
  where
    numfilter = (ceiling.sqrt.fromIntegral) n
    posibility = filter (<numfilter + 1) $ consPrime $ (ceiling.(logBase 2).sqrt.fromIntegral) n

consPrime :: Int -> [Integer]
consPrime 1 = [2]
consPrime 2 = [2,3]
--consPrime m = cons ++ (take totaking $ verify (cons) (filter odd [last cons..]))
consPrime m = cons ++ (take totaking (getfilter cons [last cons..]))
  where
    getfilter prime longlist= foldl (\x y -> filter ((/=0).(`mod` y)) x) longlist prime
    cons = consPrime (m - 1)
    totaking = 2 ^ m - 1
    --reducelist list = getfilter (filter (<(round.sqrt) (cons !! 0)) list) list
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

--'******************************************************************
  --Problem 4 "Find the largest palindrome made from the product of 3 num"
findLargestPalindrome:: Integer
findLargestPalindrome = maximum $ take 20 $ [x*y | x <- m, y <- m, f (x*y) == True]
    where f = (generateNum.toList)
          m = [999,998..1]

generateNum :: [Integer] -> Bool
generateNum [] = True
generateNum (_:[]) = True
generateNum num1 
  | (last num1)==(head num1) = True && (generateNum (tail $ init num1))
  | otherwise = False

toList :: Integer -> [Integer]
toList num 
  | (div num 10) == 0 = [mod num 10] 
  | otherwise = (mod num 10):(toList (div num 10))
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

--'******************************************************************
--Problem 5 "Find the smallest number what It will be divisible by 20 less numbers"
findLessD20 :: Integer
findLessD20 = foldl (\x y -> ((x*y) `quot` (mcd x y))) 1 [2..20]
  where mcd m 0 = m
        mcd m n = mcd n (mod m n)
