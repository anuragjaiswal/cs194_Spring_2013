module Ex06 where

--Ex01
--F0 = 0
--F1 = 1
--Fn = Fn-1 + Fn-2 (n = 2)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [1,2..]

--Ex02
fibs2 :: [Integer]
fibs2 = take 20(1:1:zipWith(+) fibs2 (tail fibs2))