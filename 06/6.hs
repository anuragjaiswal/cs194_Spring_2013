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

--Ex03
data Stream a = SCons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (SCons x xs) = x:(streamToList xs)

instance Show a => Show (Stream a) where
show x = Prelude.show (take 20 (streamToList x))

--Ex04
streamRepeat :: a -> Stream a
streamRepeat x = SCons x (streamRepeat x)

--streamMap :: (a -> b) -> Stream a -> Stream b
--streamMap f (SCons x xs) = map f x