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

--Ex06.show (streamRepeat 1)
--"[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]"

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (SCons x xs) = SCons(f x) (streamMap f xs)

-- Ex06.show (streamMap (+1) (streamRepeat 1))
--"[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2]"

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed r a = SCons a (streamFromSeed r (r a))

--Ex06.show (streamFromSeed (+1) 1)
--"[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]"
--Ex06.show (streamFromSeed (+2) 1)
--"[1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39]"
--Ex06.show (streamFromSeed (+3) 3)
--"[3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60]"

--Ex05
nats :: Stream Integer
nats = streamFromSeed (+1) 0

--Ex06.show nats
--"[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]"

--ruler :: Stream Integer
--ruler = 



















