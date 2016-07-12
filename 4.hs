module Ex4 where

--fun1 :: [Integer] -> Integer
--fun1 [] = 1
--fun1 (x:xs)
--	| even x = (x - 2) * fun1 xs
--	| otherwise = fun1 xs

fun11 :: [Integer] -> Integer
fun11 = product . map (\x -> x -2) . filter (even)

--fun2 :: Integer -> Integer
--fun2 1 = 0
--fun2 n | even n = n + fun2 (n ‘div‘ 2)
--		| otherwise = fun2 (3 * n + 1)
--


xor :: [Bool] -> Bool
xor xs = foldl (\acc x -> if x then (not acc) else acc) False xs


mapOne :: (a -> b) -> [a] -> [b]
mapOne f = foldr (\x acc -> (f x):acc) []


--sieveSundaram :: Integer -> [Integer]

notPrimes n = filter (<= n) [i+j+2*i*j | i <- [1..n], j <- [1..n]]

sieveSundaram n = 2:[2*x + 1 | x <- [1..n], not(x `elem` (notPrimes n)) ]