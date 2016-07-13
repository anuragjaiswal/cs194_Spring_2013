toDigits :: Integer -> [Integer]
toDigits n
		| n < 1 = []
        | n `quot` 10 == 0 = [n]
		| otherwise = toDigits(n `quot` 10) ++ [n `rem` 10]
		
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
		| n < 1 = []
        | n `quot` 10 == 0 = [n]
		| otherwise = [n `rem` 10] ++ toDigitsRev(n `quot` 10)
		
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs) = x : (2*y) : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits x = sum (map sum (map toDigits x))

validate :: Integer -> Bool
validate n 
	| (sumDigits . doubleEveryOther . toDigitsRev $ n) `mod` 10 == 0 = True
	| otherwise = False

--1. move n − 1 discs from a to c using b as temporary storage
--2. move the top disc from a to b
--3. move n − 1 discs from c to b using a as temporary storage.
	
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n from to tmp
	| n < 1 = []
	| n == 1 = [(from, to)]
	| otherwise = hanoi (n-1) from tmp to ++ hanoi 1 from to tmp ++ hanoi (n-1) tmp to from
