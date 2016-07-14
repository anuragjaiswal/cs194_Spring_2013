module Ex05 where
import ExprT
import Parser

--Ex 1
eval :: ExprT -> Integer
eval (Lit a) = a
eval (Add l r) = (eval l) + (eval r)
eval (Mul l r) = (eval l) * (eval r)

--(Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20

--Ex2
evalStr :: String -> Maybe Integer
evalStr s = let r = parseExp Lit Add Mul s in
	case r of Nothing -> Nothing
		  Just n -> Just (eval n)
		  
--evalStr "2+3*4"
--Just 14
-- evalStr "2+3*"
--Nothing
--evalStr "(2+3)*4"
--Just 20


-- Ex3
class Expr a where
	lit :: Integer -> a
	add :: a -> a -> a
	mul :: a -> a -> a
	
--instance Expr ExprT where
--    lit x = Lit x
--	add a b = Add a b
--	mul a b = Mul a b
    

--Ex4

instance Expr Integer where
	lit x = x
	add x y = x + y
	mul x y = x * y
	
instance Expr Bool where
	lit x = if (x <= 0) then False else True
	add x y =  x || y
	mul x y =  x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
	lit x = MinMax x
	add (MinMax x) (MinMax y) = MinMax (max x y)
	mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
	lit x = Mod7 (x `mod` 7)
	add (Mod7 x) (Mod7 y) = Mod7 (x+y)
	mul (Mod7 x) (Mod7 y) = Mod7 (x*y)

