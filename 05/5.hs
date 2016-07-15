{-# LANGUAGE FlexibleInstances #-}
module Ex05 where
import qualified Data.Map as M
import ExprT
import Parser

--Ex 1
eval :: ExprT -> Integer
eval (ExprT.Lit a) = a
eval (ExprT.Add l r) = (eval l) + (eval r)
eval (ExprT.Mul l r) = (eval l) * (eval r)

--(Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20

--Ex2
evalStr :: String -> Maybe Integer
evalStr s = let r = parseExp ExprT.Lit ExprT.Add ExprT.Mul s in
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

-- Ex6
class HasVars a where
	var :: String -> a

data VarExprT = Var String 
	   | Lit Integer
	   | Add VarExprT VarExprT
	   | Mul VarExprT VarExprT
	deriving (Show, Eq)
	
instance Expr VarExprT where
	lit x = Ex05.Lit x
	add x y = Ex05.Add x y
	mul x y = Ex05.Mul x y

instance HasVars VarExprT where
	var str = Var str     
 
instance HasVars (M.Map String Integer -> Maybe Integer) where
	var s = (\m -> M.lookup s m)
	
instance Expr  (M.Map String Integer -> Maybe Integer) where
	lit x = (\_ -> Just x)

--ithVars :: [(String, Integer)]
--			-> (M.Map String Integer -> Maybe Integer)
--			-> Maybe Integer
--			
--
--instance HasVars VarExprT where
--	var x = 
--	
--instance Expr VarExprT where
--	lit x = x
--	add x y = x + y
--	mul x y = x * y