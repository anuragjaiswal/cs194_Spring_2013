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
--evalStr s 
--	| r == Nothing = Nothing
--	| r == Just x = eval x
--	where r = parseExp Lit Add Mul s

evalStr s = let r = parseExp Lit Add Mul s in
	case r of Nothing -> Nothing
		  Just n -> Just (eval n)

--evalStr s = case (f s) of
--         Just n -> n
--         Nothing -> Nothing
--		where f = parseExp Lit Add Mul

--evalStr "2+3*4"
--Just 14
-- evalStr "2+3*"
--Nothing
--evalStr "(2+3)*4"
--Just 20


-- Ex3


