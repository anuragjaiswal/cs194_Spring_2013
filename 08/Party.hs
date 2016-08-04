module Party where
import Employee

--Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons (Emp en ef) (GL [x] f) = (GL ((Emp en ef):[x]) (f + ef))

instance Monoid GuestList where  
    mempty = (GL [] 0)
    (GL x xf) `mappend` (GL y yf) = (GL (x ++ y) (xf + yf))
	
moreFun :: GuestList -> GuestList -> GuestList
moreFun (GL x xf) (GL y yf) = if(xf < yf) then (GL y yf) else (GL x xf)

--Exercise 2
--treeFold :: (a -> b) -> Tree a -> b
--treeFold