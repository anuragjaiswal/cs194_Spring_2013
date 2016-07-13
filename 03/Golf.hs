module Golf where
import Data.Map
 

makeMap x = fromList (zip [1,2..] x)

filteredMap x i _ = filterWithKey (\k _ -> k `mod` i == 0) (makeMap x)

skips x = Prelude.map (Prelude.map snd) (Prelude.map (toList.snd) (toList (mapWithKey (filteredMap x) (makeMap x))))

window len w = 
	(case w of
		[] -> []
		x:xs -> 
			if length w >= len then
				(take len w): window len xs
			else window len xs)

maximaFilter (x:y:z:[]) = (y>x && y>z)

selector (x:y:z:[]) = y

localMaxima x = Prelude.map selector (Prelude.filter maximaFilter (window 3 x))
--Version 2:
localMaximaOne w =  
	(case w of
		x:y:z:[] -> 
			if (y>x && y>z) then
				y:[]
			else []
		x:y:z:ls -> 
			if (y>x && y>z) then
				y:localMaximaOne(y:z:ls)
			else localMaximaOne(y:z:ls))
			
			
yaxis x = reverse [1,2..(maximum x)]

plot i x = Prelude.foldl (\acc k -> if k>=i then acc ++ "*" else acc ++ " ") "\n"x

countOcc x o = Prelude.foldl (\acc x -> if x == o then acc + 1 else acc) 0 x

histData x = Prelude.map (countOcc x) [0,1..9]

histogram x = (Prelude.foldl (\acc k -> acc ++ (plot k datax)) "" (yaxis datax)) ++ "\n==========\n0123456789\n" 
	where datax = histData x

--"*Golf> putStr (histogram [1,2,3,4,4,5,1,2,5,6,7])"
--
-- ** **
-- *******
--"=========="
--0123456789