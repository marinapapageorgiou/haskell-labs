digits :: Int->Int->Int
digits 0 0 = 0
digits a b 
	| c == 8 = 1000000
	| c == 7 = 100000
	| c == 6 = 8000
	| c == 5 = 300
	| c == 4 = 20
	| c == 3 = 5 
	| c == 2 = 1
	| c == 1 = 0
	| c == 0 = 0
	where c = getprize a b 
 
 
getprize :: Int->Int->Int
getprize 0 0 = 0
getprize a b = getprize (a ` div` 10) (b `div` 10) + if (a `mod` 10) == (b `mod` 10) then 1 else 0 
	
	
