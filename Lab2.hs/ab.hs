
ab :: Int->(Int,Int)
ab 0 = (0,0)
ab n 
	| 0 <= a && a <= b && b <= n && count c <= n && n `mod` select(a,b) == 0  
		= (a,b)
	
	where 
		n = b * a
		b = count c
		a = n `div` b   
		c = 1 
	
count :: Int->Int
count 0 = 0
count c 
	| count c <=  n && count c `mod` n == 0
		= count c
	| otherwise	
		= count (c+1)
		
	where 
		n = b * a
		b = count c
		a = n `div` b

select :: (Int,Int)->Int
select (a,b)
	| count c `mod` b == 0 && count c `mod` a == 0 && count c <= n
		= count c
		
	| otherwise 
		= count (c + 1) 
	
	where 
		n = b * a
		b = count c
		a = n `div` b
		c = 1 
