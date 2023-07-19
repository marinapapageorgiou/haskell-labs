statistics :: [(Int,Int)]->(Int,Int,Int,Int,Int)
statistics [] = (0,0,0,0,0)
statistics [(x,y)] = (1, if y < x then 3 else if y == x then 1 else 0 , x , y , x-y)
statistics ((x,y):t) = (1 + a ,(if y < x then 3 else if y == x then 1 else 0) + b , x + c , y + d ,e + check (tail([(x,y)])))
	where (a,b,c,d,e) = statistics t
	

check :: [(Int,Int)]->Int
check [] = 0
check ((x,y):t) = if check ((x,y):t) < (x-y) then (x-y) else check ((x,y):t)

