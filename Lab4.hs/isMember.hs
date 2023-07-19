move :: Eq u => [u]->u->Int->[u]
move [] x n = []
move (h:t) x n 
	| 0 < n 
		= right (h:t) x n 				-- right dose not work okey
	| n < 0
		= left (h:t) x n 				-- left dose not work okey
	| otherwise
		-- = deleteInt x (h:t)
		= func x (h:t)


deleteInt :: Eq u=>u->[u]->[u]			 --correct -- delete the element x
deleteInt x (h:t)	
	| x ==h 
		= t
	| otherwise
		= h : deleteInt x t
		
deleteInt x [] = []


right :: Eq u => [u]->u->Int->[u]
right [] x n = []
right (h:t) x n 
	| dataAt x (h:t) <= length (h:t)
		= insertAt x ((dataAt x (h:t))+n) (deleteInt x (h:t))
		-- = deleteInt x (h:t) ++ insertAt x (dataAt x (h:t))+n (h:t)
		-- = h:dataAt x (h:t)
		-- = deleteInt x (h:t):x ++ ...
	| otherwise
		-- = deleteInt x (h:t):x
		= func x (h:t)

left :: Eq u => [u]->u->Int->[u]
left [] x n = []
left (h:t) x n 
	| dataAt x (h:t) <= length (h:t)
		= insertAt x ((dataAt x (h:t))-n) (deleteInt x (h:t))
		-- = deleteInt x (h:t):x ++ ...
	| otherwise
		-- = deleteInt x ((h:t):x)
		-- = h: deleteInt x (t:x)
		= func x (h:t)


dataAt :: Int -> [u] -> u				 --correct -- index of an element
dataAt y (x:xs)  
	| y <= 0 
		= x
    | otherwise 
		= dataAt (y-1) xs


insertAt :: u -> Int -> [u] -> [u]		--correct
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as


func a [] = [a]							--correct -- add element end of list
func a (x:xs) = x : func a xs



