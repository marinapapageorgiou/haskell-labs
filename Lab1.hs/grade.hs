grade :: Int->Int->Int
grade a b
	| a< 0 || a > 100
		= -1
	| b < 0 || b > 20
		= -1
	| c > 47 && a <= 47
		=47
	| c > 47 && c < 50 && a > 47
		=50
	| otherwise
		=c
	where c= (8*a `div` 10) + b