sum2021 :: Integer->Integer->Integer    
sum2021 0 0 = 0
sum2021 m n
	| 0 < m && m <= n && i <= n
		= sinartisi m n i
 	
	| otherwise 
		= 0
		
	where i = m
		
		

sinartisi :: Integer->Integer->Integer->Integer
sinartisi  0 0 0 = 0
sinartisi m n i
	| 0 < m && m <= n &&  i <= n
		= sinartisi m n (i+1) + (n + i) ^ m

	| otherwise
		= 0
		