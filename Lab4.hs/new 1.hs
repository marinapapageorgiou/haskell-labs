insertAt :: u -> Int -> [u] -> [u]		--correct
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as